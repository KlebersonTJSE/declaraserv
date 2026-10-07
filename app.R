# =========================================================================
# app.R — Declaraserv | Geração de Certidão MPRO (Formato HTML)
# -------------------------------------------------------------------------
# Login corporativo (Active Directory), código Authenticator (TOTP) ou
# Login de Administrador (senha mestra), seguindo o mesmo layout/fluxo de
# autenticação usado em outros sistemas internos, adaptado para a
# aplicação Declaraserv.
#
# Ordem de carregamento (importante):
#   1. .Renviron  ->  2. JAVA_HOME  ->  3. pacotes  ->  4. Python
#   5. R/*.R (source explícito; o autoload do Shiny está desligado por
#      R/_disable_autoload.R — ver comentário naquele arquivo).
#
# Fonte de dados das declarações (R/fonte_dados.R): IRIS de produção ou,
# quando ele não está acessível, dados FICTÍCIOS de exemplo da tabela
# exemplo_relatorios do SQLite (data/declaraserv.db). Com dados de
# exemplo, uma faixa "DADOS DE EXEMPLO" fica fixa na tela e os documentos
# gerados recebem a marca d'água "EXEMPLO". Ver DECLARASERV_FONTE_DADOS.
# =========================================================================

# =========================================================================
# DIRETÓRIO DO APP
# -------------------------------------------------------------------------
# Resolvido pelo `here` (procura .Rproj/.here/app.R a partir do diretório
# atual). Antes, o app fazia setwd() com base na ABA ATIVA do editor do
# RStudio — se a aba ativa fosse, por exemplo, R/auth.R, o APP_DIR
# apontava para a pasta errada.
# =========================================================================
# Fallback só para quando o app.R é executado via "Source" no RStudio
# fora do projeto (.Rproj) e o diretório atual não é a pasta do app.
# Pelo botão "Run App" / shiny::runApp() o Shiny já entra na pasta certa.
if (!file.exists("app.R") &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
    caminho_editor <- rstudioapi::getSourceEditorContext()$path
    if (identical(basename(caminho_editor), "app.R")) {
        setwd(dirname(caminho_editor))
    }
    rm(caminho_editor)
}

library(here)
here::i_am("app.R")

APP_DIR    <- normalizePath(here::here(), winslash = "/", mustWork = TRUE)
ASSETS_DIR <- file.path(APP_DIR, "assets")

# =========================================================================
# .Renviron — carregamento explícito (uma única vez)
# -------------------------------------------------------------------------
# Feito antes de QUALQUER library() que dependa de variáveis de ambiente
# (JAVA_HOME para o rJava, RETICULATE_PYTHON para o reticulate).
# =========================================================================
RENVIRON_PATH <- file.path(APP_DIR, ".Renviron")

if (file.exists(RENVIRON_PATH)) {
    readRenviron(RENVIRON_PATH)
} else {
    warning(
        "Arquivo .Renviron não encontrado em: ", RENVIRON_PATH,
        ". As credenciais de banco/AD precisam estar definidas de outra ",
        "forma no ambiente (ex.: variáveis de ambiente do serviço)."
    )
}

# Lê uma variável de ambiente obrigatória; interrompe a execução com
# mensagem clara caso ela não esteja definida no .Renviron.
obter_env_obrigatoria <- function(nome_var) {
    valor <- Sys.getenv(nome_var, unset = "")

    if (valor == "") {
        stop(
            "Variável de ambiente obrigatória não definida: ", nome_var,
            ". Defina-a no arquivo .Renviron (veja .Renviron.example)."
        )
    }

    valor
}

# =========================================================================
# JAVA_HOME
# -------------------------------------------------------------------------
# O rJava lê JAVA_HOME (e, no Windows, ajusta o PATH para
# encontrar jvm.dll) já na inicialização do pacote — se JAVA_HOME só for
# definido depois de library(rJava), o valor configurado no .Renviron
# pode ser ignorado silenciosamente, e o app passa a depender de um
# JAVA_HOME "de sistema" que pode não ser o correto para produção.
# =========================================================================
# Não é mais obrigatório: sem Java/IRIS o app sobe e usa os dados de
# EXEMPLO do SQLite (ver R/fonte_dados.R).
JAVA_HOME_PATH <- Sys.getenv("JAVA_HOME", unset = "")

if (nzchar(JAVA_HOME_PATH)) {
    Sys.setenv(JAVA_HOME = JAVA_HOME_PATH)
} else {
    warning(
        "JAVA_HOME não definido no .Renviron. Sem Java não há conexão com ",
        "o IRIS — as declarações usarão os dados de EXEMPLO."
    )
}

library(shiny)
library(rmarkdown)
library(glue)
library(DT)
library(bslib)
library(digest)
library(DBI)
library(RSQLite)
library(reticulate)

# jsonlite também exporta validate(), que mascararia shiny::validate()
# (era a origem do aviso "O seguinte objeto é mascarado...: validate").
library(jsonlite, exclude = "validate")

# rJava/RJDBC (IRIS): se não carregarem (Java ausente ou JAVA_HOME
# errado), o app continua — só o IRIS fica indisponível.
IRIS_PACOTES_OK <- tryCatch({
    suppressPackageStartupMessages({
        library(rJava)
        library(RJDBC)
    })
    TRUE
}, error = function(e) {
    warning(
        "Não foi possível carregar rJava/RJDBC (", conditionMessage(e), "). ",
        "As declarações usarão os dados de EXEMPLO do SQLite."
    )
    FALSE
})

# =========================================================================
# PYTHON (reticulate) — configurado ANTES de carregar R/auth.R
# -------------------------------------------------------------------------
# Ver R/python_env.R. Se o 'ldap3' estiver ausente, o app sobe mesmo
# assim: o login AD mostra a mensagem com o comando de instalação e o
# login por Authenticator (TOTP) continua funcionando.
# =========================================================================
source(file.path(APP_DIR, "R", "python_env.R"), encoding = "UTF-8")
configurar_python()

# =========================================================================
# MÓDULOS DE AUTENTICAÇÃO, BANCO E UTILITÁRIOS
# =========================================================================
for (arquivo_r in c(
    file.path(APP_DIR, "R", "utils.R"),
    file.path(APP_DIR, "R", "auth.R"),
    file.path(APP_DIR, "R", "auth_totp.R"),
    file.path(APP_DIR, "R", "database.R"),
    file.path(APP_DIR, "R", "fonte_dados.R"),
    file.path(APP_DIR, "modules", "mod_totp_admin.R")
)) {
    source(arquivo_r, encoding = "UTF-8")
}
rm(arquivo_r)

# =========================================================================
# EMPRESAS (MULTI-DISTRO)
# -------------------------------------------------------------------------
# Lista de empresas/unidades configuradas em DISTRO_1, DISTRO_2... no
# .Renviron (ver listar_distros() em R/utils.R). Cada uma tem sua própria
# configuração de LDAP ({DISTRO}_LDAP_*) e de banco ({DISTRO}_IRIS_*).
# Adicionar uma nova empresa não exige alterar este arquivo nem
# R/auth.R/R/database.R — só DISTRO_N + as variáveis correspondentes no
# .Renviron.
# =========================================================================
DISTROS_DISPONIVEIS <- listar_distros()

if (length(DISTROS_DISPONIVEIS) == 0) {
    warning(
        "Nenhuma empresa configurada em DISTRO_1, DISTRO_2... no ",
        ".Renviron. O login corporativo (AD) ficará sem opções até isso ",
        "ser configurado."
    )
}

# Avisa (sem interromper) sobre configuração incompleta de cada empresa —
# o app ainda pode ser usado normalmente para as empresas que estiverem
# corretamente configuradas.
for (distro_check in DISTROS_DISPONIVEIS) {

    variaveis_esperadas <- c(
        distro_env(distro_check, "LDAP_SERVER"),
        distro_env(distro_check, "LDAP_PORT"),
        distro_env(distro_check, "LDAP_DOMAIN"),
        distro_env(distro_check, "LDAP_SEARCH_BASE"),
        distro_env(distro_check, "IRIS_URL"),
        distro_env(distro_check, "IRIS_USER"),
        distro_env(distro_check, "IRIS_PASSWORD")
    )

    faltando <- variaveis_esperadas[
        Sys.getenv(variaveis_esperadas, unset = "") == ""
    ]

    if (length(faltando) > 0) {
        warning(
            "Configuração incompleta para a empresa '", distro_check,
            "' no .Renviron: ", paste(faltando, collapse = ", "),
            ". O login corporativo (AD) e/ou a consulta ao banco dessa ",
            "empresa falharão até isso ser definido."
        )
    }
}
rm(distro_check)

# =========================================================================
# TIPOS DE CERTIDÃO
# =========================================================================

# =========================================================================
# RMD_DIR — SEMPRE resolvido via `here`, sem caminho fixo/variável de
# ambiente de espécie alguma.
# -------------------------------------------------------------------------
# Com o suporte multi-empresa, os templates deixaram de ficar soltos
# direto em "rmark/" (antes distinguidos por sufixo, ex.: "certidao_MPRO.Rmd")
# e passaram a ficar em uma subpasta por empresa, com o MESMO nome da
# empresa escolhida no login (ex.: "rmark/MPRO/certidao.Rmd"), sem sufixo.
# O mesmo vale para os assets (logo), em "assets/<empresa>/logo.png".
# Por isso RMD_DIR/ASSETS_DIR abaixo são apenas os diretórios BASE — o
# caminho final de cada arquivo só é conhecido depois do login, quando a
# empresa (distro) do usuário é definida (ver obter_certidao_tipos() e
# obter_logo_path(), usados no servidor via distroSelecionado()).
# =========================================================================
RMD_DIR <- here::here("rmark")

if (!dir.exists(RMD_DIR)) {
    stop("Pasta base de templates RMarkdown não encontrada em: ", RMD_DIR)
}

if (!dir.exists(ASSETS_DIR)) {
    stop("Pasta base de assets não encontrada em: ", ASSETS_DIR)
}

# Monta os caminhos dos templates de certidão para uma empresa (distro)
# específica, dentro da subpasta "rmark/<empresa>/".
# -------------------------------------------------------------------------
# `distro` é normalizado para MAIÚSCULAS aqui pelo mesmo motivo que em
# conectar_banco()/distro_env() (R/database.R e R/utils.R): listar_distros()
# devolve o valor exatamente como está escrito no .Renviron (sem forçar
# caixa), mas cadastrar_usuario_totp() SEMPRE grava a empresa do usuário
# TOTP em maiúsculas. Sem essa normalização, login AD e login TOTP da
# mesma empresa poderiam resolver para subpastas de nomes diferentes
# (ex.: "rmark/Mpro/" vs "rmark/MPRO/") caso o .Renviron não esteja
# 100% em maiúsculas. Por isso as subpastas físicas devem sempre usar o
# nome da empresa em MAIÚSCULAS (ex.: "rmark/MPRO/", "assets/MPRO/").
obter_certidao_tipos <- function(distro) {
    distro <- toupper(trimws(distro))
    rmd_dir_empresa <- file.path(RMD_DIR, distro)

    c(
        "Certidão Simples"                  = file.path(rmd_dir_empresa, "certidao.Rmd"),
        "Certidão de Afastamento Funcional" = file.path(rmd_dir_empresa, "afastamento_funcional.Rmd"),
        "Certidão Funcional Consolidada"    = file.path(rmd_dir_empresa, "funcional_consolidada.Rmd"),
        "Certidão de Tempo de Contribuição" = file.path(rmd_dir_empresa, "tempo_contribuicao.Rmd"),
        "Certidão de Tempo de Serviço"      = file.path(rmd_dir_empresa, "tempo_servico.Rmd"),
        "Certidão de Vínculo Funcional"     = file.path(rmd_dir_empresa, "vinculo_funcional.Rmd")
    )
}

# Caminho do logo (usado na geração da certidão) para uma empresa
# específica, dentro da subpasta "assets/<empresa>/". Ver nota de
# normalização de maiúsculas acima, em obter_certidao_tipos().
obter_logo_path <- function(distro) {
    distro <- toupper(trimws(distro))
    file.path(ASSETS_DIR, distro, "logo.png")
}

# Avisa (sem interromper) sobre pastas/templates/logo de cada empresa
# configurada que ainda não existam — o app segue funcionando normalmente
# para as empresas corretamente configuradas.
for (distro_rmd_check in DISTROS_DISPONIVEIS) {

    distro_rmd_check_norm <- toupper(trimws(distro_rmd_check))
    rmd_dir_empresa_check <- file.path(RMD_DIR, distro_rmd_check_norm)

    if (!dir.exists(rmd_dir_empresa_check)) {
        warning(
            "Subpasta de templates RMarkdown não encontrada para a ",
            "empresa '", distro_rmd_check, "': ", rmd_dir_empresa_check,
            ". A geração de certidão falhará para essa empresa até a ",
            "pasta ser criada."
        )
    }

    certidao_tipos_check <- obter_certidao_tipos(distro_rmd_check)

    for (tipo_nome in names(certidao_tipos_check)) {
        caminho_tipo <- certidao_tipos_check[[tipo_nome]]
        if (!file.exists(caminho_tipo)) {
            warning(
                "Template RMarkdown não encontrado para '", tipo_nome,
                "' (empresa '", distro_rmd_check, "'): ", caminho_tipo,
                ". Essa opção falhará se for selecionada até o arquivo ",
                "ser adicionado."
            )
        }
    }

    logo_path_check <- obter_logo_path(distro_rmd_check)

    if (!file.exists(logo_path_check)) {
        warning(
            "Logo não encontrada para a empresa '", distro_rmd_check,
            "' em: ", logo_path_check,
            ". A certidão dessa empresa poderá ser gerada sem o logotipo."
        )
    }
}
rm(distro_rmd_check)

# =========================================================================
# LOGO DA TELA DE LOGIN
# =========================================================================
IMG_DIR <- file.path(APP_DIR, "img")

if (dir.exists(IMG_DIR)) {
    addResourcePath("img", IMG_DIR)
} else {
    warning(
        "Pasta 'img' não encontrada em: ", IMG_DIR,
        ". A logo da tela de login não será exibida."
    )
}

LOGIN_LOGO_PATH <- file.path(IMG_DIR, "declaraserv_logo.png")

if (!file.exists(LOGIN_LOGO_PATH)) {
    warning(
        "Logo da tela de login não encontrada em: ", LOGIN_LOGO_PATH,
        ". A tela de login será exibida sem a logo."
    )
}

# =========================================================================
# LOGO HORIZONTAL DO CABEÇALHO (substitui o texto "Certidão")
# =========================================================================
HEADER_LOGO_PATH <- file.path(IMG_DIR, "declaraserv_logo_horizontal.png")

if (!file.exists(HEADER_LOGO_PATH)) {
    warning(
        "Logo horizontal não encontrada em: ", HEADER_LOGO_PATH,
        ". O cabeçalho da tela principal será exibido sem a logo."
    )
}

# =========================================================================
# README.md — exibido em janela modal a partir da tela principal
# =========================================================================
README_PATH <- file.path(APP_DIR, "README.md")

# Lido e convertido UMA vez na inicialização (antes era relido do disco
# e reconvertido a cada clique no botão de ajuda).
README_HTML <- if (file.exists(README_PATH)) {
    shiny::markdown(paste(
        readLines(README_PATH, warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
    ))
} else {
    warning(
        "Arquivo README.md não encontrado em: ", README_PATH,
        ". O botão de ajuda exibirá uma mensagem informando que o ",
        "arquivo não está disponível."
    )
    div(
        style = "color:#842029;",
        "Arquivo README.md não encontrado em: ", README_PATH
    )
}

# =========================================================================
# CONEXÃO COM O IRIS
# -------------------------------------------------------------------------
# O driver JDBC (classe + .jar) é compartilhado por todas as empresas por
# padrão — validado aqui uma única vez. Sem ele nenhuma empresa conecta
# ao IRIS, mas o app NÃO é interrompido: as declarações passam a usar os
# dados de EXEMPLO do SQLite (R/fonte_dados.R). As credenciais específicas de
# cada empresa (URL/usuário/senha) já foram checadas (com aviso, não
# obrigatório) no laço de EMPRESAS (MULTI-DISTRO) acima, e são lidas de
# fato por conectar_banco(distro) em R/database.R (que também guarda o
# driver JDBC em cache, para não recarregar o .jar a cada consulta).
# =========================================================================
JAR_PATH <- Sys.getenv("IRIS_JAR_PATH", unset = "")

if (!nzchar(JAR_PATH) || !file.exists(JAR_PATH)) {
    warning(
        "Driver JDBC do IRIS não encontrado em IRIS_JAR_PATH: '", JAR_PATH,
        "'. As declarações usarão os dados de EXEMPLO do SQLite até o ",
        "caminho ser corrigido no .Renviron."
    )
}

# =========================================================================
# BANCO DE LOGIN (SQLite) — auditoria de acesso + usuários TOTP
# =========================================================================

DB_LOGIN_PATH <- file.path(APP_DIR, "data", "declaraserv.db")
dir.create(dirname(DB_LOGIN_PATH), recursive = TRUE, showWarnings = FALSE)

con <- dbConnect(SQLite(), DB_LOGIN_PATH)

# Evita erros esporádicos de "database is locked" quando duas sessões do
# Shiny gravam auditoria/TOTP quase ao mesmo tempo (SQLite serializa
# escritas; com um pequeno timeout, a segunda tentativa apenas espera em
# vez de falhar imediatamente).
dbExecute(con, "PRAGMA busy_timeout = 5000;")

# WAL: leituras (auditoria, lista TOTP) não bloqueiam as gravações de
# login de outras sessões, e vice-versa.
invisible(dbGetQuery(con, "PRAGMA journal_mode = WAL;"))

garantir_schema_login <- function(con) {

    dbExecute(con, "
        CREATE TABLE IF NOT EXISTS login_auditoria (
            id       INTEGER PRIMARY KEY AUTOINCREMENT,
            login    TEXT NOT NULL,
            metodo   TEXT NOT NULL,
            sucesso  INTEGER NOT NULL,
            datahora TEXT NOT NULL
        )
    ")

    dbExecute(con, "
        CREATE TABLE IF NOT EXISTS usuarios_totp (
            login      TEXT PRIMARY KEY,
            nome       TEXT NOT NULL,
            secret_key TEXT NOT NULL,
            distro     TEXT,
            ativo      INTEGER NOT NULL DEFAULT 1
        )
    ")

    # Migração: instalações que já rodaram uma versão anterior deste app
    # (antes do suporte multi-empresa) têm a tabela usuarios_totp sem a
    # coluna "distro" — CREATE TABLE IF NOT EXISTS não adiciona colunas
    # a uma tabela já existente, então isso precisa ser feito à parte.
    colunas_totp <- dbListFields(con, "usuarios_totp")
    if (!("distro" %in% colunas_totp)) {
        dbExecute(con, "ALTER TABLE usuarios_totp ADD COLUMN distro TEXT")
    }
}

garantir_schema_login(con)

onStop(function() {
    try(dbDisconnect(con), silent = TRUE)
})

# =========================================================================
# LOGIN DE ADMINISTRADOR (SENHA MESTRA)
# -------------------------------------------------------------------------
# Terceira opção da tela de login, além de AD e Authenticator. Mesmas
# regras do FarolJus Lite:
#
#   - Só vale para os logins listados em ADMINS_TOTP (comparação sem
#     diferenciar maiúsculas/minúsculas). Para qualquer outro login, a
#     senha é tratada como inválida, igual a qualquer outro texto.
#   - A senha é SENHA_MESTRE_ADMIN, configurável no .Renviron; sem essa
#     variável, usa "M4st3r$" como padrão.
#   - Ainda depende de o login ter cadastro em usuarios_totp (nome). Para
#     isso funcionar mesmo num banco recém-criado, os logins de
#     ADMINS_TOTP são cadastrados automaticamente na inicialização (ver
#     garantir_admins_totp_padrao(), abaixo) — cadastros existentes nunca
#     são sobrescritos.
#   - O administrador atende mais de uma empresa: escolhe a empresa numa
#     janela modal logo após o login (obrigatório) e pode trocá-la depois
#     pelo ícone "Trocar empresa" da barra lateral.
#   - Tem acesso a "Administração TOTP" (menu Configurações), como quem
#     entra via AD.
# =========================================================================
ADMINS_TOTP <- c("adminK", "adminE")

SENHA_MESTRE_ADMIN <- Sys.getenv("SENHA_MESTRE_ADMIN", unset = "M4st3r$")

usuario_eh_admin_totp <- function(login) {

    if (is.null(login) || length(login) == 0 || is.na(login[1]) || trimws(login[1]) == "") {
        return(FALSE)
    }

    tolower(trimws(login[1])) %in% tolower(ADMINS_TOTP)
}

# Cadastro TOTP de um login (ativo), sem validar código — usado só depois
# de a senha mestra já ter sido conferida.
obter_cadastro_admin <- function(con, login) {
    dbGetQuery(
        con,
        "SELECT login, nome, distro FROM usuarios_totp
          WHERE lower(login) = lower(?) AND ativo = 1",
        params = list(trimws(login))
    )
}

# Garante um cadastro em usuarios_totp para cada login de ADMINS_TOTP
# (idempotente: INSERT OR IGNORE nunca sobrescreve um cadastro existente).
# A chave secreta é aleatória e não é exibida — se o administrador quiser
# usar também o Authenticator, recadastre-o em "Administração TOTP".
garantir_admins_totp_padrao <- function(con, admins = ADMINS_TOTP) {

    for (login in admins) {
        dbExecute(
            con,
            "INSERT OR IGNORE INTO usuarios_totp (login, nome, secret_key, distro, ativo)
             VALUES (?, ?, ?, NULL, 1)",
            params = list(login, login, gerar_totp_secret())
        )
    }

    invisible(TRUE)
}

garantir_admins_totp_padrao(con)

# =========================================================================
# DADOS DE EXEMPLO (tabela exemplo_relatorios)
# -------------------------------------------------------------------------
# Cria a tabela e a carga de dados fictícios se ainda não existirem
# (idempotente; nunca sobrescreve dados já presentes). Usada quando o
# IRIS de produção não está acessível — ver R/fonte_dados.R.
# =========================================================================
tryCatch(
    garantir_dados_exemplo(con),
    error = function(e) {
        warning("Não foi possível preparar os dados de exemplo: ", conditionMessage(e))
    }
)

if (identical(modo_fonte_configurado(), "exemplo")) {
    message("DECLARASERV_FONTE_DADOS = exemplo: o IRIS não será consultado.")
}

# =========================================================================
# CONSULTAS — consultar_matricula() e as consultas dos relatórios ficam
# em R/fonte_dados.R (IRIS ou dados de exemplo).
# =========================================================================

# =========================================================================
# LIMPEZA DE DIRETÓRIOS TEMPORÁRIOS DE RENDERIZAÇÃO
# -------------------------------------------------------------------------
# Remove pastas "certidao_*" com mais de `max_idade_horas`.
# =========================================================================
limpar_renders_antigos <- function(max_idade_horas = 2) {

    dirs <- Sys.glob(file.path(tempdir(), "certidao_*"))

    if (length(dirs) == 0) {
        return(invisible(NULL))
    }

    limite <- Sys.time() - (max_idade_horas * 3600)

    for (d in dirs) {
        info <- file.info(d)
        if (!is.na(info$mtime) && info$mtime < limite) {
            unlink(d, recursive = TRUE, force = TRUE)
        }
    }

    invisible(NULL)
}

limpar_renders_antigos()

# =========================================================================
# NOME DE ARQUIVO SEGURO PARA DOWNLOAD
# -------------------------------------------------------------------------
# Remove acentos/espaços/caracteres especiais para evitar problemas de
# download em navegadores/sistemas diferentes.
# =========================================================================
sanitizar_nome_arquivo <- function(texto) {

    texto <- as.character(texto)

    de   <- "áàâãäéèêëíìîïóòôõöúùûüçÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ"
    para <- "aaaaaeeeeiiiiooooouuuucAAAAAEEEEIIIIOOOOOUUUUC"

    texto <- chartr(de, para, texto)
    texto <- gsub("[^A-Za-z0-9]+", "-", texto)
    texto <- gsub("^-+|-+$", "", texto)

    tolower(texto)
}

# =========================================================================
# NÚMERO DA CERTIDÃO (gerado automaticamente)
# -------------------------------------------------------------------------
# Código de 11 dígitos = quantidade de segundos entre 01/01/2025 00:00:00
# (horário de Maceió) e o instante da geração. Esse mesmo código pode ser
# revertido de volta para a data/hora exata (ver recuperar_data_hora11()
# na aba "Validação" de mod_totp_admin.R), funcionando como um "número de
# série" verificável da certidão.
# =========================================================================
gerar_codigo_tempo11 <- function(data_hora = Sys.time()) {
    data_base <- as.POSIXct(
        "2025-01-01 00:00:00",
        tz = "America/Maceio"
    )
    sprintf(
        "%011d",
        as.integer(
            difftime(data_hora, data_base, units = "secs")
        )
    )
}

# Monta o número completo da certidão: {codigo11}/{ano}/DIGEPE.
gerar_numero_certidao <- function(data_hora = Sys.time()) {
    codigo <- gerar_codigo_tempo11(data_hora)
    ano <- format(data_hora, "%Y")
    glue("{codigo}/{ano}/DIGEPE")
}

# =========================================================================
# CONTEÚDO PRINCIPAL (CERTIDÃO) — mesma funcionalidade do app.R original,
# agora exibida dentro da tela autenticada.
# =========================================================================
painel_certidao_ui <- function() {

    div(
        class = "painel",

        div(
            class = "app-subtitle",
            "Informe a matrícula do(a) residente para gerar a certidão."
        ),

        # Campo Matrícula: caixa de texto (IRIS) ou combobox com as
        # matrículas disponíveis (dados de EXEMPLO) — ver
        # output$campo_matricula no server. O id é sempre "matricula".
        uiOutput("campo_matricula"),

        tags$script(HTML("
            $(document).on('input', '#matricula', function() {
                var valorLimpo = $(this).val().replace(/[^0-9]/g, '');

                if ($(this).val() !== valorLimpo) {
                    $(this).val(valorLimpo);
                }
            });
        ")),

        br(),

        actionButton(
            "consultar",
            "Consultar",
            class = "btn-primary"
        ),

        br(),
        br(),

        uiOutput("resultado_consulta"),

        br(),

        uiOutput("selecao_certidao"),

        br(),

        uiOutput("acao_gerar"),

        br(),

        uiOutput("numero_certidao_gerado_ui"),

        br(),

        uiOutput("tempo_processamento"),

        br(),

        uiOutput("download_ui"),

        br(),

        div(
            id = "status_msg",
            style = "color:#6c6c6c;"
        )
    )
}

# =========================================================================
# INTERFACE
# =========================================================================
ui <- fluidPage(

    theme = bs_theme(
        version = 5,
        bootswatch = "flatly"
    ),

    tags$head(

        tags$style(HTML("

            body {
                background: #f4f6f9;
            }

            .app-title {
                margin-top: 20px;
                margin-bottom: 4px;
                font-weight: 600;
            }

            .app-title .logo-horizontal {
                display: block;
                max-width: 280px;
                width: 100%;
                height: auto;
            }

            .app-subtitle {
                color: #6c6c6c;
                margin-bottom: 24px;
            }

            .painel {
                max-width: 720px;
                margin: 0 auto;
                padding: 24px;
            }

            .tabela-consulta {
                margin-top: 12px;
                margin-bottom: 12px;
            }

            /* =============================================
               LOGIN - CARD ENTERPRISE
               ============================================= */

            .login-wrapper {
                min-height: 100vh;
                display: flex;
                align-items: center;
                justify-content: center;
                padding: 40px 20px;
            }

            .login-card {
                width: 100%;
                max-width: 460px;
                background: #fff;
                border: 1px solid rgba(0,0,0,.05);
                border-radius: 1rem;
            }

            .login-icon-badge {
                width: 60px;
                height: 60px;
                border-radius: 50%;
                margin: 0 auto;
                display: flex;
                align-items: center;
                justify-content: center;
                background: linear-gradient(135deg, #003366, #0d6efd);
                color: #fff;
                font-size: 1.4rem;
                box-shadow: 0 .4rem 1rem rgba(13,110,253,.25);
            }

            .login-title {
                text-align: center;
                letter-spacing: -.01em;
            }

            .login-subtitle {
                text-align: center;
                font-size: .9rem;
            }

            /* Passo 1 (seletor de método): o cartão de login fica mais
               largo para caber as três opções lado a lado. Nos passos
               seguintes (formulários), volta aos 460px. */

            .login-card:has(.metodo-opcoes) {
                max-width: 820px;
            }

            /* Cartões de método de acesso (radioButtons estilizado),
               dispostos EM LINHA. flex-wrap + flex-basis mínimo fazem os
               cartões se reorganizarem sozinhos ao redimensionar a
               janela: 3 por linha em telas largas, 2 ou 1 em telas
               estreitas (celular), sempre ocupando a largura toda. */

            /* O Shiny fixa inputs em 300px de largura por padrão —
               aqui o grupo de opções ocupa a largura toda do cartão. */
            .metodo-opcoes .shiny-input-container {
                width: 100%;
                max-width: 100%;
            }

            .metodo-opcoes .shiny-options-group {
                display: flex;
                flex-wrap: wrap;
                gap: .85rem;
                margin-bottom: .85rem;
            }

            .metodo-opcoes .radio {
                flex: 1 1 200px;
                display: flex;
                margin: 0;
            }

            .metodo-opcoes .radio label {
                display: flex;
                align-items: flex-start;
                gap: .85rem;
                width: 100%;
                height: 100%;      /* cartões da mesma linha com a mesma altura */
                box-sizing: border-box;
                margin: 0;
                border: 1.5px solid #e2e6ea;
                border-radius: .85rem;
                padding: 1rem 1.1rem;
                cursor: pointer;
                transition: border-color .15s ease,
                            background-color .15s ease,
                            box-shadow .15s ease,
                            transform .1s ease;
            }

            .metodo-opcoes .radio label:hover {
                border-color: #8fb8ff;
                background: #f5f9ff;
                box-shadow: 0 .25rem .75rem rgba(13,110,253,.08);
                transform: translateY(-1px);
            }

            .metodo-opcoes .radio input[type=radio] {
                margin-top: .3rem;
                accent-color: #0d6efd;
                width: 1.05rem;
                height: 1.05rem;
                flex-shrink: 0;
            }

            .metodo-opcoes .radio:has(input:checked) label {
                border-color: #0d6efd;
                background: #eef4ff;
                box-shadow: 0 .3rem .9rem rgba(13,110,253,.15);
            }

            .metodo-opcao-icone {
                color: #0d6efd;
                font-size: 1.15rem;
                margin-top: .1rem;
            }

            .metodo-opcao-titulo {
                font-weight: 600;
                color: #212529;
            }

            .metodo-opcao-desc {
                font-weight: 400;
                font-size: .8rem;
                color: #6c757d;
            }

            .btn-acesso {
                height: 48px;
                font-weight: 600;
                font-size: 1rem;
                border-radius: .6rem;
            }

            .voltar-link {
                font-size: .85rem;
                color: #6c757d !important;
            }

            .voltar-link:hover {
                color: #0d6efd !important;
            }

          .logo {
            text-align: center;
            font-size: 30px;
            font-weight: bold;
            color: #003366;
            margin-bottom: 25px;
          }

          .logo-login {
            display: block;
            width: 100%;
            max-width: 320px;
            height: auto;
            margin: 0 auto 30px auto;
            filter: drop-shadow(0 3px 8px rgba(0,0,0,.10));
          }

            .foto-usuario {
                width: 64px;
                height: 64px;
                border-radius: 50%;
                border: 3px solid #ddd;
                display: block;
                object-fit: cover;
            }

            .shiny-notification {
                position: fixed !important;
                top: 20px !important;
                right: 20px !important;
                left: auto !important;
                bottom: auto !important;
                transform: none !important;
            }

            #capslock_warning {
                display: none;
                margin-top: 8px;
                padding: 6px 10px;
                font-size: 13px;
                color: #842029;
                background: #f8d7da;
                border: 1px solid #f5c2c7;
                border-radius: 6px;
            }

            .icon-bar {
                position: fixed;
                top: 0;
                left: 0;
                width: 52px;
                height: 100vh;
                background: #003366;
                display: flex;
                flex-direction: column;
                align-items: center;
                padding-top: 14px;
                gap: 12px;
                z-index: 1050;
                transition: width .2s ease;
            }

            .icon-bar .icon-btn {
                width: 36px;
                height: 36px;
                display: flex;
                align-items: center;
                justify-content: center;
                color: rgba(255,255,255,.75);
                font-size: 16px;
                border-radius: 8px;
                cursor: pointer;
                text-decoration: none !important;
                transition: background .15s ease,
                            color .15s ease;
            }

            .icon-bar .icon-btn:hover {
                background: rgba(255,255,255,.12);
                color: #fff;
            }

            .icon-bar .icon-btn.sair {
                margin-top: auto;
                margin-bottom: 14px;
                color: #ff9d9d;
            }

            .icon-bar .icon-btn.sair:hover {
                background: rgba(220,53,69,.25);
                color: #fff;
            }

            /* =============================================
               ÍCONE MENU (expandir/recolher a barra lateral)
               — maior que os demais e sempre no topo.
               ============================================= */

            .icon-bar .icon-btn-menu {
                width: 44px;
                height: 44px;
                font-size: 20px;
            }

            /* Nome de cada ícone — oculto com a barra recolhida, visível
               com a barra expandida (.icon-bar.expandida). */

            .icon-bar .icon-label {
                display: none;
                margin-left: 10px;
                font-size: 13px;
                white-space: nowrap;
            }

            .icon-bar.expandida {
                width: 220px;
                align-items: stretch;
                padding-left: 8px;
                padding-right: 8px;
            }

            .icon-bar.expandida .icon-btn {
                width: 100%;
                justify-content: flex-start;
                padding: 0 8px;
            }

            .icon-bar.expandida .icon-btn-menu {
                padding-left: 8px;
            }

            .icon-bar.expandida .icon-label {
                display: inline;
            }

            /* Separador logo abaixo do ícone Menu. */
            .icon-bar .icon-sep {
                width: 28px;
                border-top: 1px solid rgba(255,255,255,.15);
                margin: 2px auto;
            }

            .icon-bar.expandida .icon-sep {
                width: 100%;
            }

            /* =============================================
               CONFIGURAÇÕES — submenu flutuante à direita da barra
               ============================================= */

            .icon-grupo {
                position: relative;
            }

            .icon-bar.expandida .icon-grupo {
                width: 100%;
            }

            .icon-bar .icon-btn .icon-seta {
                display: none;
                margin-left: auto;
                font-size: 11px;
                opacity: .7;
            }

            .icon-bar.expandida .icon-btn .icon-seta {
                display: inline;
            }

            .submenu-config {
                display: none;
                position: absolute;
                top: 0;
                left: calc(100% + 10px);
                min-width: 230px;
                background: #fff;
                border: 1px solid rgba(0,0,0,.08);
                border-radius: .6rem;
                box-shadow: 0 .5rem 1.25rem rgba(15,23,42,.18);
                padding: .35rem;
                z-index: 1060;
            }

            .submenu-config.aberto {
                display: block;
            }

            .submenu-config .submenu-titulo {
                font-size: .7rem;
                font-weight: 600;
                text-transform: uppercase;
                letter-spacing: .04em;
                color: #6c757d;
                padding: .35rem .6rem .25rem .6rem;
            }

            .submenu-config .submenu-item {
                display: flex;
                align-items: center;
                gap: .6rem;
                padding: .5rem .6rem;
                border-radius: .4rem;
                color: #212529;
                font-size: .9rem;
                text-decoration: none;
                white-space: nowrap;
            }

            .submenu-config .submenu-item:hover {
                background: #eef4ff;
                color: #0d6efd;
            }

            /* =============================================
               AVISO DADOS DE EXEMPLO — fixo no topo do conteúdo
               (position: sticky), sempre visível enquanto a fonte
               de dados for o SQLite de exemplo. Não pode ser
               fechado.
               ============================================= */

            /* O sticky fica no contêiner do uiOutput (filho direto de
               #app-content): num elemento interno ele não teria onde
               deslizar e rolaria junto com a página. */
            .aviso-dados-exemplo-wrap {
                position: sticky;
                top: 0;
                z-index: 1040;
                margin: -20px -25px 16px -25px;
            }

            .aviso-dados-exemplo-wrap:empty {
                margin: 0;
            }

            .aviso-dados-exemplo {
                padding: 10px 25px;
                display: flex;
                align-items: center;
                gap: .75rem;
                color: #fff;
                font-size: .92rem;
                background: repeating-linear-gradient(
                    -45deg, #b45309, #b45309 14px, #c2620f 14px, #c2620f 28px
                );
                box-shadow: 0 .25rem .6rem rgba(0,0,0,.15);
            }

            .aviso-dados-exemplo .aviso-icone {
                font-size: 1.25rem;
                flex-shrink: 0;
            }

            .aviso-dados-exemplo .aviso-titulo {
                font-weight: 800;
                letter-spacing: .06em;
            }

            .aviso-dados-exemplo .aviso-motivo {
                display: block;
                font-size: .78rem;
                opacity: .9;
            }

            /* Título dos modais ocupando a largura toda: no Bootstrap 5
               o .modal-title encolhe até o tamanho do texto, e o X da janela
               Administração TOTP ficava colado ao título em vez de ir
               para o canto superior direito. */
            .modal-header .modal-title {
                flex: 1 1 auto;
            }

            /* Calendário (Auditoria - Gráfico) por cima da janela modal. */
            .datepicker.dropdown-menu {
                z-index: 2000 !important;
            }

            /* Opção selecionada (a última clicada entre Mostrar/ocultar,
               Declaração e Configurações) — ver o JS SELEÇÃO NA BARRA
               LATERAL. Sem seleção, todas têm a mesma cor. */
            .icon-bar .icon-btn.ativo {
                background: rgba(255,255,255,.18);
                color: #fff;
                box-shadow: inset 3px 0 0 #6ea8fe;
            }

            #app-content {
                margin-left: 52px;
                padding: 20px 25px;
                transition: margin-left .2s ease;
            }

            /* Com a barra expandida, o conteúdo é empurrado (não fica
               coberto pela barra). */
            .icon-bar.expandida ~ #app-content {
                margin-left: 220px;
            }

            .header-container {
                overflow: hidden;
                max-height: 320px;
                opacity: 1;
                transition: max-height .28s ease,
                            opacity .2s ease,
                            margin .28s ease;
                margin-bottom: 15px;
            }

            .header-container.collapsed {
                max-height: 0;
                opacity: 0;
                margin-bottom: 0;
            }

            .header-info {
                display: grid;
                grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
                column-gap: 28px;
                row-gap: 14px;
                align-items: start;
                width: 100%;
                color: #555;
            }

            .header-info .info-foto {
                grid-column: 1 / -1;
                display: flex;
                align-items: center;
                gap: 12px;
            }

            .header-info .info-item {
                min-width: 0;
            }

            .header-info .info-label {
                display: block;
                font-size: 11px;
                font-weight: 700;
                text-transform: uppercase;
                letter-spacing: .04em;
                color: #8a8f98;
                margin-bottom: 2px;
            }

            .header-info .info-value {
                display: block;
                font-size: 14px;
                color: #333;
                word-break: break-word;
            }

        ")),

        # =================================================
        # AVISO DE CAPS LOCK
        # =================================================

        tags$script(HTML("

            $(document).on(
                'keydown keyup focus',
                '#senha, #senha_admin',
                function(event) {

                    var aviso =
                        document.getElementById(
                            'capslock_warning'
                        );

                    if (!aviso) return;

                    if (
                        event.originalEvent &&
                        typeof event.originalEvent
                            .getModifierState === 'function'
                    ) {

                        if (
                            event.originalEvent
                                .getModifierState('CapsLock')
                        ) {

                            $(aviso).show();

                        } else {

                            $(aviso).hide();

                        }

                    }

                }
            );

            $(document).on(
                'blur',
                '#senha, #senha_admin',
                function() {

                    $('#capslock_warning').hide();

                }
            );

        ")),

        # =================================================
        # TOGGLE DO CABEÇALHO (client-side, preserva estado)
        # =================================================

        tags$script(HTML("

            Shiny.addCustomMessageHandler(

                'toggle-header',

                function(message) {

                    var header =
                        document.querySelector(
                            '.header-container'
                        );

                    if (!header) return;

                    if (message.oculto) {

                        header.classList.add(
                            'collapsed'
                        );

                    } else {

                        header.classList.remove(
                            'collapsed'
                        );

                    }

                }

            );

            // Expande/recolhe a barra lateral (ícone Menu) sem recriar a UI.
            Shiny.addCustomMessageHandler(
                'toggle-icon-bar',
                function(message) {
                    var barra = document.querySelector('.icon-bar');
                    if (!barra) return;
                    barra.classList.toggle('expandida', !!message.expandida);
                }
            );

            // Submenu 'Configurações' — abre/fecha só no navegador (sem ida
            // ao servidor). Fecha ao escolher uma opção, ao clicar fora ou
            // com Esc.
            function fecharSubmenuConfig() {
                $('#submenu_config').removeClass('aberto');
                $('#btn_configuracoes').attr('aria-expanded', 'false');
            }

            $(document).on('click', '#btn_configuracoes', function(e) {
                e.preventDefault();
                e.stopPropagation();
                var aberto = $('#submenu_config').toggleClass('aberto').hasClass('aberto');
                $(this).attr('aria-expanded', aberto ? 'true' : 'false');
            });

            $(document).on('click', '#submenu_config .submenu-item', fecharSubmenuConfig);

            $(document).on('click', function(e) {
                if (!$(e.target).closest('.icon-grupo').length) fecharSubmenuConfig();
            });

            $(document).on('keydown', function(e) {
                if (e.key === 'Escape') fecharSubmenuConfig();
            });

            // SELEÇÃO NA BARRA LATERAL: destaca a opção clicada (e só
            // ela). Feito no navegador, sem ida ao servidor.
            $(document).on('click', '.icon-bar .icon-btn.selecionavel', function() {
                $('.icon-bar .icon-btn.selecionavel').not(this).removeClass('ativo');
                $(this).addClass('ativo');
            });

        "))

    ),

    # ===================================================
    # LOGIN
    # ===================================================

    uiOutput("tela_login"),

    # ===================================================
    # SISTEMA PRINCIPAL
    # ===================================================

    uiOutput("tela_principal")

)

# =========================================================================
# SERVIDOR
# =========================================================================
server <- function(input, output, session) {

    # ===================================================
    # ESTADO DA SESSÃO (AUTENTICAÇÃO)
    # ===================================================

    autenticado <- reactiveVal(FALSE)
    usuarioLogado <- reactiveVal(NULL)
    dadosUsuario <- reactiveVal(NULL)
    fotoUsuario <- reactiveVal(NULL)

    # Método escolhido na tela de seleção
    # ("ad" | "totp" | "admin" | NULL = seletor)
    metodoAcesso <- reactiveVal(NULL)

    # Método efetivamente usado no login bem-sucedido
    # ("AD" | "TOTP" | "ADMIN")
    metodoAutenticado <- reactiveVal(NULL)

    # Quem pode ver/usar "Administração TOTP" (janela modal): login AD ou Login de
    # Administrador (senha mestra).
    podeAdministrar <- reactive({
        isTRUE(autenticado()) && metodoAutenticado() %in% c("AD", "ADMIN")
    })

    # Quem escolhe a empresa livremente: só o Login de Administrador.
    ehAdminSenhaMestra <- reactive({
        isTRUE(autenticado()) && identical(metodoAutenticado(), "ADMIN")
    })

    # Empresa (distro) do usuário autenticado — escolhida manualmente no
    # login AD, ou herdada do cadastro TOTP (ver R/auth_totp.R). É ela
    # que decide qual banco IRIS é consultado em consultar_matricula()
    # (ou, sem acesso ao IRIS, os dados de exemplo — ver fonteDados).
    distroSelecionado <- reactiveVal(NULL)

    # Fonte de dados das declarações para a empresa atual: "iris" (produção)
    # ou "exemplo" (SQLite, dados fictícios) — ver R/fonte_dados.R. Controla
    # a faixa "DADOS DE EXEMPLO" (output$aviso_dados_exemplo).
    fonteDados <- reactiveVal(NULL)

    # Templates de certidão e logo da empresa atualmente logada — dependem
    # da subpasta "rmark/<empresa>/" e "assets/<empresa>/logo.png" (ver
    # obter_certidao_tipos()/obter_logo_path() definidos no topo do app.R).
    certidaoTiposAtual <- reactive({
        req(distroSelecionado())
        obter_certidao_tipos(distroSelecionado())
    })

    logoPathAtual <- reactive({
        req(distroSelecionado())
        obter_logo_path(distroSelecionado())
    })

    # ===================================================
    # ESTADO DO CABEÇALHO
    # ===================================================

    # Começa oculto: logo após um login bem-sucedido, o bloco de
    # informações do usuário deve iniciar escondido (usuário usa o botão
    # "Mostrar/ocultar informações do usuário" para exibi-lo).
    header_oculto <- reactiveVal(TRUE)

    # Barra lateral expandida (mostrando os nomes dos ícones) ou não — ver
    # ícone "Menu" (toggle_icon_bar) e o handler JS "toggle-icon-bar".
    iconBarExpandida <- reactiveVal(FALSE)

    # ===================================================
    # ABA SELECIONADA
    # ===================================================

    menuSelecionado <- reactiveVal("Declaração")

    # Incrementado a cada logout — sinaliza para mod_totp_admin_server()
    # limpar seu estado interno (chave recém-gerada, campos do formulário),
    # já que o módulo é iniciado uma única vez por sessão do navegador e,
    # sem isso, essas informações ficariam visíveis para quem fizer login
    # em seguida na mesma aba/sessão.
    resetarAdminTotp <- reactiveVal(0)

    # ===================================================
    # ESTADO DA CERTIDÃO (do app.R original)
    # ===================================================

    html_path <- reactiveVal(NULL)
    dados_consulta <- reactiveVal(NULL)
    matricula_consultada <- reactiveVal(NULL)
    tempo_geracao <- reactiveVal(NULL)
    numero_certidao_gerado <- reactiveVal(NULL)

    # Fonte usada na consulta exibida — a geração do documento usa a MESMA
    # fonte, para o documento sair com os dados que estão na tela.
    fonte_consulta <- reactiveVal(NULL)

    limpar_estado_certidao <- function() {
        fonte_consulta(NULL)
        dados_consulta(NULL)
        matricula_consultada(NULL)
        html_path(NULL)
        tempo_geracao(NULL)
        numero_certidao_gerado(NULL)
    }

    validar_matricula <- function(matricula_txt) {

        if (matricula_txt == "") {
            showModal(
                modalDialog(
                    title = "Matrícula obrigatória",
                    "Informe a matrícula do(a) residente antes de consultar.",
                    easyClose = TRUE,
                    footer = modalButton("Fechar")
                )
            )
            return(NULL)
        }

        if (!grepl("^[0-9]+$", matricula_txt)) {
            showModal(
                modalDialog(
                    title = "Matrícula inválida",
                    "A matrícula deve conter apenas números.",
                    easyClose = TRUE,
                    footer = modalButton("Fechar")
                )
            )
            return(NULL)
        }

        valor <- suppressWarnings(as.numeric(matricula_txt))

        if (is.na(valor) || valor <= 0 || valor > .Machine$integer.max) {
            showModal(
                modalDialog(
                    title = "Matrícula inválida",
                    "A matrícula informada não é válida.",
                    easyClose = TRUE,
                    footer = modalButton("Fechar")
                )
            )
            return(NULL)
        }

        as.integer(valor)
    }

    # Quando a matrícula muda, os dados anteriores deixam de ser válidos.
    observeEvent(input$matricula, {
        limpar_estado_certidao()
    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # SELEÇÃO DO MÉTODO DE ACESSO
    # ---------------------------------------------------------------------

    observeEvent(input$continuar, {
        req(input$metodo_acesso)
        metodoAcesso(input$metodo_acesso)
    }, ignoreInit = TRUE)

    observeEvent(input$voltar_metodo, {
        metodoAcesso(NULL)
    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # LOGIN - AD
    # ---------------------------------------------------------------------

    observeEvent(input$entrar, {

        req(input$usuario, input$senha)

        distro_escolhida <- input$distro_ad

        if (is.null(distro_escolhida) || trimws(distro_escolhida) == "") {
            showNotification(
                "Selecione uma empresa válida antes de continuar.",
                type = "warning"
            )
            return(invisible(NULL))
        }

        # Defesa extra: o <select> já restringe as opções no navegador,
        # mas nada impede uma requisição manipulada tentando mandar um
        # valor fora da lista — Sys.getenv() com um nome inválido não
        # causaria dano, mas validamos mesmo assim para dar um erro claro.
        if (!(distro_escolhida %in% DISTROS_DISPONIVEIS)) {
            showNotification(
                "Empresa selecionada é inválida.",
                type = "error"
            )
            return(invisible(NULL))
        }

        if (!isTRUE(PYTHON_STATUS$ok)) {
            showNotification(
                paste("Login AD indisponível no momento.", PYTHON_STATUS$mensagem),
                type = "error",
                duration = 15
            )
            return(invisible(NULL))
        }

        dados <- tryCatch(
            authenticate_ad(input$usuario, input$senha, distro_escolhida),
            error = function(e) {
                showNotification(
                    paste("Erro ao consultar o Active Directory:", conditionMessage(e)),
                    type = "error"
                )
                NULL
            }
        )

        registrar_auditoria(
            con,
            input$usuario,
            paste0("AD:", distro_escolhida),
            !is.null(dados)
        )

        if (!is.null(dados)) {

            autenticado(TRUE)
            usuarioLogado(input$usuario)
            dadosUsuario(dados)
            fotoUsuario(obter_foto_usuario(dados))
            metodoAutenticado("AD")
            distroSelecionado(distro_escolhida)
            menuSelecionado("Declaração")

            # Inicia com as informações do usuário escondidas.
            header_oculto(TRUE)

            updateTabsetPanel(
                session,
                "menu",
                selected = "Declaração"
            )

            showNotification(
                paste("Bem-vindo", obter_campo(dados, "displayName")),
                type = "message"
            )

        } else {

            showNotification(
                "Usuário ou senha inválidos",
                type = "error"
            )

        }

    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # LOGIN - TOTP (Authenticator)
    # ---------------------------------------------------------------------

    observeEvent(input$entrar_totp, {

        req(input$usuario_totp, input$codigo_totp)

        dados <- autenticar_totp(con, input$usuario_totp, input$codigo_totp)

        if (!is.null(dados) && (is.null(dados$distro) || is.na(dados$distro) || trimws(dados$distro) == "")) {
            # Usuário TOTP cadastrado antes do suporte multi-empresa (ou
            # sem empresa definida): não dá pra saber qual banco consultar.
            showNotification(
                paste0(
                    "O usuário '", dados$login, "' não tem uma empresa ",
                    "associada. Peça para um administrador recadastrar o ",
                    "acesso TOTP em Administração TOTP, selecionando a ",
                    "empresa."
                ),
                type = "error",
                duration = 10
            )
            dados <- NULL
        }

        if (!is.null(dados)) {

            autenticado(TRUE)
            usuarioLogado(dados$login)
            dadosUsuario(dados)
            fotoUsuario(NULL)
            metodoAutenticado("TOTP")
            distroSelecionado(dados$distro)
            menuSelecionado("Declaração")

            # Inicia com as informações do usuário escondidas.
            header_oculto(TRUE)

            updateTabsetPanel(
                session,
                "menu",
                selected = "Declaração"
            )

            showNotification(
                paste("Bem-vindo", dados$displayName),
                type = "message"
            )

        } else {

            showNotification(
                "Usuário ou código inválido",
                type = "error"
            )

        }

    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # LOGIN DE ADMINISTRADOR (senha mestra)
    # ---------------------------------------------------------------------

    observeEvent(input$entrar_admin, {

        req(input$usuario_admin, input$senha_admin)

        login_informado <- trimws(input$usuario_admin)

        # Senha mestra só vale para logins de ADMINS_TOTP. Para qualquer
        # outro login (ou senha errada) a resposta é a mesma mensagem
        # genérica, sem revelar quais logins são de administrador.
        senha_ok <- usuario_eh_admin_totp(login_informado) &&
            identical(input$senha_admin, SENHA_MESTRE_ADMIN)

        registro <- if (senha_ok) obter_cadastro_admin(con, login_informado) else NULL

        dados <- if (!is.null(registro) && nrow(registro) == 1) {
            list(
                login       = registro$login[1],
                displayName = registro$nome[1],
                distro      = registro$distro[1]
            )
        } else {
            NULL
        }

        registrar_auditoria(con, login_informado, "ADMIN:SENHA_MESTRA", !is.null(dados))

        if (!is.null(dados)) {

            autenticado(TRUE)
            usuarioLogado(dados$login)
            dadosUsuario(dados)
            fotoUsuario(NULL)
            metodoAutenticado("ADMIN")

            # A empresa é escolhida na janela modal logo abaixo (não herda
            # automaticamente a do cadastro, que pode nem estar preenchida).
            distroSelecionado(NULL)
            menuSelecionado("Declaração")
            header_oculto(TRUE)

            showNotification(
                paste("Bem-vindo", dados$displayName),
                type = "message"
            )

            mostrar_modal_empresa(obrigatorio = TRUE, sugerida = dados$distro)

        } else if (senha_ok) {

            # Senha mestra correta, mas o login não tem cadastro ativo em
            # usuarios_totp (ex.: foi desativado em Administração TOTP).
            showNotification(
                paste0(
                    "O usuário '", login_informado, "' não tem cadastro TOTP ativo. ",
                    "Cadastre-o em Administração TOTP antes de usar a senha de administrador."
                ),
                type = "error",
                duration = 10
            )

        } else {

            showNotification(
                "Usuário ou senha de administrador inválidos",
                type = "error"
            )

        }

    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # ESCOLHA / TROCA DE EMPRESA (apenas Login de Administrador)
    # ---------------------------------------------------------------------

    mostrar_modal_empresa <- function(obrigatorio = FALSE, sugerida = NULL) {

        atual <- distroSelecionado()
        if (is.null(atual) || is.na(atual)) atual <- sugerida
        if (!is.null(atual) && !is.na(atual)) atual <- toupper(trimws(atual))

        opcoes <- DISTROS_DISPONIVEIS
        selecionada <- opcoes[toupper(trimws(opcoes)) %in% atual]
        if (length(selecionada) == 0 && length(opcoes) > 0) selecionada <- opcoes[1]

        showModal(
            modalDialog(
                title = "Selecione a empresa",

                if (length(opcoes) == 0) {
                    div(
                        class = "alert alert-warning mb-0",
                        "Nenhuma empresa está configurada em DISTRO_1, DISTRO_2... ",
                        "no .Renviron."
                    )
                } else {
                    tagList(
                        p(
                            class = "text-muted",
                            "Como administrador, você pode usar o sistema em nome de ",
                            "qualquer empresa configurada. As consultas e certidões ",
                            "seguirão a empresa selecionada abaixo."
                        ),
                        selectInput(
                            "empresa_admin",
                            "Empresa",
                            choices = opcoes,
                            selected = selecionada,
                            width = "100%"
                        )
                    )
                },

                easyClose = !obrigatorio,

                footer = tagList(
                    if (!obrigatorio) modalButton("Cancelar"),
                    if (length(opcoes) > 0) {
                        actionButton(
                            "confirmar_empresa_admin",
                            "Usar esta empresa",
                            class = "btn-primary"
                        )
                    }
                )
            )
        )
    }

    observeEvent(input$confirmar_empresa_admin, {

        req(ehAdminSenhaMestra(), input$empresa_admin)

        # Defesa extra contra valor fora da lista (requisição manipulada).
        if (!(input$empresa_admin %in% DISTROS_DISPONIVEIS)) {
            showNotification("Empresa selecionada é inválida.", type = "error")
            return(invisible(NULL))
        }

        removeModal()

        if (identical(input$empresa_admin, distroSelecionado())) {
            return(invisible(NULL))
        }

        # Os dados consultados pertencem ao banco da empresa anterior.
        limpar_estado_certidao()
        distroSelecionado(input$empresa_admin)

        showNotification(
            paste("Usando o sistema como a empresa", input$empresa_admin),
            type = "message"
        )

    }, ignoreInit = TRUE)

    observeEvent(input$trocar_empresa, {
        req(ehAdminSenhaMestra())
        mostrar_modal_empresa(obrigatorio = FALSE)
    }, ignoreInit = TRUE)

    # Empresa no cabeçalho: saída própria, para que trocar de empresa não
    # recrie toda a tela principal (abas, matrícula digitada etc.).
    output$empresa_atual_display <- renderText({
        req(autenticado())
        distro <- distroSelecionado()
        if (is.null(distro) || is.na(distro)) "(não selecionada)" else distro
    })

    # ---------------------------------------------------------------------
    # FONTE DE DADOS (IRIS ou EXEMPLO)
    # ---------------------------------------------------------------------
    # resolver_fonte_dados() testa o acesso ao IRIS da empresa (com cache
    # de alguns minutos, compartilhado entre sessões) e devolve "iris" ou
    # "exemplo". É chamada ao definir a empresa (login / troca de empresa)
    # e antes de cada consulta — se o IRIS cair ou voltar, a faixa de aviso
    # acompanha.
    atualizar_fonte_dados <- function(distro) {
        fonte <- tryCatch(
            resolver_fonte_dados("auto", distro),
            error = function(e) "exemplo"
        )
        fonteDados(fonte)
        fonte
    }

    observeEvent(distroSelecionado(), {
        distro <- distroSelecionado()
        req(distro)
        withProgress(
            message = "Verificando acesso ao banco de dados...",
            value = 0.5,
            atualizar_fonte_dados(distro)
        )
    }, ignoreNULL = TRUE)

    # ---------------------------------------------------------------------
    # CAMPO MATRÍCULA — texto (IRIS) ou combobox (dados de EXEMPLO)
    # ---------------------------------------------------------------------
    # Com os dados de exemplo, digitar uma matrícula "de cabeça" não faz
    # sentido: o campo vira um combobox com as matrículas do SQLite
    # (busca por número ou nome). Volta a ser caixa de texto quando a
    # fonte é o IRIS. Em ambos os casos o input é input$matricula, então
    # validação e consulta não mudam.
    output$campo_matricula <- renderUI({

        req(autenticado())

        if (identical(fonteDados(), "exemplo")) {

            opcoes <- tryCatch(listar_matriculas_exemplo(), error = function(e) NULL)

            if (!is.null(opcoes) && nrow(opcoes) > 0) {

                escolhas <- stats::setNames(
                    as.character(opcoes$MATRICULA),
                    paste0(opcoes$MATRICULA, " — ", opcoes$NOME)
                )

                atual <- isolate(input$matricula)
                selecionada <- if (!is.null(atual) && atual %in% escolhas) atual else ""

                return(selectizeInput(
                    inputId = "matricula",
                    label = "Matrícula (dados de exemplo)",
                    choices = c("", escolhas),
                    selected = selecionada,
                    width = "460px",
                    options = list(
                        placeholder = "Selecione ou digite a matrícula ou o nome"
                    )
                ))
            }
        }

        textInput(
            inputId = "matricula",
            label = "Matrícula",
            value = "",
            placeholder = "Somente números"
        )
    })

    output$aviso_dados_exemplo <- renderUI({

        req(autenticado(), identical(fonteDados(), "exemplo"))

        motivo <- if (identical(modo_fonte_configurado(), "exemplo")) {
            "Modo de exemplo definido em DECLARASERV_FONTE_DADOS."
        } else {
            m <- motivo_iris_indisponivel(distroSelecionado())
            paste0(
                "O banco de produção (IRIS) não está acessível",
                if (!is.null(m)) paste0(": ", m) else ".",
                " As consultas e declarações usam dados fictícios do SQLite local."
            )
        }

        div(
            class = "aviso-dados-exemplo",
            role = "alert",
            icon("triangle-exclamation", class = "aviso-icone"),
            div(
                span(class = "aviso-titulo", "DADOS DE EXEMPLO"),
                " — as informações exibidas NÃO são reais e os documentos gerados não têm validade.",
                span(class = "aviso-motivo", motivo)
            )
        )
    })

    # ---------------------------------------------------------------------
    # LOGOUT
    # ---------------------------------------------------------------------

    observeEvent(input$sair, {

        autenticado(FALSE)
        usuarioLogado(NULL)
        dadosUsuario(NULL)
        fotoUsuario(NULL)
        metodoAutenticado(NULL)
        metodoAcesso(NULL)
        distroSelecionado(NULL)
        fonteDados(NULL)
        menuSelecionado("Declaração")

        # Dados da certidão são de uma pessoa específica; ao sair, não
        # devem ficar disponíveis para quem fizer login a seguir na
        # mesma aba/sessão do navegador.
        limpar_estado_certidao()

        # Idem para o formulário/chave TOTP recém-gerada (ver
        # mod_totp_admin_server() e o comentário em resetarAdminTotp acima).
        resetarAdminTotp(resetarAdminTotp() + 1)
        adminAberto(FALSE)

        # A tela principal é recriada no próximo login com a barra
        # recolhida — o estado precisa acompanhar, senão o primeiro clique
        # no ícone Menu "não faz nada".
        iconBarExpandida(FALSE)

    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # BARRA LATERAL — EXPANDIR/RECOLHER (client-side, não recria a UI)
    # ---------------------------------------------------------------------

    observeEvent(input$toggle_icon_bar, {

        iconBarExpandida(!iconBarExpandida())

        session$sendCustomMessage(
            "toggle-icon-bar",
            list(expandida = iconBarExpandida())
        )

    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # BARRA LATERAL — NAVEGAÇÃO ENTRE AS ABAS
    # ---------------------------------------------------------------------
    # O botão Declaração abre a aba correspondente (que continua visível no
    # topo). O destaque do botão é feito no navegador, ao clicar.

    observeEvent(input$ir_declaracao, {
        nav_select("menu", selected = "Declaração", session = session)
    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # ADMINISTRAÇÃO TOTP (janela modal)
    # ---------------------------------------------------------------------
    # Antes era uma aba ao lado de Declaração. Agora abre em janela modal
    # pelo menu Configurações > Administração TOTP, como Trocar empresa e
    # Ver instruções (só para login AD ou de Administrador).
    #
    # adminAberto() é o `ativo` do módulo: as consultas ao banco (usuários
    # TOTP, auditoria) só rodam com a janela aberta. Por isso a janela não
    # fecha com clique fora/Esc (easyClose = FALSE) — só pelo "Fechar" ou
    # pelo X, que passam por observeEvent(input$fechar_admin) e mantêm
    # adminAberto() coerente. Ao fechar, resetarAdminTotp() limpa a chave
    # recém-gerada (secret em texto puro) e os campos do formulário.

    adminAberto <- reactiveVal(FALSE)

    observeEvent(input$ir_admin_totp, {

        req(podeAdministrar())

        adminAberto(TRUE)

        showModal(
            modalDialog(
                title = div(
                    style = "position:relative; padding-right:28px;",
                    icon("user-shield", class = "me-2"),
                    "Administração TOTP",
                    tags$button(
                        type = "button",
                        class = "btn-close",
                        style = "position:absolute; top:50%; right:0; transform:translateY(-50%);",
                        `aria-label` = "Fechar",
                        title = "Fechar",
                        onclick = "Shiny.setInputValue('fechar_admin', Math.random(), {priority: 'event'})"
                    )
                ),
                mod_totp_admin_ui("totp_admin"),
                size = "xl",
                easyClose = FALSE,
                footer = actionButton("fechar_admin", "Fechar")
            )
        )

    }, ignoreInit = TRUE)

    observeEvent(input$fechar_admin, {
        adminAberto(FALSE)
        resetarAdminTotp(resetarAdminTotp() + 1)
        removeModal()
    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # ALTERNÂNCIA DO CABEÇALHO (client-side, não recria a UI)
    # ---------------------------------------------------------------------

    observeEvent(input$toggle_header, {

        header_oculto(!header_oculto())

        session$sendCustomMessage(
            "toggle-header",
            list(oculto = header_oculto())
        )

    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # README (janela modal)
    # ---------------------------------------------------------------------

    observeEvent(input$mostrar_readme, {

        showModal(
            modalDialog(
                title = "README",
                README_HTML,
                easyClose = TRUE,
                size = "l",
                footer = modalButton("Fechar")
            )
        )

    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # CAPTURA DA ABA SELECIONADA
    # ---------------------------------------------------------------------

    observeEvent(input$menu, {
        req(input$menu)
        menuSelecionado(input$menu)
    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # TELA DE LOGIN
    # ---------------------------------------------------------------------

    output$tela_login <- renderUI({

        if (autenticado()) {
            return(NULL)
        }

        div(
            class = "login-wrapper",

            div(
                class = "login-card shadow-sm p-4",

                div(
                    class = "logo-container mb-4",

                    tags$img(
                        src = "img/declaraserv_logo.png",
                        class = "logo-login",
                        alt = "DeclaraServ"
                    )
                ),

                tags$h4(
                    "Acesso ao sistema",
                    class = "login-title fw-bold mb-1"
                ),

                tags$p(
                    "Escolha como deseja entrar no DeclaraServ",
                    class = "login-subtitle text-muted mb-4"
                ),

                if (is.null(metodoAcesso())) {

                    # =============================================
                    # PASSO 1 - SELETOR DE MÉTODO
                    # =============================================

                    tagList(

                        div(
                            class = "metodo-opcoes",

                            radioButtons(
                                "metodo_acesso",
                                NULL,
                                choiceNames = list(

                                    tagList(
                                        icon("building-shield", class = "metodo-opcao-icone"),
                                        div(
                                            div("Login Corporativo (AD)", class = "metodo-opcao-titulo"),
                                            div("Entrar com seu usuário e senha de domínio", class = "metodo-opcao-desc")
                                        )
                                    ),

                                    tagList(
                                        icon("mobile-screen-button", class = "metodo-opcao-icone"),
                                        div(
                                            div("Código Authenticator", class = "metodo-opcao-titulo"),
                                            div("Entrar com um código gerado no seu celular", class = "metodo-opcao-desc")
                                        )
                                    ),

                                    tagList(
                                        icon("user-shield", class = "metodo-opcao-icone"),
                                        div(
                                            div("Login de Administrador", class = "metodo-opcao-titulo"),
                                            div("Entrar com a senha de administrador", class = "metodo-opcao-desc")
                                        )
                                    )

                                ),
                                choiceValues = list("ad", "totp", "admin"),
                                selected = character(0)
                            )

                        ),

                        actionButton(
                            "continuar",
                            tagList("Continuar", icon("arrow-right", class = "ms-2")),
                            class = "btn btn-primary w-100 btn-acesso mt-2"
                        )

                    )

                } else if (metodoAcesso() == "ad") {

                    # =============================================
                    # PASSO 2A - LOGIN CORPORATIVO (AD)
                    # =============================================

                    tagList(

                        actionLink(
                            "voltar_metodo",
                            tagList(icon("arrow-left"), " Voltar"),
                            class = "voltar-link mb-4 d-inline-block"
                        ),

                        div(
                            class = "mb-3",
                            selectInput(
                                "distro_ad",
                                "Empresa / Unidade",
                                choices = c(
                                    "Selecione uma empresa" = "",
                                    DISTROS_DISPONIVEIS
                                ),
                                selected = "",
                                width = "100%"
                            )
                        ),

                        div(
                            class = "mb-3",
                            textInput("usuario", "Usuário", width = "100%")
                        ),

                        div(
                            class = "mb-2",
                            passwordInput("senha", "Senha", width = "100%")
                        ),

                        div(
                            id = "capslock_warning",
                            icon("triangle-exclamation"),
                            " Caps Lock está ativado"
                        ),

                        actionButton(
                            "entrar",
                            tagList(icon("right-to-bracket", class = "me-2"), "Entrar"),
                            class = "btn btn-primary w-100 btn-acesso mt-4"
                        )

                    )

                } else if (metodoAcesso() == "totp") {

                    # =============================================
                    # PASSO 2B - CÓDIGO AUTHENTICATOR (TOTP)
                    # =============================================

                    tagList(

                        actionLink(
                            "voltar_metodo",
                            tagList(icon("arrow-left"), " Voltar"),
                            class = "voltar-link mb-4 d-inline-block"
                        ),

                        div(
                            class = "mb-3",
                            textInput("usuario_totp", "Usuário", width = "100%")
                        ),

                        div(
                            class = "mb-2",
                            textInput(
                                "codigo_totp",
                                "Código do Authenticator",
                                placeholder = "000000",
                                width = "100%"
                            )
                        ),

                        actionButton(
                            "entrar_totp",
                            tagList(icon("key", class = "me-2"), "Entrar"),
                            class = "btn btn-primary w-100 btn-acesso mt-4"
                        )

                    )

                } else if (metodoAcesso() == "admin") {

                    # =============================================
                    # PASSO 2C - LOGIN DE ADMINISTRADOR (senha mestra)
                    # =============================================

                    tagList(

                        actionLink(
                            "voltar_metodo",
                            tagList(icon("arrow-left"), " Voltar"),
                            class = "voltar-link mb-4 d-inline-block"
                        ),

                        div(
                            class = "mb-3",
                            textInput("usuario_admin", "Usuário", width = "100%")
                        ),

                        div(
                            class = "mb-2",
                            passwordInput("senha_admin", "Senha de Administrador", width = "100%")
                        ),

                        div(
                            id = "capslock_warning",
                            icon("triangle-exclamation"),
                            " Caps Lock está ativado"
                        ),

                        actionButton(
                            "entrar_admin",
                            tagList(icon("user-shield", class = "me-2"), "Entrar"),
                            class = "btn btn-primary w-100 btn-acesso mt-4"
                        )

                    )

                }

            )

        )
    })

    # ---------------------------------------------------------------------
    # TELA PRINCIPAL
    # ---------------------------------------------------------------------

    output$tela_principal <- renderUI({

        req(autenticado())

        tagList(

            # =================================================
            # BARRA DE ÍCONES
            # =================================================

            div(

                class = paste(
                    "icon-bar",
                    if (isolate(iconBarExpandida())) "expandida"
                ),

                # Maior que os demais e sempre no topo: expande/recolhe a
                # barra, mostrando o nome de cada ícone.
                actionLink(
                    "toggle_icon_bar",
                    tagList(
                        icon("bars"),
                        tags$span(class = "icon-label", "Menu")
                    ),
                    class = "icon-btn icon-btn-menu",
                    title = "Expandir/recolher menu"
                ),

                div(class = "icon-sep"),

                # -----------------------------------------------
                # OPÇÕES — classe "selecionavel": a opção clicada fica
                # destacada (classe "ativo") e só ela; nenhuma começa
                # destacada. Ver o JS "SELEÇÃO NA BARRA LATERAL".
                # -----------------------------------------------

                actionLink(
                    "toggle_header",
                    tagList(
                        icon("id-badge"),
                        tags$span(class = "icon-label", "Mostrar/ocultar")
                    ),
                    class = "icon-btn selecionavel",
                    title = "Mostrar/ocultar informações do usuário"
                ),

                actionLink(
                    "ir_declaracao",
                    tagList(
                        icon("file-signature"),
                        tags$span(class = "icon-label", "Declaração")
                    ),
                    class = "icon-btn selecionavel",
                    title = "Declaração"
                ),

                # -----------------------------------------------
                # CONFIGURAÇÕES — submenu com Administração TOTP,
                # Trocar empresa e Ver instruções. Os IDs das opções
                # são os mesmos dos antigos ícones, então os
                # observeEvent() do server continuam valendo. Cada
                # opção só aparece para quem pode usá-la:
                #   - Administração TOTP: login AD ou de Administrador;
                #   - Trocar empresa: login de Administrador;
                #   - Ver instruções: todos.
                # O abrir/fechar do submenu é só no navegador (JS
                # fecharSubmenuConfig / #btn_configuracoes).
                # -----------------------------------------------

                div(
                    class = "icon-grupo",

                    tags$a(
                        id = "btn_configuracoes",
                        href = "#",
                        class = "icon-btn selecionavel",
                        title = "Configurações",
                        role = "button",
                        `aria-haspopup` = "true",
                        `aria-expanded` = "false",
                        icon("gear"),
                        tags$span(class = "icon-label", "Configurações"),
                        tags$span(class = "icon-seta", icon("chevron-right"))
                    ),

                    div(
                        id = "submenu_config",
                        class = "submenu-config",
                        role = "menu",

                        div(class = "submenu-titulo", "Configurações"),

                        if (podeAdministrar()) {
                            actionLink(
                                "ir_admin_totp",
                                tagList(icon("user-shield", class = "fa-fw"), "Administração TOTP"),
                                class = "submenu-item",
                                role = "menuitem"
                            )
                        },

                        if (ehAdminSenhaMestra()) {
                            actionLink(
                                "trocar_empresa",
                                tagList(icon("building", class = "fa-fw"), "Trocar empresa"),
                                class = "submenu-item",
                                role = "menuitem"
                            )
                        },

                        actionLink(
                            "mostrar_readme",
                            tagList(icon("circle-info", class = "fa-fw"), "Ver instruções"),
                            class = "submenu-item",
                            role = "menuitem"
                        )
                    )
                ),

                actionLink(
                    "sair",
                    tagList(
                        icon("power-off"),
                        tags$span(class = "icon-label", "Sair")
                    ),
                    class = "icon-btn sair",
                    title = "Sair"
                )

            ),

            # =================================================
            # CONTEÚDO
            # =================================================

            div(

                id = "app-content",

                # Faixa "DADOS DE EXEMPLO" (só aparece com a fonte de
                # exemplo) — fixa no topo enquanto a página rola.
                uiOutput("aviso_dados_exemplo", class = "aviso-dados-exemplo-wrap"),

                # ===============================================
                # CABEÇALHO
                # ===============================================

                div(

                    class = paste(
                        "header-container",
                        if (isolate(header_oculto())) "collapsed" else ""
                    ),

                    div(
                        class = "app-title",
                        tags$img(
                            src = "img/declaraserv_logo_horizontal.png",
                            class = "logo-horizontal",
                            alt = "DeclaraServ"
                        )
                    ),

                    if (identical(metodoAutenticado(), "AD")) {

                        tags$div(

                            class = "header-info",

                            if (!is.null(fotoUsuario())) {
                                div(
                                    class = "info-item info-foto",
                                    tags$img(src = fotoUsuario(), class = "foto-usuario")
                                )
                            },

                            div(
                                class = "info-item",
                                tags$span("Usuário", class = "info-label"),
                                tags$span(obter_campo(dadosUsuario(), "displayName"), class = "info-value")
                            ),

                            div(
                                class = "info-item",
                                tags$span("Departamento", class = "info-label"),
                                tags$span(obter_campo(dadosUsuario(), "department"), class = "info-value")
                            ),

                            div(
                                class = "info-item",
                                tags$span("Criado em", class = "info-label"),
                                tags$span(
                                    formatar_whenCreated(
                                        obter_campo(dadosUsuario(), "whenCreated")
                                    ),
                                    class = "info-value"
                                )
                            ),

                            div(
                                class = "info-item",
                                tags$span("Último acesso", class = "info-label"),
                                tags$span(
                                    formatar_lastLogon(
                                        obter_campo(dadosUsuario(), "lastLogonTimestamp")
                                    ),
                                    class = "info-value"
                                )
                            ),

                            div(
                                class = "info-item",
                                tags$span("Gestor", class = "info-label"),
                                tags$span(extrair_manager(dadosUsuario()$manager), class = "info-value")
                            ),

                            div(
                                class = "info-item",
                                tags$span("Empresa", class = "info-label"),
                                tags$span(textOutput("empresa_atual_display", inline = TRUE), class = "info-value")
                            ),

                            div(
                                class = "info-item",
                                tags$span("Método de acesso", class = "info-label"),
                                tags$span("Login Corporativo (AD)", class = "info-value")
                            )

                        )

                    } else {

                        tags$div(

                            class = "header-info",

                            div(
                                class = "info-item",
                                tags$span("Usuário", class = "info-label"),
                                tags$span(dadosUsuario()$displayName, class = "info-value")
                            ),

                            div(
                                class = "info-item",
                                tags$span("Login", class = "info-label"),
                                tags$span(dadosUsuario()$login, class = "info-value")
                            ),

                            div(
                                class = "info-item",
                                tags$span("Empresa", class = "info-label"),
                                tags$span(textOutput("empresa_atual_display", inline = TRUE), class = "info-value")
                            ),

                            div(
                                class = "info-item",
                                tags$span("Método de acesso", class = "info-label"),
                                tags$span(
                                    if (identical(metodoAutenticado(), "ADMIN")) {
                                        "Login de Administrador"
                                    } else {
                                        "Código Authenticator (TOTP)"
                                    },
                                    class = "info-value"
                                )
                            )

                        )

                    }

                ),

                hr(),

                # ===============================================
                # ABAS
                # ===============================================

                do.call(
                    navset_tab,
                    c(
                        list(
                            id = "menu",
                            selected = isolate(menuSelecionado())
                        ),
                        # Administração TOTP deixou de ser aba: abre em
                        # janela modal pelo menu Configurações.
                        list(
                            nav_panel("Declaração", painel_certidao_ui())
                        )
                    )
                )

            )

        )

    })

    # ---------------------------------------------------------------------
    # CONSULTA
    # ---------------------------------------------------------------------
    observeEvent(input$consultar, {

        req(autenticado())

        distro_atual <- distroSelecionado()

        if (is.null(distro_atual) || trimws(distro_atual) == "") {
            showModal(
                modalDialog(
                    title = "Empresa não definida",
                    "Sua sessão não tem uma empresa associada. Saia e ",
                    "entre novamente selecionando a empresa correta.",
                    easyClose = TRUE,
                    footer = modalButton("Fechar")
                )
            )
            return(invisible(NULL))
        }

        matricula_txt <- trimws(input$matricula)
        matricula_num <- validar_matricula(matricula_txt)

        if (is.null(matricula_num)) {
            return(invisible(NULL))
        }

        limpar_estado_certidao()

        withProgress(
            message = "Consultando dados...",
            value = 0.3,
            {
                # IRIS ou exemplo (teste em cache; atualiza a faixa de aviso).
                fonte_atual <- atualizar_fonte_dados(distro_atual)

                resultado <- tryCatch(
                    {
                        df <- consultar_matricula(matricula_num, distro_atual, fonte_atual)
                        incProgress(0.7)
                        df
                    },
                    error = function(e) {
                        showModal(
                            modalDialog(
                                title = "Erro na consulta",
                                paste0(
                                    "Não foi possível consultar a matrícula ",
                                    matricula_num,
                                    ".\n\nDetalhe técnico: ",
                                    conditionMessage(e)
                                ),
                                easyClose = TRUE,
                                footer = modalButton("Fechar")
                            )
                        )
                        NULL
                    }
                )

                if (!is.null(resultado) && nrow(resultado) == 0) {
                    showModal(
                        modalDialog(
                            title = "Matrícula não encontrada",
                            paste0(
                                "Nenhum registro encontrado para a matrícula ",
                                matricula_num,
                                "."
                            ),
                            easyClose = TRUE,
                            footer = modalButton("Fechar")
                        )
                    )
                    resultado <- NULL
                }

                if (!is.null(resultado)) {
                    fonte_consulta(fonte_atual)
                    dados_consulta(resultado)
                    matricula_consultada(matricula_num)
                }
            }
        )
    })

    output$resultado_consulta <- renderUI({
        req(dados_consulta())

        div(
            class = "tabela-consulta",
            h5("Dados encontrados:"),
            DTOutput("tabela_dados")
        )
    })

    output$tabela_dados <- renderDT({
        req(dados_consulta())

        datatable(
            dados_consulta(),
            options = list(
                dom = "t",
                scrollX = TRUE,
                paging = FALSE,
                searching = FALSE
            ),
            rownames = FALSE
        )
    })

    output$selecao_certidao <- renderUI({
        req(dados_consulta())

        certidao_tipos_atual <- certidaoTiposAtual()

        tagList(
            selectInput(
                inputId = "tipo_certidao",
                label = "Tipo de certidão",
                choices = names(certidao_tipos_atual),
                selected = names(certidao_tipos_atual)[1]
            )
        )
    })

    output$acao_gerar <- renderUI({
        req(dados_consulta())

        actionButton(
            "gerar",
            "Gerar certidão",
            class = "btn-success"
        )
    })

    # ---------------------------------------------------------------------
    # GERAÇÃO DO HTML
    # ---------------------------------------------------------------------
    observeEvent(input$gerar, {

        matricula_num <- matricula_consultada()
        req(matricula_num)

        tipo_selecionado <- input$tipo_certidao
        req(tipo_selecionado)

        # Número da certidão gerado automaticamente no momento da geração
        # (ver gerar_numero_certidao()/gerar_codigo_tempo11() acima). Só é
        # exposto para exibição (numero_certidao_gerado) se a geração for
        # bem-sucedida, mais abaixo.
        numero_certidao <- gerar_numero_certidao()

        rmd_arquivo <- certidaoTiposAtual()[[tipo_selecionado]]
        rmd_path_selecionado <- rmd_arquivo

        html_path(NULL)
        tempo_geracao(NULL)
        numero_certidao_gerado(NULL)

        if (!file.exists(rmd_path_selecionado)) {
            showModal(
                modalDialog(
                    title = "Template não encontrado",
                    paste0(
                        "O arquivo RMarkdown para '", tipo_selecionado,
                        "' não foi encontrado em: ", rmd_path_selecionado
                    ),
                    easyClose = TRUE,
                    footer = modalButton("Fechar")
                )
            )
            return(invisible(NULL))
        }

        limpar_renders_antigos()

        # Marca o início do processamento para medir quanto tempo a
        # geração da certidão selecionada leva.
        inicio_processamento <- Sys.time()

        withProgress(
            message = paste0("Gerando ", tipo_selecionado, "..."),
            value = 0.1,
            {
                saida <- tryCatch(
                    {
                        render_dir <- file.path(
                            tempdir(),
                            paste0(
                                "certidao_",
                                matricula_num,
                                "_",
                                format(Sys.time(), "%Y%m%d%H%M%S")
                            )
                        )

                        dir.create(
                            render_dir,
                            recursive = TRUE,
                            showWarnings = FALSE
                        )

                        tmp_html <- file.path(
                            render_dir,
                            sprintf("certidao_%s.html", matricula_num)
                        )

                        incProgress(
                            0.2,
                            detail = "Preparando documento..."
                        )

                        # Parâmetros do .Rmd — fonte_dados garante que o
                        # documento use a mesma fonte (IRIS ou exemplo)
                        # da consulta exibida na tela. Só são passados os
                        # parâmetros que o template declara no YAML
                        # (rmarkdown::render() recusa parâmetros não
                        # declarados).
                        params_render <- list(
                            matricula = matricula_num,
                            numero_certidao = numero_certidao,
                            logo_path = logoPathAtual(),
                            distro = distroSelecionado(),
                            fonte_dados = if (is.null(fonte_consulta())) "auto" else fonte_consulta()
                        )

                        declarados <- names(rmarkdown::yaml_front_matter(rmd_path_selecionado)$params)
                        params_render <- params_render[names(params_render) %in% declarados]

                        rmarkdown::render(
                            input = rmd_path_selecionado,
                            output_format = "html_document",
                            output_file = tmp_html,
                            output_dir = render_dir,
                            # Arquivos intermediários (.knit.md, *_files/)
                            # ficam na pasta temporária desta geração, e não
                            # ao lado do .Rmd — evita conflito quando duas
                            # sessões geram a mesma certidão ao mesmo tempo.
                            intermediates_dir = render_dir,
                            params = params_render,
                            envir = new.env(parent = globalenv()),
                            knit_root_dir = APP_DIR,
                            clean = TRUE,
                            quiet = TRUE,
                            encoding = "UTF-8"
                        )

                        if (!file.exists(tmp_html)) {
                            stop(
                                "A aplicação terminou sem gerar o arquivo HTML: ",
                                tmp_html
                            )
                        }

                        incProgress(
                            0.8,
                            detail = "Certidão gerada com sucesso."
                        )

                        tmp_html
                    },

                    error = function(e) {
                        detalhe <- conditionMessage(e)

                        showModal(
                            modalDialog(
                                title = "Não foi possível gerar a certidão",
                                div(
                                    style = paste(
                                        "white-space: pre-wrap;",
                                        "font-family: monospace;",
                                        "font-size: 12px;",
                                        "max-height: 500px;",
                                        "overflow-y: auto;"
                                    ),
                                    paste0(
                                        "A matrícula ",
                                        matricula_num,
                                        " foi consultada com sucesso, ",
                                        "mas ocorreu um erro durante a ",
                                        "geração da ",
                                        tipo_selecionado,
                                        ".\n\n",
                                        "Detalhe técnico:\n",
                                        detalhe
                                    )
                                ),
                                easyClose = TRUE,
                                size = "l",
                                footer = modalButton("Fechar")
                            )
                        )

                        NULL
                    }
                )

                # Calcula o tempo total de processamento, independente de
                # sucesso ou falha na geração.
                duracao_segundos <- as.numeric(
                    difftime(Sys.time(), inicio_processamento, units = "secs")
                )
                tempo_geracao(duracao_segundos)

                if (!is.null(saida)) {
                    html_path(saida)
                    numero_certidao_gerado(numero_certidao)
                }
            }
        )
    })

    # ---------------------------------------------------------------------
    # NÚMERO DA CERTIDÃO (gerado automaticamente)
    # ---------------------------------------------------------------------
    output$numero_certidao_gerado_ui <- renderUI({
        req(numero_certidao_gerado())

        tags$div(
            style = "color:#333; font-size: 14px; margin-bottom: 8px;",
            tags$b("Número da certidão: "),
            tags$code(numero_certidao_gerado())
        )
    })

    # ---------------------------------------------------------------------
    # TEMPO DE PROCESSAMENTO
    # ---------------------------------------------------------------------
    output$tempo_processamento <- renderUI({
        req(tempo_geracao())

        tags$div(
            style = "color:#6c6c6c; font-size: 13px;",
            sprintf(
                "Tempo de processamento: %.2f segundos.",
                tempo_geracao()
            )
        )
    })

    # ---------------------------------------------------------------------
    # DOWNLOAD
    # ---------------------------------------------------------------------
    output$download_ui <- renderUI({
        req(html_path())

        downloadButton(
            "baixar",
            "Baixar certidão (HTML)",
            class = "btn-success"
        )
    })

    output$baixar <- downloadHandler(

        filename = function() {
            matricula_txt <- as.character(matricula_consultada())

            tipo_certidao_atual <- input$tipo_certidao
            if (is.null(tipo_certidao_atual)) {
                tipo_certidao_atual <- ""
            }
            tipo_txt <- sanitizar_nome_arquivo(trimws(tipo_certidao_atual))

            # Documento gerado com dados fictícios: nome começa com EXEMPLO_.
            prefixo <- if (identical(fonte_consulta(), "exemplo")) "EXEMPLO_" else ""

            sprintf(
                "%scertidao_%s_%s.html",
                prefixo,
                tipo_txt,
                matricula_txt
            )
        },

        content = function(file) {
            req(html_path())

            if (!file.exists(html_path())) {
                stop("O arquivo HTML não está mais disponível.")
            }

            ok <- file.copy(
                html_path(),
                file,
                overwrite = TRUE
            )

            if (!ok) {
                stop("Não foi possível copiar o HTML para o download.")
            }
        },

        contentType = "text/html"
    )

    # ---------------------------------------------------------------------
    # ADMINISTRAÇÃO TOTP (AD ou Login de Administrador)
    # ---------------------------------------------------------------------
    # Só roda com a janela "Administração TOTP" aberta E para quem tem o
    # perfil — a checagem de podeAdministrar() protege o módulo mesmo que
    # alguém dispare input$ir_admin_totp manualmente.
    mod_totp_admin_server(
        "totp_admin",
        con = con,
        ativo = reactive(adminAberto() && podeAdministrar()),
        resetar = resetarAdminTotp
    )
}

shinyApp(ui, server)
