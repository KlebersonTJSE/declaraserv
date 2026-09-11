# =========================================================================
# app.R — Declaraserv | Geração de Certidão MPRO (Formato HTML)
# -------------------------------------------------------------------------
# Login corporativo (Active Directory) ou código Authenticator (TOTP),
# seguindo o mesmo layout/fluxo de autenticação usado em outros sistemas
# internos, adaptado para a aplicação Declaraserv.
#
# Corrige o diretório de trabalho caso o projeto não
# tenha sido aberto pelo .Rproj
# =========================================================================

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)), silent = TRUE)
}

library(here)
here::i_am("app.R")

# =========================================================================
# .Renviron — carregamento explícito
# -------------------------------------------------------------------------
# O R carrega o .Renviron automaticamente ao iniciar a sessão, mas isso
# depende de onde a sessão foi iniciada (ex.: Shiny Server, RStudio, Rscript
# manual). Para não depender desse comportamento implícito, carregamos aqui
# explicitamente o .Renviron que fica na própria pasta do app — antes de
# QUALQUER library() que dependa de variáveis de ambiente (em especial
# JAVA_HOME, que precisa existir antes de library(rJava) ser chamado).
# =========================================================================
readRenviron(here::here(".Renviron"))

APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
ASSETS_DIR <- file.path(APP_DIR, "assets")

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
JAVA_HOME_PATH <- obter_env_obrigatoria("JAVA_HOME")
Sys.setenv(JAVA_HOME = JAVA_HOME_PATH)

library(shiny)
library(rmarkdown)
library(rJava)
library(RJDBC)
library(glue)
library(DT)

library(bslib)
library(jsonlite)
library(digest)

library(DBI)
library(RSQLite)

library(reticulate)

python_path <- Sys.getenv("RETICULATE_PYTHON", unset = Sys.which("python"))
if (!nzchar(python_path) || !file.exists(python_path)) {
    stop(
        "Python não encontrado em '", python_path, "'. ",
        "Verifique a instalação do Python ou defina RETICULATE_PYTHON no .Renviron ",
        "apontando para o python.exe correto."
    )
}
use_python(python_path, required = TRUE)

# =========================================================================
# MÓDULOS DE AUTENTICAÇÃO, BANCO E UTILITÁRIOS
# =========================================================================
source(here::here("R", "utils.R"))
source(here::here("R", "auth.R"))
source(here::here("R", "auth_totp.R"))
source(here::here("R", "database.R"))
source(here::here("modules", "mod_totp_admin.R"))

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

if (!file.exists(README_PATH)) {
    warning(
        "Arquivo README.md não encontrado em: ", README_PATH,
        ". O botão de ajuda exibirá uma mensagem informando que o ",
        "arquivo não está disponível."
    )
}

# =========================================================================
# CONEXÃO COM O IRIS
# -------------------------------------------------------------------------
# O driver JDBC (classe + .jar) é compartilhado por todas as empresas por
# padrão — validado aqui uma única vez, de forma obrigatória, pois sem
# ele NENHUMA empresa consegue conectar. As credenciais específicas de
# cada empresa (URL/usuário/senha) já foram checadas (com aviso, não
# obrigatório) no laço de EMPRESAS (MULTI-DISTRO) acima, e são lidas de
# fato por conectar_banco(distro) em R/database.R.
# =========================================================================
DRIVER_CLASS <- Sys.getenv(
    "IRIS_DRIVER_CLASS",
    unset = "com.intersystems.jdbc.IRISDriver"
)
JAR_PATH <- obter_env_obrigatoria("IRIS_JAR_PATH")

if (!file.exists(JAR_PATH)) {
    stop(
        "Driver JDBC do IRIS não encontrado em IRIS_JAR_PATH: ",
        JAR_PATH,
        ". Verifique o caminho configurado no .Renviron para este ",
        "servidor/ambiente."
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
# CONSULTA
# =========================================================================
consultar_matricula <- function(matricula_num, distro) {

    con_iris <- conectar_banco(distro)

    on.exit(
        try(dbDisconnect(con_iris), silent = TRUE),
        add = TRUE
    )

    query <- glue("
SELECT TOP 1
    Servidor->Nome AS NOME,
    Servidor->Funcional->DataIngOrgaoFormatada AS DATAINICIO,
    Servidor->Financeiro->DataDesligamento AS DATAFIM,
    ProvDocumento_Tipo->Descricao AS PORTARIA_TIPO,
    ProvDocumento_Numero AS PORTARIA_NUMERO,
    TO_CHAR(ProvDocumento_DataDoc, 'DD/MM/YYYY') AS PORTARIA_DATA,
    ProvDocumento_PublicacaoTipo->Descricao AS DIARIO_TIPO,
    ProvDocumento_PublicacaoNumero AS DIARIO_NUMERO,
    TO_CHAR(ProvDocumento_PublicacaoData, 'DD/MM/YYYY') AS DIARIO_DATA,
    Servidor->Funcional->LotacaoExercicio->Descricao AS LOTACAO,
    Servidor->Matricula AS MATRICULA,
    Servidor->Funcional->LotacaoExercicio->Gestor->Nome AS GESTOR_NOME,
    Servidor->Funcional->LotacaoExercicio->Gestor->Funcional->CargoFuncao->Descricao AS GESTOR_CARGO,
    Servidor->Funcional->LotacaoExercicio->Gestor->Matricula AS GESTOR_MATRICULA
FROM
    RHCadCargoEfetivo
WHERE
    Servidor->MATRICULA = {matricula_num}
ORDER BY
    ProvDocumento_DataDoc DESC
")

    dbGetQuery(con_iris, query)
}

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

        textInput(
            inputId = "matricula",
            label = "Matrícula",
            value = "",
            placeholder = "Somente números"
        ),

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

            /* Cartões de método de acesso (radioButtons estilizado) */

            .metodo-opcoes .radio {
                margin-bottom: .85rem;
            }

            .metodo-opcoes .radio label {
                display: flex;
                align-items: flex-start;
                gap: .85rem;
                width: 100%;
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

            #app-content {
                margin-left: 52px;
                padding: 20px 25px;
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
                '#senha',
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
                '#senha',
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

    # Método escolhido na tela de seleção ("ad" | "totp" | NULL = seletor)
    metodoAcesso <- reactiveVal(NULL)

    # Método efetivamente usado no login bem-sucedido ("AD" | "TOTP")
    metodoAutenticado <- reactiveVal(NULL)

    # Empresa (distro) do usuário autenticado — escolhida manualmente no
    # login AD, ou herdada do cadastro TOTP (ver R/auth_totp.R). É ela
    # que decide qual banco IRIS é consultado em consultar_matricula().
    distroSelecionado <- reactiveVal(NULL)

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

    # ===================================================
    # ABA SELECIONADA
    # ===================================================

    menuSelecionado <- reactiveVal("Certidão")

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

    limpar_estado_certidao <- function() {
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
            menuSelecionado("Certidão")

            # Inicia com as informações do usuário escondidas.
            header_oculto(TRUE)

            updateTabsetPanel(
                session,
                "menu",
                selected = "Certidão"
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
            menuSelecionado("Certidão")

            # Inicia com as informações do usuário escondidas.
            header_oculto(TRUE)

            updateTabsetPanel(
                session,
                "menu",
                selected = "Certidão"
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
        menuSelecionado("Certidão")

        # Dados da certidão são de uma pessoa específica; ao sair, não
        # devem ficar disponíveis para quem fizer login a seguir na
        # mesma aba/sessão do navegador.
        limpar_estado_certidao()

        # Idem para o formulário/chave TOTP recém-gerada (ver
        # mod_totp_admin_server() e o comentário em resetarAdminTotp acima).
        resetarAdminTotp(resetarAdminTotp() + 1)

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

        conteudo_modal <- if (file.exists(README_PATH)) {

            texto_readme <- paste(
                readLines(README_PATH, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n"
            )

            shiny::markdown(texto_readme)

        } else {

            div(
                style = "color:#842029;",
                "Arquivo README.md não encontrado em: ", README_PATH
            )

        }

        showModal(
            modalDialog(
                title = "README",
                conteudo_modal,
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
                                    )

                                ),
                                choiceValues = list("ad", "totp"),
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

                class = "icon-bar",

                actionLink(
                    "toggle_header",
                    icon("id-badge"),
                    class = "icon-btn",
                    title = "Mostrar/ocultar informações do usuário"
                ),

                actionLink(
                    "mostrar_readme",
                    icon("circle-info"),
                    class = "icon-btn",
                    title = "Ver instruções (README)"
                ),

                actionLink(
                    "sair",
                    icon("power-off"),
                    class = "icon-btn sair",
                    title = "Sair"
                )

            ),

            # =================================================
            # CONTEÚDO
            # =================================================

            div(

                id = "app-content",

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
                                tags$span(distroSelecionado(), class = "info-value")
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
                                tags$span(distroSelecionado(), class = "info-value")
                            ),

                            div(
                                class = "info-item",
                                tags$span("Método de acesso", class = "info-label"),
                                tags$span("Código Authenticator (TOTP)", class = "info-value")
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
                        list(
                            nav_panel("Certidão", painel_certidao_ui())
                        ),
                        if (identical(metodoAutenticado(), "AD")) {
                            list(
                                nav_panel("Administração TOTP", mod_totp_admin_ui("totp_admin"))
                            )
                        }
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
                resultado <- tryCatch(
                    {
                        df <- consultar_matricula(matricula_num, distro_atual)
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

                        rmarkdown::render(
                            input = rmd_path_selecionado,
                            output_format = "html_document",
                            output_file = tmp_html,
                            output_dir = render_dir,
                            params = list(
                                matricula = matricula_num,
                                numero_certidao = numero_certidao,
                                logo_path = logoPathAtual(),
                                distro = distroSelecionado()
                            ),
                            envir = new.env(parent = globalenv()),
                            knit_root_dir = APP_DIR,
                            clean = TRUE,
                            quiet = FALSE,
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
            matricula_txt <- trimws(input$matricula)

            tipo_certidao_atual <- input$tipo_certidao
            if (is.null(tipo_certidao_atual)) {
                tipo_certidao_atual <- ""
            }
            tipo_txt <- sanitizar_nome_arquivo(trimws(tipo_certidao_atual))

            sprintf(
                "certidao_%s_%s.html",
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
    # ADMINISTRAÇÃO TOTP (apenas visível/relevante para quem entrou via AD)
    # ---------------------------------------------------------------------
    mod_totp_admin_server(
        "totp_admin",
        con = con,
        ativo = reactive(menuSelecionado() == "Administração TOTP"),
        resetar = resetarAdminTotp
    )
}

shinyApp(ui, server)
