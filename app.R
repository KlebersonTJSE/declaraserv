# =========================================================================
# app.R — Declaraserv | Geração de Certidão MPRO (Formato HTML)
# -----------------------------------------------------------------------------
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
# BUG CORRIGIDO: esta definição precisa acontecer ANTES de library(rJava)
# ser chamado. O rJava lê JAVA_HOME (e, no Windows, ajusta o PATH para
# encontrar jvm.dll) já na inicialização do pacote — se JAVA_HOME só for
# definido depois de library(rJava), o valor configurado no .Renviron
# pode ser ignorado silenciosamente, e o app passa a depender de um
# JAVA_HOME "de sistema" que pode não ser o correto para produção.
# =========================================================================
JAVA_HOME_PATH <- obter_env_obrigatoria("MPRO_JAVA_HOME")
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

# Avisa (sem interromper) se o Active Directory não estiver configurado —
# o app ainda pode ser usado só com login via Authenticator (TOTP).
if (!validar_config_ldap()) {
    warning(
        "Configuração de LDAP/Active Directory incompleta no .Renviron ",
        "(LDAP_SERVER, LDAP_PORT, LDAP_DOMAIN, LDAP_SEARCH_BASE). ",
        "O login corporativo (AD) não funcionará até isso ser configurado; ",
        "o login via Authenticator (TOTP) continua disponível."
    )
}

# =========================================================================
# TIPOS DE CERTIDÃO
# =========================================================================

RMD_DIR <- Sys.getenv("DECLARASERV_RMD_DIR", unset = "~/declaraserv/rmark")

CERTIDAO_TIPOS <- c(
    "Certidão Simples"                  = file.path(RMD_DIR, "certidao_mpro.Rmd"),
    "Certidão de Afastamento Funcional" = file.path(RMD_DIR, "afastamento_funcional_mpro.Rmd"),
    "Certidão Funcional Consolidada"    = file.path(RMD_DIR, "funcional_consolidada_mpro.Rmd"),
    "Certidão de Tempo de Contribuição" = file.path(RMD_DIR, "tempo_contribuicao_mpro.Rmd"),
    "Certidão de Tempo de Serviço"      = file.path(RMD_DIR, "tempo_servico_mpro.Rmd"),
    "Certidão de Vínculo Funcional"     = file.path(RMD_DIR, "vinculo_funcional_mpro.Rmd")
)

# Caminho padrão usado apenas para a checagem inicial de existência do
# template "principal" (Certidão Simples).
RMD_PATH <- CERTIDAO_TIPOS[["Certidão Simples"]]

if (!file.exists(RMD_PATH)) {
    stop("Arquivo RMarkdown não encontrado em: ", RMD_PATH)
}

# Avisa (sem interromper) sobre templates de outras certidões que ainda
# não existam na pasta configurada.
for (tipo_nome in names(CERTIDAO_TIPOS)) {
    caminho_tipo <- CERTIDAO_TIPOS[[tipo_nome]]
    if (!file.exists(caminho_tipo)) {
        warning(
            "Template RMarkdown não encontrado para '", tipo_nome,
            "': ", caminho_tipo,
            ". Essa opção falhará se for selecionada até o arquivo ",
            "ser adicionado."
        )
    }
}

LOGO_PATH <- file.path(ASSETS_DIR, "logo_mpro.png")

if (!file.exists(LOGO_PATH)) {
    warning(
        "Logo não encontrada em: ", LOGO_PATH,
        ". A certidão poderá ser gerada sem o logotipo."
    )
}

# =========================================================================
# CONEXÃO COM O IRIS
# -------------------------------------------------------------------------
# Todas as credenciais e parâmetros de conexão vêm exclusivamente do
# .Renviron. Não há valores padrão hardcoded no código (usuário, senha,
# caminho do .jar, URL ou JAVA_HOME), pois isso mascarava falhas de
# configuração — por exemplo, um JAVA_HOME/JAR_PATH de desenvolvimento
# (Windows) sendo usado silenciosamente em produção e causando
# ClassNotFoundException. A conexão em si é feita por conectar_banco()
# (R/database.R), que lê exatamente as mesmas variáveis validadas aqui.
# =========================================================================
DRIVER_CLASS <- Sys.getenv(
    "MPRO_IRIS_DRIVER_CLASS",
    unset = "com.intersystems.jdbc.IRISDriver"
)

JAR_PATH  <- obter_env_obrigatoria("MPRO_IRIS_JAR_PATH")
IRIS_URL  <- obter_env_obrigatoria("MPRO_IRIS_URL")
IRIS_USER <- obter_env_obrigatoria("MPRO_IRIS_USER")
IRIS_PASS <- obter_env_obrigatoria("MPRO_IRIS_PASS")

if (!file.exists(JAR_PATH)) {
    stop(
        "Driver JDBC do IRIS não encontrado em MPRO_IRIS_JAR_PATH: ",
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
            ativo      INTEGER NOT NULL DEFAULT 1
        )
    ")
}

garantir_schema_login(con)

onStop(function() {
    try(dbDisconnect(con), silent = TRUE)
})

# =========================================================================
# CONSULTA
# =========================================================================
consultar_matricula <- function(matricula_num) {

    con_iris <- conectar_banco()

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
# Cada certidão gerada cria uma pasta nova em tempdir() e nunca a removia;
# numa sessão de Shiny Server de longa duração isso acumulava arquivos
# indefinidamente. Remove pastas "certidao_*" com mais de `max_idade_horas`.
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
                max-height: 260px;
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

    # ===================================================
    # ESTADO DO CABEÇALHO
    # ===================================================

    header_oculto <- reactiveVal(FALSE)

    # ===================================================
    # ABA SELECIONADA
    # ===================================================

    menuSelecionado <- reactiveVal("Certidão MPRO")

    # ===================================================
    # ESTADO DA CERTIDÃO (do app.R original)
    # ===================================================

    html_path <- reactiveVal(NULL)
    dados_consulta <- reactiveVal(NULL)
    matricula_consultada <- reactiveVal(NULL)
    tempo_geracao <- reactiveVal(NULL)

    limpar_estado_certidao <- function() {
        dados_consulta(NULL)
        matricula_consultada(NULL)
        html_path(NULL)
        tempo_geracao(NULL)
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

        dados <- tryCatch(
            authenticate_ad(input$usuario, input$senha),
            error = function(e) {
                showNotification(
                    paste("Erro ao consultar o Active Directory:", conditionMessage(e)),
                    type = "error"
                )
                NULL
            }
        )

        registrar_auditoria(con, input$usuario, "AD", !is.null(dados))

        if (!is.null(dados)) {

            autenticado(TRUE)
            usuarioLogado(input$usuario)
            dadosUsuario(dados)
            fotoUsuario(obter_foto_usuario(dados))
            metodoAutenticado("AD")
            menuSelecionado("Certidão MPRO")

            updateTabsetPanel(
                session,
                "menu",
                selected = "Certidão MPRO"
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

        if (!is.null(dados)) {

            autenticado(TRUE)
            usuarioLogado(dados$login)
            dadosUsuario(dados)
            fotoUsuario(NULL)
            metodoAutenticado("TOTP")
            menuSelecionado("Certidão MPRO")

            updateTabsetPanel(
                session,
                "menu",
                selected = "Certidão MPRO"
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
        menuSelecionado("Certidão MPRO")

        # Dados da certidão são de uma pessoa específica; ao sair, não
        # devem ficar disponíveis para quem fizer login a seguir na
        # mesma aba/sessão do navegador.
        limpar_estado_certidao()

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
                    class = "login-icon-badge mb-3",
                    icon("shield-halved")
                ),

                tags$h4(
                    "Declaraserv",
                    class = "login-title fw-bold mb-1"
                ),

                tags$p(
                    "Acesso ao sistema de emissão de certidões MPRO",
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

                    class = "header-container",

                    div(class = "app-title", h3("Certidão MPRO")),

                    if (identical(metodoAutenticado(), "AD")) {

                        tags$div(

                            style = "color:#555;",

                            if (!is.null(fotoUsuario())) {
                                tags$img(src = fotoUsuario(), class = "foto-usuario mb-2")
                            },

                            tags$b("Usuário: "),
                            obter_campo(dadosUsuario(), "displayName"),
                            br(),

                            tags$b("Departamento: "),
                            obter_campo(dadosUsuario(), "department"),
                            br(),

                            tags$b("Criado em: "),
                            formatar_whenCreated(
                                obter_campo(dadosUsuario(), "whenCreated")
                            ),
                            br(),

                            tags$b("Último acesso: "),
                            formatar_lastLogon(
                                obter_campo(dadosUsuario(), "lastLogonTimestamp")
                            ),
                            br(),

                            tags$b("Gestor: "),
                            extrair_manager(dadosUsuario()$manager),
                            br(),

                            tags$b("Método de acesso: "),
                            "Login Corporativo (AD)"

                        )

                    } else {

                        tags$div(

                            style = "color:#555;",

                            tags$b("Usuário: "),
                            dadosUsuario()$displayName,
                            br(),

                            tags$b("Login: "),
                            dadosUsuario()$login,
                            br(),

                            tags$b("Método de acesso: "),
                            "Código Authenticator (TOTP)"

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
                            nav_panel("Certidão MPRO", painel_certidao_ui())
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
                        df <- consultar_matricula(matricula_num)
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

        tagList(
            selectInput(
                inputId = "tipo_certidao",
                label = "Tipo de certidão",
                choices = names(CERTIDAO_TIPOS),
                selected = names(CERTIDAO_TIPOS)[1]
            ),
            textInput(
                inputId = "numero_certidao",
                label = "Número da certidão",
                placeholder = "Ex.: 61/2026/DGP"
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

        numero_certidao <- input$numero_certidao
        if (is.null(numero_certidao)) {
            numero_certidao <- ""
        }
        numero_certidao <- trimws(numero_certidao)

        if (numero_certidao == "") {
            showModal(
                modalDialog(
                    title = "Número da certidão obrigatório",
                    "Informe o número da certidão (ex.: 61/2026/DGP) antes de gerar.",
                    easyClose = TRUE,
                    footer = modalButton("Fechar")
                )
            )
            return(invisible(NULL))
        }

        rmd_arquivo <- CERTIDAO_TIPOS[[tipo_selecionado]]
        rmd_path_selecionado <- rmd_arquivo

        html_path(NULL)
        tempo_geracao(NULL)

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
                                logo_path = LOGO_PATH
                            ),
                            envir = new.env(parent = globalenv()),
                            knit_root_dir = APP_DIR,
                            clean = TRUE,
                            quiet = FALSE,
                            encoding = "UTF-8"
                        )

                        if (!file.exists(tmp_html)) {
                            stop(
                                "O RMarkdown terminou sem gerar o arquivo HTML: ",
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
                }
            }
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
                "certidao_mpro_%s_%s.html",
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
        ativo = reactive(menuSelecionado() == "Administração TOTP")
    )
}

shinyApp(ui, server)

# # =========================================================================
# # app.R — Geração de Certidão MPRO (Formato HTML)
# # -----------------------------------------------------------------------------
# # Corrige o diretório de trabalho caso o projeto não
# # tenha sido aberto pelo .Rproj
# # =========================================================================
#
# if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
#     try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)), silent = TRUE)
# }
#
# library(here)
# here::i_am("app.R")
#
# readRenviron(here::here(".Renviron"))
#
# library(shiny)
# library(rmarkdown)
# library(rJava)
# library(RJDBC)
# library(glue)
# library(DT)
#
# library(bslib)
# library(DT)
# library(jsonlite)
# library(digest)
#
# library(DBI)
# library(RSQLite)
#
# library(reticulate)
#
# python_path <- Sys.getenv("RETICULATE_PYTHON", unset = Sys.which("python"))
# if (!nzchar(python_path) || !file.exists(python_path)) {
#     stop(
#         "Python não encontrado em '", python_path, "'. ",
#         "Verifique a instalação do Python ou defina RETICULATE_PYTHON no .Renviron ",
#         "apontando para o python.exe correto."
#     )
# }
# use_python(python_path, required = TRUE)
#
# APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
# ASSETS_DIR <- file.path(APP_DIR, "assets")
#
# # =========================================================================
# # TIPOS DE CERTIDÃO
# # =========================================================================
#
# RMD_DIR <- "~/declaraserv/rmark"
#
# CERTIDAO_TIPOS <- c(
#     "Certidão Simples"                  = file.path(RMD_DIR, "certidao_mpro.Rmd"),
#     "Certidão de Afastamento Funcional" = file.path(RMD_DIR, "afastamento_funcional_mpro.Rmd"),
#     "Certidão Funcional Consolidada"    = file.path(RMD_DIR, "funcional_consolidada_mpro.Rmd"),
#     "Certidão de Tempo de Contribuição" = file.path(RMD_DIR, "tempo_contribuicao_mpro.Rmd"),
#     "Certidão de Tempo de Serviço"      = file.path(RMD_DIR, "tempo_servico_mpro.Rmd"),
#     "Certidão de Vínculo Funcional"     = file.path(RMD_DIR, "vinculo_funcional_mpro.Rmd")
# )
#
# # Caminho padrão usado apenas para a checagem inicial de existência do
# # template "principal" (Certidão Simples).
# RMD_PATH <- CERTIDAO_TIPOS[["Certidão Simples"]]
#
# if (!file.exists(RMD_PATH)) {
#     stop("Arquivo RMarkdown não encontrado em: ", RMD_PATH)
# }
#
# # Avisa (sem interromper) sobre templates de outras certidões que ainda
# # não existam na pasta configurada.
# for (tipo_nome in names(CERTIDAO_TIPOS)) {
#     caminho_tipo <- CERTIDAO_TIPOS[[tipo_nome]]
#     if (!file.exists(caminho_tipo)) {
#         warning(
#             "Template RMarkdown não encontrado para '", tipo_nome,
#             "': ", caminho_tipo,
#             ". Essa opção falhará se for selecionada até o arquivo ",
#             "ser adicionado."
#         )
#     }
# }
#
# LOGO_PATH <- file.path(ASSETS_DIR, "logo_mpro.png")
#
# if (!file.exists(LOGO_PATH)) {
#     warning(
#         "Logo não encontrada em: ", LOGO_PATH,
#         ". A certidão poderá ser gerada sem o logotipo."
#     )
# }
#
# # =========================================================================
# # .Renviron — carregamento explícito
# # -------------------------------------------------------------------------
# # O R carrega o .Renviron automaticamente ao iniciar a sessão, mas isso
# # depende de onde a sessão foi iniciada (ex.: Shiny Server, RStudio, Rscript
# # manual). Para não depender desse comportamento implícito, carregamos aqui
# # explicitamente o .Renviron que fica na própria pasta do app.
# # =========================================================================
# RENVIRON_PATH <- file.path(APP_DIR, ".Renviron")
#
# if (file.exists(RENVIRON_PATH)) {
#     readRenviron(RENVIRON_PATH)
# } else {
#     warning(
#         "Arquivo .Renviron não encontrado em: ", RENVIRON_PATH,
#         ". As credenciais de banco precisam estar definidas de outra ",
#         "forma no ambiente (ex.: variáveis de ambiente do serviço)."
#     )
# }
#
# # Lê uma variável de ambiente obrigatória; interrompe a execução com
# # mensagem clara caso ela não esteja definida no .Renviron.
# obter_env_obrigatoria <- function(nome_var) {
#     valor <- Sys.getenv(nome_var, unset = "")
#
#     if (valor == "") {
#         stop(
#             "Variável de ambiente obrigatória não definida: ", nome_var,
#             ". Defina-a no arquivo .Renviron (veja .Renviron.example)."
#         )
#     }
#
#     valor
# }
#
# # =========================================================================
# # CONEXÃO COM O IRIS
# # -------------------------------------------------------------------------
# # Todas as credenciais e parâmetros de conexão vêm exclusivamente do
# # .Renviron. Não há mais valores padrão hardcoded no código (usuário,
# # senha, caminho do .jar, URL ou JAVA_HOME), pois isso mascarava falhas de
# # configuração — por exemplo, um JAVA_HOME/JAR_PATH de desenvolvimento
# # (Windows) sendo usado silenciosamente em produção e causando
# # ClassNotFoundException.
# # =========================================================================
# JAVA_HOME_PATH <- obter_env_obrigatoria("MPRO_JAVA_HOME")
# Sys.setenv(JAVA_HOME = JAVA_HOME_PATH)
#
# # A classe do driver JDBC é a mesma em qualquer ambiente (não é sensível
# # nem específica de máquina), por isso mantém um valor padrão razoável.
# DRIVER_CLASS <- Sys.getenv(
#     "MPRO_IRIS_DRIVER_CLASS",
#     unset = "com.intersystems.jdbc.IRISDriver"
# )
#
# JAR_PATH  <- obter_env_obrigatoria("MPRO_IRIS_JAR_PATH")
# IRIS_URL  <- obter_env_obrigatoria("MPRO_IRIS_URL")
# IRIS_USER <- obter_env_obrigatoria("MPRO_IRIS_USER")
# IRIS_PASS <- obter_env_obrigatoria("MPRO_IRIS_PASS")
#
# if (!file.exists(JAR_PATH)) {
#     stop(
#         "Driver JDBC do IRIS não encontrado em MPRO_IRIS_JAR_PATH: ",
#         JAR_PATH,
#         ". Verifique o caminho configurado no .Renviron para este ",
#         "servidor/ambiente."
#     )
# }
#
# # =========================================================================
# # CONSULTA
# # =========================================================================
# consultar_matricula <- function(matricula_num) {
#
#     drv <- JDBC(
#         driverClass = DRIVER_CLASS,
#         classPath = JAR_PATH
#     )
#
#     con <- dbConnect(
#         drv,
#         IRIS_URL,
#         user = IRIS_USER,
#         password = IRIS_PASS
#     )
#
#     on.exit(
#         try(dbDisconnect(con), silent = TRUE),
#         add = TRUE
#     )
#
#     query <- glue("
# SELECT TOP 1
#     Servidor->Nome AS NOME,
#     Servidor->Funcional->DataIngOrgaoFormatada AS DATAINICIO,
#     Servidor->Financeiro->DataDesligamento AS DATAFIM,
#     ProvDocumento_Tipo->Descricao AS PORTARIA_TIPO,
#     ProvDocumento_Numero AS PORTARIA_NUMERO,
#     TO_CHAR(ProvDocumento_DataDoc, 'DD/MM/YYYY') AS PORTARIA_DATA,
#     ProvDocumento_PublicacaoTipo->Descricao AS DIARIO_TIPO,
#     ProvDocumento_PublicacaoNumero AS DIARIO_NUMERO,
#     TO_CHAR(ProvDocumento_PublicacaoData, 'DD/MM/YYYY') AS DIARIO_DATA,
#     Servidor->Funcional->LotacaoExercicio->Descricao AS LOTACAO,
#     Servidor->Matricula AS MATRICULA,
#     Servidor->Funcional->LotacaoExercicio->Gestor->Nome AS GESTOR_NOME,
#     Servidor->Funcional->LotacaoExercicio->Gestor->Funcional->CargoFuncao->Descricao AS GESTOR_CARGO,
#     Servidor->Funcional->LotacaoExercicio->Gestor->Matricula AS GESTOR_MATRICULA
# FROM
#     RHCadCargoEfetivo
# WHERE
#     Servidor->MATRICULA = {matricula_num}
# ORDER BY
#     ProvDocumento_DataDoc DESC
# ")
#
#     dbGetQuery(con, query)
# }
#
# # =========================================================================
# # INTERFACE
# # =========================================================================
# ui <- fluidPage(
#
#     tags$head(
#         tags$style(HTML("
#             .app-title {
#                 margin-top: 20px;
#                 margin-bottom: 4px;
#                 font-weight: 600;
#             }
#
#             .app-subtitle {
#                 color: #6c6c6c;
#                 margin-bottom: 24px;
#             }
#
#             .painel {
#                 max-width: 720px;
#                 margin: 0 auto;
#                 padding: 24px;
#             }
#
#             .tabela-consulta {
#                 margin-top: 12px;
#                 margin-bottom: 12px;
#             }
#         "))
#     ),
#
#     div(
#         class = "painel",
#
#         div(
#             class = "app-title",
#             h3("Certidão MPRO")
#         ),
#
#         div(
#             class = "app-subtitle",
#             "Informe a matrícula do(a) residente para gerar a certidão."
#         ),
#
#         textInput(
#             inputId = "matricula",
#             label = "Matrícula",
#             value = "",
#             placeholder = "Somente números"
#         ),
#
#         tags$script(HTML("
#             $(document).on('input', '#matricula', function() {
#                 var valorLimpo = $(this).val().replace(/[^0-9]/g, '');
#
#                 if ($(this).val() !== valorLimpo) {
#                     $(this).val(valorLimpo);
#                 }
#             });
#         ")),
#
#         br(),
#
#         actionButton(
#             "consultar",
#             "Consultar",
#             class = "btn-primary"
#         ),
#
#         br(),
#         br(),
#
#         uiOutput("resultado_consulta"),
#
#         br(),
#
#         uiOutput("selecao_certidao"),
#
#         br(),
#
#         uiOutput("acao_gerar"),
#
#         br(),
#
#         uiOutput("tempo_processamento"),
#
#         br(),
#
#         uiOutput("download_ui"),
#
#         br(),
#
#         div(
#             id = "status_msg",
#             style = "color:#6c6c6c;"
#         )
#     )
# )
#
# # =========================================================================
# # SERVIDOR
# # =========================================================================
# server <- function(input, output, session) {
#
#     html_path <- reactiveVal(NULL)
#     dados_consulta <- reactiveVal(NULL)
#     matricula_consultada <- reactiveVal(NULL)
#     tempo_geracao <- reactiveVal(NULL)
#
#     validar_matricula <- function(matricula_txt) {
#
#         if (matricula_txt == "") {
#             showModal(
#                 modalDialog(
#                     title = "Matrícula obrigatória",
#                     "Informe a matrícula do(a) residente antes de consultar.",
#                     easyClose = TRUE,
#                     footer = modalButton("Fechar")
#                 )
#             )
#             return(NULL)
#         }
#
#         if (!grepl("^[0-9]+$", matricula_txt)) {
#             showModal(
#                 modalDialog(
#                     title = "Matrícula inválida",
#                     "A matrícula deve conter apenas números.",
#                     easyClose = TRUE,
#                     footer = modalButton("Fechar")
#                 )
#             )
#             return(NULL)
#         }
#
#         valor <- suppressWarnings(as.numeric(matricula_txt))
#
#         if (is.na(valor) || valor > .Machine$integer.max) {
#             showModal(
#                 modalDialog(
#                     title = "Matrícula inválida",
#                     "A matrícula informada não é válida.",
#                     easyClose = TRUE,
#                     footer = modalButton("Fechar")
#                 )
#             )
#             return(NULL)
#         }
#
#         as.integer(valor)
#     }
#
#     # Quando a matrícula muda, os dados anteriores deixam de ser válidos.
#     observeEvent(input$matricula, {
#         dados_consulta(NULL)
#         matricula_consultada(NULL)
#         html_path(NULL)
#         tempo_geracao(NULL)
#     }, ignoreInit = TRUE)
#
#     # ---------------------------------------------------------------------
#     # CONSULTA
#     # ---------------------------------------------------------------------
#     observeEvent(input$consultar, {
#
#         matricula_txt <- trimws(input$matricula)
#         matricula_num <- validar_matricula(matricula_txt)
#
#         if (is.null(matricula_num)) {
#             return(invisible(NULL))
#         }
#
#         dados_consulta(NULL)
#         html_path(NULL)
#
#         withProgress(
#             message = "Consultando dados...",
#             value = 0.3,
#             {
#                 resultado <- tryCatch(
#                     {
#                         df <- consultar_matricula(matricula_num)
#                         incProgress(0.7)
#                         df
#                     },
#                     error = function(e) {
#                         showModal(
#                             modalDialog(
#                                 title = "Erro na consulta",
#                                 paste0(
#                                     "Não foi possível consultar a matrícula ",
#                                     matricula_num,
#                                     ".\n\nDetalhe técnico: ",
#                                     conditionMessage(e)
#                                 ),
#                                 easyClose = TRUE,
#                                 footer = modalButton("Fechar")
#                             )
#                         )
#                         NULL
#                     }
#                 )
#
#                 if (!is.null(resultado) && nrow(resultado) == 0) {
#                     showModal(
#                         modalDialog(
#                             title = "Matrícula não encontrada",
#                             paste0(
#                                 "Nenhum registro encontrado para a matrícula ",
#                                 matricula_num,
#                                 "."
#                             ),
#                             easyClose = TRUE,
#                             footer = modalButton("Fechar")
#                         )
#                     )
#                     resultado <- NULL
#                 }
#
#                 if (!is.null(resultado)) {
#                     dados_consulta(resultado)
#                     matricula_consultada(matricula_num)
#                 }
#             }
#         )
#     })
#
#     output$resultado_consulta <- renderUI({
#         req(dados_consulta())
#
#         div(
#             class = "tabela-consulta",
#             h5("Dados encontrados:"),
#             DTOutput("tabela_dados")
#         )
#     })
#
#     output$tabela_dados <- renderDT({
#         req(dados_consulta())
#
#         datatable(
#             dados_consulta(),
#             options = list(
#                 dom = "t",
#                 scrollX = TRUE,
#                 paging = FALSE,
#                 searching = FALSE
#             ),
#             rownames = FALSE
#         )
#     })
#
#     output$selecao_certidao <- renderUI({
#         req(dados_consulta())
#
#         selectInput(
#             inputId = "tipo_certidao",
#             label = "Tipo de certidão",
#             choices = names(CERTIDAO_TIPOS),
#             selected = names(CERTIDAO_TIPOS)[1]
#         )
#     })
#
#     output$acao_gerar <- renderUI({
#         req(dados_consulta())
#
#         actionButton(
#             "gerar",
#             "Gerar certidão",
#             class = "btn-success"
#         )
#     })
#
#     # ---------------------------------------------------------------------
#     # GERAÇÃO DO HTML
#     # ---------------------------------------------------------------------
#     observeEvent(input$gerar, {
#
#         matricula_num <- matricula_consultada()
#         req(matricula_num)
#
#         tipo_selecionado <- input$tipo_certidao
#         req(tipo_selecionado)
#
#         rmd_arquivo <- CERTIDAO_TIPOS[[tipo_selecionado]]
#         rmd_path_selecionado <- rmd_arquivo
#
#         html_path(NULL)
#         tempo_geracao(NULL)
#
#         if (!file.exists(rmd_path_selecionado)) {
#             showModal(
#                 modalDialog(
#                     title = "Template não encontrado",
#                     paste0(
#                         "O arquivo RMarkdown para '", tipo_selecionado,
#                         "' não foi encontrado em: ", rmd_path_selecionado
#                     ),
#                     easyClose = TRUE,
#                     footer = modalButton("Fechar")
#                 )
#             )
#             return(invisible(NULL))
#         }
#
#         # Marca o início do processamento para medir quanto tempo a
#         # geração da certidão selecionada leva.
#         inicio_processamento <- Sys.time()
#
#         withProgress(
#             message = paste0("Gerando ", tipo_selecionado, "..."),
#             value = 0.1,
#             {
#                 saida <- tryCatch(
#                     {
#                         render_dir <- file.path(
#                             tempdir(),
#                             paste0(
#                                 "certidao_",
#                                 matricula_num,
#                                 "_",
#                                 format(Sys.time(), "%Y%m%d%H%M%S")
#                             )
#                         )
#
#                         dir.create(
#                             render_dir,
#                             recursive = TRUE,
#                             showWarnings = FALSE
#                         )
#
#                         tmp_html <- file.path(
#                             render_dir,
#                             sprintf("certidao_%s.html", matricula_num)
#                         )
#
#                         incProgress(
#                             0.2,
#                             detail = "Preparando documento..."
#                         )
#
#                         rmarkdown::render(
#                             input = rmd_path_selecionado,
#                             output_format = "html_document",
#                             output_file = tmp_html,
#                             output_dir = render_dir,
#                             params = list(
#                                 matricula = matricula_num,
#                                 numero_certidao = "61/2026/DGP",
#                                 logo_path = LOGO_PATH
#                             ),
#                             envir = new.env(parent = globalenv()),
#                             knit_root_dir = APP_DIR,
#                             clean = TRUE,
#                             quiet = FALSE,
#                             encoding = "UTF-8"
#                         )
#
#                         if (!file.exists(tmp_html)) {
#                             stop(
#                                 "O RMarkdown terminou sem gerar o arquivo HTML: ",
#                                 tmp_html
#                             )
#                         }
#
#                         incProgress(
#                             0.8,
#                             detail = "Certidão gerada com sucesso."
#                         )
#
#                         tmp_html
#                     },
#
#                     error = function(e) {
#                         detalhe <- conditionMessage(e)
#
#                         showModal(
#                             modalDialog(
#                                 title = "Não foi possível gerar a certidão",
#                                 div(
#                                     style = paste(
#                                         "white-space: pre-wrap;",
#                                         "font-family: monospace;",
#                                         "font-size: 12px;",
#                                         "max-height: 500px;",
#                                         "overflow-y: auto;"
#                                     ),
#                                     paste0(
#                                         "A matrícula ",
#                                         matricula_num,
#                                         " foi consultada com sucesso, ",
#                                         "mas ocorreu um erro durante a ",
#                                         "geração da ",
#                                         tipo_selecionado,
#                                         ".\n\n",
#                                         "Detalhe técnico:\n",
#                                         detalhe
#                                     )
#                                 ),
#                                 easyClose = TRUE,
#                                 size = "l",
#                                 footer = modalButton("Fechar")
#                             )
#                         )
#
#                         NULL
#                     }
#                 )
#
#                 # Calcula o tempo total de processamento, independente de
#                 # sucesso ou falha na geração.
#                 duracao_segundos <- as.numeric(
#                     difftime(Sys.time(), inicio_processamento, units = "secs")
#                 )
#                 tempo_geracao(duracao_segundos)
#
#                 if (!is.null(saida)) {
#                     html_path(saida)
#                 }
#             }
#         )
#     })
#
#     # ---------------------------------------------------------------------
#     # TEMPO DE PROCESSAMENTO
#     # ---------------------------------------------------------------------
#     output$tempo_processamento <- renderUI({
#         req(tempo_geracao())
#
#         tags$div(
#             style = "color:#6c6c6c; font-size: 13px;",
#             sprintf(
#                 "Tempo de processamento: %.2f segundos.",
#                 tempo_geracao()
#             )
#         )
#     })
#
#     # ---------------------------------------------------------------------
#     # DOWNLOAD
#     # ---------------------------------------------------------------------
#     output$download_ui <- renderUI({
#         req(html_path())
#
#         downloadButton(
#             "baixar",
#             "Baixar certidão (HTML)",
#             class = "btn-success"
#         )
#     })
#
#     output$baixar <- downloadHandler(
#
#         filename = function() {
#             #matricula_txt <- trimws(input$matricula)
#             tipo_txt <- trimws(input$tipo_certidao)
#
#             sprintf(
#                 "certidao_mpro_%s.html",
#                 tipo_txt
#                 #matricula_txt
#             )
#         },
#
#         content = function(file) {
#             req(html_path())
#
#             if (!file.exists(html_path())) {
#                 stop("O arquivo HTML não está mais disponível.")
#             }
#
#             ok <- file.copy(
#                 html_path(),
#                 file,
#                 overwrite = TRUE
#             )
#
#             if (!ok) {
#                 stop("Não foi possível copiar o HTML para o download.")
#             }
#         },
#
#         contentType = "text/html"
#     )
# }
#
# shinyApp(ui, server)
