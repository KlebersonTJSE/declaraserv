library(shiny)
library(rmarkdown)
library(DBI)
library(rJava)
library(RJDBC)
library(glue)
library(DT)

# =========================================================================
# app.R — Geração de Certidão MPRO (Formato HTML)
# =========================================================================
APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
ASSETS_DIR <- file.path(APP_DIR, "assets")

# =========================================================================
# TIPOS DE CERTIDÃO
# -------------------------------------------------------------------------
# Agora os arquivos RMarkdown são buscados na pasta fixa
# D:/Projetos R/certidoes_mpro/rmark
# =========================================================================
# RMD_DIR <- "D:/Projetos R/certidoes_mpro/rmark"
RMD_DIR <- "~/Projetos R/certidoes_mpro/rmark"

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
# .Renviron — carregamento explícito
# -------------------------------------------------------------------------
# O R carrega o .Renviron automaticamente ao iniciar a sessão, mas isso
# depende de onde a sessão foi iniciada (ex.: Shiny Server, RStudio, Rscript
# manual). Para não depender desse comportamento implícito, carregamos aqui
# explicitamente o .Renviron que fica na própria pasta do app.
# =========================================================================
RENVIRON_PATH <- file.path(APP_DIR, ".Renviron")

if (file.exists(RENVIRON_PATH)) {
    readRenviron(RENVIRON_PATH)
} else {
    warning(
        "Arquivo .Renviron não encontrado em: ", RENVIRON_PATH,
        ". As credenciais de banco precisam estar definidas de outra ",
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
# CONEXÃO COM O IRIS
# -------------------------------------------------------------------------
# Todas as credenciais e parâmetros de conexão vêm exclusivamente do
# .Renviron. Não há mais valores padrão hardcoded no código (usuário,
# senha, caminho do .jar, URL ou JAVA_HOME), pois isso mascarava falhas de
# configuração — por exemplo, um JAVA_HOME/JAR_PATH de desenvolvimento
# (Windows) sendo usado silenciosamente em produção e causando
# ClassNotFoundException.
# =========================================================================
JAVA_HOME_PATH <- obter_env_obrigatoria("MPRO_JAVA_HOME")
Sys.setenv(JAVA_HOME = JAVA_HOME_PATH)

# A classe do driver JDBC é a mesma em qualquer ambiente (não é sensível
# nem específica de máquina), por isso mantém um valor padrão razoável.
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
# CONSULTA
# =========================================================================
consultar_matricula <- function(matricula_num) {

    drv <- JDBC(
        driverClass = DRIVER_CLASS,
        classPath = JAR_PATH
    )

    con <- dbConnect(
        drv,
        IRIS_URL,
        user = IRIS_USER,
        password = IRIS_PASS
    )

    on.exit(
        try(dbDisconnect(con), silent = TRUE),
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

    dbGetQuery(con, query)
}

# =========================================================================
# INTERFACE
# =========================================================================
ui <- fluidPage(

    tags$head(
        tags$style(HTML("
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
        "))
    ),

    div(
        class = "painel",

        div(
            class = "app-title",
            h3("Certidão MPRO")
        ),

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
)

# =========================================================================
# SERVIDOR
# =========================================================================
server <- function(input, output, session) {

    html_path <- reactiveVal(NULL)
    dados_consulta <- reactiveVal(NULL)
    matricula_consultada <- reactiveVal(NULL)
    tempo_geracao <- reactiveVal(NULL)

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

        if (is.na(valor) || valor > .Machine$integer.max) {
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
        dados_consulta(NULL)
        matricula_consultada(NULL)
        html_path(NULL)
        tempo_geracao(NULL)
    }, ignoreInit = TRUE)

    # ---------------------------------------------------------------------
    # CONSULTA
    # ---------------------------------------------------------------------
    observeEvent(input$consultar, {

        matricula_txt <- trimws(input$matricula)
        matricula_num <- validar_matricula(matricula_txt)

        if (is.null(matricula_num)) {
            return(invisible(NULL))
        }

        dados_consulta(NULL)
        html_path(NULL)

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

        selectInput(
            inputId = "tipo_certidao",
            label = "Tipo de certidão",
            choices = names(CERTIDAO_TIPOS),
            selected = names(CERTIDAO_TIPOS)[1]
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
                                numero_certidao = "61/2026/DGP",
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
            #matricula_txt <- trimws(input$matricula)
            tipo_txt <- trimws(input$tipo_certidao)

            sprintf(
                "certidao_mpro_%s.html",
                tipo_txt
                #matricula_txt
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
}

shinyApp(ui, server)
