# =====================================================
# modules/mod_totp_admin.R
# -----------------------------------------------------
# Administração dos usuários habilitados a entrar via
# código do Authenticator (TOTP), sem depender do AD.
#
# Só é exibido para quem entrou via AD (ver app.R), já
# que permite conceder/revogar acesso de outras pessoas.
# =====================================================

mod_totp_admin_ui <- function(id) {

    ns <- NS(id)

    tagList(
        h4("Administração de acesso via Authenticator (TOTP)"),

        p(
            class = "text-muted",
            "Cadastre um usuário para permitir login via código do ",
            "Authenticator, sem depender do Active Directory. Gerar uma ",
            "nova chave para um login já existente substitui a chave ",
            "anterior."
        ),

        fluidRow(
            column(
                3,
                textInput(ns("login"), "Login")
            ),
            column(
                3,
                textInput(ns("nome"), "Nome de exibição")
            ),
            column(
                3,
                selectInput(
                    ns("distro"),
                    "Empresa",
                    choices = c(
                        "Selecione uma empresa" = "",
                        listar_distros()
                    ),
                    selected = ""
                )
            ),
            column(
                3,
                br(),
                div(
                    class = "d-flex gap-2 flex-wrap",
                    actionButton(
                        ns("cadastrar"),
                        "Gerar / recadastrar chave",
                        class = "btn-primary"
                    ),
                    actionButton(
                        ns("limpar_campos"),
                        "Limpar campos",
                        class = "btn-outline-secondary"
                    )
                )
            )
        ),

        uiOutput(ns("resultado_cadastro")),

        hr(),

        fluidRow(
            column(
                4,
                textInput(ns("login_desativar"), "Login a desativar")
            ),
            column(
                4,
                br(),
                actionButton(
                    ns("desativar"),
                    "Desativar acesso TOTP",
                    class = "btn-outline-danger"
                )
            )
        ),

        hr(),

        navset_tab(
            id = ns("subtab_totp"),

            nav_panel(
                "Usuários cadastrados",

                p(
                    class = "text-muted",
                    style = "font-size: 13px; margin-top: 10px;",
                    "Clique em um usuário na tabela para carregar seus dados ",
                    "nos campos acima (inclusive no campo \"Login a desativar\")."
                ),

                DTOutput(ns("tabela_totp"))
            ),

            nav_panel(
                "Auditoria",

                p(
                    class = "text-muted",
                    style = "font-size: 13px; margin-top: 10px;",
                    "Histórico de tentativas de login, corporativo (AD) e ",
                    "via Authenticator (TOTP), bem-sucedidas ou não."
                ),

                DTOutput(ns("tabela_auditoria"))
            ),

            nav_panel(
                "Validação",

                p(
                    class = "text-muted",
                    style = "font-size: 13px; margin-top: 10px;",
                    "Informe o número da certidão (ou apenas o código de ",
                    "11 dígitos) para conferir a data e hora exatas em ",
                    "que ela foi gerada."
                ),

                fluidRow(
                    column(
                        4,
                        textInput(
                            ns("codigo_validacao"),
                            "Número da certidão",
                            placeholder = "Ex.: 00123456789/2026/DIGEPE"
                        )
                    ),
                    column(
                        4,
                        br(),
                        actionButton(
                            ns("validar_codigo"),
                            "Validar",
                            class = "btn-primary"
                        )
                    )
                ),

                uiOutput(ns("resultado_validacao"))
            )
        )
    )
}

mod_totp_admin_server <- function(id, con, ativo, resetar = reactiveVal(0)) {

    moduleServer(id, function(input, output, session) {

        atualizar_tabela <- reactiveVal(0)
        ultimo_cadastro <- reactiveVal(NULL)

        # Guarda o data.frame atualmente exibido na tabela, para que o
        # clique em uma linha (input$tabela_totp_rows_selected) consiga
        # recuperar os dados daquele usuário e preencher o formulário.
        dadosTotpAtual <- reactiveVal(NULL)

        # Proxy da tabela, usado para desmarcar a linha selecionada quando
        # o formulário é limpo (ver input$limpar_campos abaixo).
        proxy_tabela_totp <- DT::dataTableProxy("tabela_totp")

        observeEvent(input$cadastrar, {

            login  <- trimws(input$login)
            nome   <- trimws(input$nome)
            distro <- input$distro

            if (login == "" || nome == "") {
                showNotification(
                    "Informe login e nome antes de gerar a chave.",
                    type = "warning"
                )
                return(invisible(NULL))
            }

            if (is.null(distro) || trimws(distro) == "" || !(distro %in% listar_distros())) {
                showNotification(
                    "Selecione uma empresa válida antes de gerar a chave.",
                    type = "warning"
                )
                return(invisible(NULL))
            }

            resultado <- tryCatch(
                cadastrar_usuario_totp(con, login, nome, distro),
                error = function(e) {
                    showNotification(
                        paste("Erro ao cadastrar:", conditionMessage(e)),
                        type = "error"
                    )
                    NULL
                }
            )

            if (!is.null(resultado)) {
                ultimo_cadastro(resultado)
                atualizar_tabela(atualizar_tabela() + 1)
                showNotification(
                    paste0("Chave TOTP gerada para '", login, "' (empresa: ", distro, ")."),
                    type = "message"
                )
            }
        })

        observeEvent(input$desativar, {

            login <- trimws(input$login_desativar)

            if (login == "") {
                showNotification(
                    "Informe o login a ser desativado.",
                    type = "warning"
                )
                return(invisible(NULL))
            }

            tryCatch(
                {
                    desativar_usuario_totp(con, login)
                    atualizar_tabela(atualizar_tabela() + 1)
                    showNotification(
                        paste0("Acesso TOTP de '", login, "' desativado."),
                        type = "message"
                    )
                },
                error = function(e) {
                    showNotification(
                        paste("Erro ao desativar:", conditionMessage(e)),
                        type = "error"
                    )
                }
            )
        })

        output$resultado_cadastro <- renderUI({

            req(ultimo_cadastro())
            r <- ultimo_cadastro()

            qr <- tryCatch(gerar_qrcode_base64(r$uri), error = function(e) NULL)

            div(
                class = "alert alert-success",

                tags$b("Chave gerada para: "), r$login,
                " (empresa: ", r$distro, ")", tags$br(),

                tags$b("Chave secreta (entrada manual): "),
                tags$code(r$secret),
                tags$br(),

                if (!is.null(qr)) {
                    tags$img(src = qr, width = 180, style = "margin-top:10px;")
                } else {
                    tags$em(
                        "Instale o pacote 'qrcode' para exibir o QR Code; ",
                        "por ora, use a chave secreta acima para cadastro ",
                        "manual no Authenticator."
                    )
                }
            )
        })

        output$tabela_totp <- renderDT({

            req(ativo())
            atualizar_tabela()

            dados <- listar_usuarios_totp(con)
            dadosTotpAtual(dados)

            datatable(
                dados,
                rownames = FALSE,
                selection = "single",
                options = list(dom = "tp", pageLength = 10)
            )
        })

        # ---------------------------------------------------------------
        # AUDITORIA DE LOGIN
        # ---------------------------------------------------------------
        output$tabela_auditoria <- renderDT({

            req(ativo())

            dados <- listar_auditoria_login(con)

            if (nrow(dados) > 0 && "sucesso" %in% names(dados)) {
                dados$sucesso <- ifelse(dados$sucesso == 1, "Sim", "Não")
            }

            datatable(
                dados,
                rownames = FALSE,
                selection = "none",
                options = list(
                    dom = "tp",
                    pageLength = 10,
                    order = list()
                )
            )
        })

        # ---------------------------------------------------------------
        # VALIDAÇÃO DE CERTIDÃO
        # -----------------------------------------------------------------
        # Reverte o código de 11 dígitos gerado por gerar_codigo_tempo11()
        # (ver app.R) de volta para a data/hora exata em que a certidão
        # foi gerada — é o "inverso" daquela função: soma o código (em
        # segundos) à mesma data-base (01/01/2025, horário de Maceió).
        # ---------------------------------------------------------------
        recuperar_data_hora11 <- function(codigo) {
            data_base <- as.POSIXct(
                "2025-01-01 00:00:00",
                tz = "America/Maceio"
            )
            data_base + as.numeric(codigo)
        }

        resultado_validacao <- reactiveVal(NULL)

        observeEvent(input$validar_codigo, {

            entrada <- trimws(input$codigo_validacao)

            if (entrada == "") {
                showNotification(
                    "Informe o número da certidão antes de validar.",
                    type = "warning"
                )
                resultado_validacao(NULL)
                return(invisible(NULL))
            }

            # Aceita tanto o número completo ("00123456789/2026/DIGEPE")
            # quanto apenas o código de 11 dígitos — usa só a parte antes
            # da primeira barra, se houver.
            codigo <- trimws(strsplit(entrada, "/", fixed = TRUE)[[1]][1])

            if (!grepl("^[0-9]{11}$", codigo)) {
                showNotification(
                    paste(
                        "Código inválido. Informe o número da certidão",
                        "completo ou apenas o código de 11 dígitos."
                    ),
                    type = "error"
                )
                resultado_validacao(NULL)
                return(invisible(NULL))
            }

            data_gerada <- tryCatch(
                recuperar_data_hora11(codigo),
                error = function(e) NULL
            )

            if (is.null(data_gerada) || is.na(data_gerada)) {
                showNotification(
                    "Não foi possível calcular a data para esse código.",
                    type = "error"
                )
                resultado_validacao(NULL)
                return(invisible(NULL))
            }

            resultado_validacao(data_gerada)

        }, ignoreInit = TRUE)

        output$resultado_validacao <- renderUI({
            req(resultado_validacao())

            div(
                class = "alert alert-success",
                style = "margin-top: 16px;",
                tags$b("Certidão gerada em: "),
                format(
                    resultado_validacao(),
                    "%d/%m/%Y %H:%M:%S",
                    tz = "America/Maceio"
                )
            )
        })

        # ---------------------------------------------------------------
        # CLIQUE EM UM USUÁRIO CADASTRADO
        # -----------------------------------------------------------------
        # Preenche Login, Nome de exibição, Empresa e "Login a desativar"
        # com os dados da linha clicada na tabela.
        # ---------------------------------------------------------------
        observeEvent(input$tabela_totp_rows_selected, {

            idx <- input$tabela_totp_rows_selected
            req(idx)

            dados <- dadosTotpAtual()
            req(dados)

            linha <- dados[idx, , drop = FALSE]

            login_valor <- if ("login" %in% names(linha)) {
                as.character(linha$login[1])
            } else {
                as.character(linha[[1]][1])
            }

            nome_valor <- if ("nome" %in% names(linha)) {
                as.character(linha$nome[1])
            } else {
                as.character(linha[[2]][1])
            }

            distro_valor <- if ("distro" %in% names(linha)) {
                as.character(linha$distro[1])
            } else {
                as.character(linha[[3]][1])
            }

            updateTextInput(session, "login", value = login_valor)
            updateTextInput(session, "nome", value = nome_valor)

            if (distro_valor %in% listar_distros()) {
                updateSelectInput(
                    session,
                    "distro",
                    selected = distro_valor
                )
            }

            updateTextInput(session, "login_desativar", value = login_valor)

        }, ignoreInit = TRUE)

        # ---------------------------------------------------------------
        # LIMPAR CAMPOS
        # -----------------------------------------------------------------
        # Limpa Login, Nome de exibição, Empresa e "Login a desativar", e
        # desmarca a linha selecionada na tabela.
        # ---------------------------------------------------------------
        limpar_campos_formulario <- function() {

            updateTextInput(session, "login", value = "")
            updateTextInput(session, "nome", value = "")
            updateSelectInput(session, "distro", selected = "")
            updateTextInput(session, "login_desativar", value = "")

            DT::selectRows(proxy_tabela_totp, NULL)
        }

        observeEvent(input$limpar_campos, {
            limpar_campos_formulario()
        }, ignoreInit = TRUE)

        # ---------------------------------------------------------------
        # RESET NO LOGOUT
        # -----------------------------------------------------------------
        # O módulo é iniciado uma única vez por sessão do navegador (não
        # é recriado a cada login), então, sem isso, a chave recém-gerada
        # (com o secret em texto puro) e os campos do formulário ficariam
        # visíveis para a próxima pessoa que fizer login na mesma sessão.
        # `resetar` é incrementado em app.R dentro de observeEvent(input$sair).
        # ---------------------------------------------------------------
        observeEvent(resetar(), {
            limpar_campos_formulario()
            ultimo_cadastro(NULL)
            updateTextInput(session, "codigo_validacao", value = "")
            resultado_validacao(NULL)
        }, ignoreInit = TRUE)
    })
}
