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
                textInput(ns("login"), "Login", placeholder = "usuario.ad")
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
                    choices = listar_distros()
                )
            ),
            column(
                3,
                br(),
                actionButton(
                    ns("cadastrar"),
                    "Gerar / recadastrar chave",
                    class = "btn-primary"
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

        h5("Usuários cadastrados"),

        DTOutput(ns("tabela_totp"))
    )
}

mod_totp_admin_server <- function(id, con, ativo) {

    moduleServer(id, function(input, output, session) {

        atualizar_tabela <- reactiveVal(0)
        ultimo_cadastro <- reactiveVal(NULL)

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

            datatable(
                listar_usuarios_totp(con),
                rownames = FALSE,
                options = list(dom = "tp", pageLength = 10)
            )
        })
    })
}
