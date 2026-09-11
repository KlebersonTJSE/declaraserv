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
                "Auditoria - Tabela",

                p(
                    class = "text-muted",
                    style = "font-size: 13px; margin-top: 10px;",
                    "Histórico de tentativas de login, corporativo (AD) e ",
                    "via Authenticator (TOTP), bem-sucedidas ou não."
                ),

                fluidRow(
                    column(
                        4,
                        selectInput(
                            ns("filtro_periodo"),
                            "Filtrar por período do dia",
                            choices = c("Todos", "Manhã", "Tarde", "Noite"),
                            selected = "Todos"
                        )
                    )
                ),

                uiOutput(ns("resumo_periodos")),

                DTOutput(ns("tabela_auditoria"))
            ),

            nav_panel(
                "Auditoria - Gráfico",

                p(
                    class = "text-muted",
                    style = "font-size: 13px; margin-top: 10px;",
                    "Use o controle deslizante abaixo para escolher a data ",
                    "inicial e final do período considerado nos gráficos."
                ),

                uiOutput(ns("slider_periodo_grafico_ui")),

                fluidRow(
                    column(
                        6,
                        h5("Acessos por usuário", style = "margin-top: 20px;"),
                        plotOutput(ns("grafico_auditoria_usuarios"), height = "420px")
                    ),
                    column(
                        6,
                        h5("Acessos por empresa", style = "margin-top: 20px;"),
                        plotOutput(ns("grafico_auditoria_empresas"), height = "420px")
                    )
                ),

                fluidRow(
                    column(
                        6,
                        h5("Acessos por método", style = "margin-top: 24px;"),
                        plotOutput(ns("grafico_auditoria_metodo"), height = "420px")
                    ),
                    column(
                        6,
                        h5("Acessos por período do dia", style = "margin-top: 24px;"),
                        plotOutput(ns("grafico_auditoria_periodo"), height = "420px")
                    )
                )
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
        ),

        uiOutput(ns("tempo_processamento_bd"))
    )
}

mod_totp_admin_server <- function(id, con, ativo, resetar = reactiveVal(0)) {

    moduleServer(id, function(input, output, session) {

        ns <- session$ns

        atualizar_tabela <- reactiveVal(0)
        ultimo_cadastro <- reactiveVal(NULL)

        # Tempo da última operação de banco (qualquer uma) — exibido no
        # rodapé da tela (ver output$tempo_processamento_bd, no final do
        # arquivo). registrar_tempo_bd() é chamada logo após cada consulta
        # ou gravação no banco feita por este módulo.
        tempoUltimoProcessamento <- reactiveVal(NULL)

        registrar_tempo_bd <- function(descricao, inicio) {
            tempoUltimoProcessamento(list(
                descricao = descricao,
                segundos = as.numeric(difftime(Sys.time(), inicio, units = "secs"))
            ))
        }

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
                {
                    t0 <- Sys.time()
                    r <- cadastrar_usuario_totp(con, login, nome, distro)
                    registrar_tempo_bd("Cadastro de usuário TOTP", t0)
                    r
                },
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
                    t0 <- Sys.time()
                    desativar_usuario_totp(con, login)
                    registrar_tempo_bd("Desativação de usuário TOTP", t0)
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

            t0 <- Sys.time()
            dados <- listar_usuarios_totp(con)
            registrar_tempo_bd("Consulta de usuários TOTP cadastrados", t0)

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
        # -----------------------------------------------------------------
        # O período considerado vem do controle deslizante (definido na
        # aba "Auditoria - Gráfico") SÓ quando o filtro está ativado
        # (input$usar_filtro_periodo); caso contrário, usa todo o
        # histórico gravado no banco. Isso é o que faz a aba
        # "Auditoria - Tabela" mostrar dados assim que a pessoa loga, sem
        # precisar visitar a aba "Auditoria - Gráfico" antes: como o
        # slider é um uiOutput de uma aba escondida, o Shiny suspende sua
        # renderização (e não define input$periodo_grafico) enquanto ela
        # não for visitada — então, por padrão (filtro desativado), a
        # consulta abaixo nunca depende dele.
        #
        # Cada acesso é classificado em Manhã (06h–11h59), Tarde
        # (12h–17h59) ou Noite (demais horários, cobrindo a madrugada
        # também).
        # ---------------------------------------------------------------
        classificar_periodo <- function(hora) {
            ifelse(
                hora >= 6 & hora < 12, "Manhã",
                ifelse(hora >= 12 & hora < 18, "Tarde", "Noite")
            )
        }

        # Menor/maior data já registrada em login_auditoria — consulta
        # leve, usada tanto para dimensionar o slider quanto como período
        # padrão quando o filtro está desativado.
        intervaloAuditoria <- reactive({
            req(ativo())

            t0 <- Sys.time()
            intervalo <- obter_intervalo_auditoria(con)
            registrar_tempo_bd("Consulta do intervalo de datas da auditoria", t0)

            intervalo
        })

        output$slider_periodo_grafico_ui <- renderUI({

            intervalo <- intervaloAuditoria()
            req(nrow(intervalo) > 0, !is.na(intervalo$minimo[1]))

            data_min_real <- as.Date(intervalo$minimo[1])
            data_max_real <- as.Date(intervalo$maximo[1])

            # O slider sempre cobre pelo menos 180 dias, mesmo que o
            # histórico gravado seja mais curto que isso.
            data_min_slider <- min(data_min_real, data_max_real - 180)

            tagList(
                checkboxInput(
                    ns("usar_filtro_periodo"),
                    "Usar filtro de período (caso desmarcado, considera todo o histórico gravado no banco)",
                    value = FALSE
                ),
                sliderInput(
                    ns("periodo_grafico"),
                    "Data inicial e final",
                    min = data_min_slider,
                    max = data_max_real,
                    value = c(max(data_min_slider, data_max_real - 180), data_max_real),
                    timeFormat = "%d/%m/%Y"
                )
            )
        })

        # Dados do período considerado (filtro ativo → slider; filtro
        # inativo → todo o histórico) — buscados direto do banco já
        # filtrados por data (ver listar_auditoria_login()).
        dadosAuditoriaPeriodo <- reactive({

            req(ativo())

            intervalo <- intervaloAuditoria()
            req(nrow(intervalo) > 0, !is.na(intervalo$minimo[1]))

            if (isTRUE(input$usar_filtro_periodo)) {
                req(input$periodo_grafico)
                data_inicio <- input$periodo_grafico[1]
                data_fim <- input$periodo_grafico[2]
            } else {
                data_inicio <- as.Date(intervalo$minimo[1])
                data_fim <- as.Date(intervalo$maximo[1])
            }

            t0 <- Sys.time()
            dados <- listar_auditoria_login(
                con,
                data_inicio = data_inicio,
                data_fim = data_fim
            )
            registrar_tempo_bd("Consulta de auditoria de login", t0)

            if (nrow(dados) > 0) {
                dados$datahora_dt <- as.POSIXct(dados$datahora, tz = "America/Maceio")
                dados$periodo <- classificar_periodo(
                    as.integer(format(dados$datahora_dt, "%H"))
                )
            }

            dados
        })

        # -- Tabela (com filtro por período do dia) ----------------------

        output$resumo_periodos <- renderUI({

            dados <- dadosAuditoriaPeriodo()
            req(nrow(dados) > 0)

            niveis <- c("Manhã", "Tarde", "Noite")
            contagem <- table(factor(dados$periodo, levels = niveis))
            periodo_top <- names(contagem)[which.max(contagem)]

            intervalo_exibido <- range(as.Date(dados$datahora_dt))

            tags$div(
                style = "font-size: 13px; color:#555; margin: 4px 0 14px 0;",
                tags$b("Período considerado: "),
                paste0(
                    format(intervalo_exibido[1], "%d/%m/%Y"), " a ",
                    format(intervalo_exibido[2], "%d/%m/%Y"),
                    if (isTRUE(input$usar_filtro_periodo)) {
                        " (filtro ativo — ajustável na aba \"Auditoria - Gráfico\")"
                    } else {
                        " (todo o histórico — ative o filtro na aba \"Auditoria - Gráfico\" para restringir)"
                    }
                ),
                tags$br(),
                tags$b("Acessos por período: "),
                paste0(
                    "Manhã: ", contagem[["Manhã"]],
                    " · Tarde: ", contagem[["Tarde"]],
                    " · Noite: ", contagem[["Noite"]]
                ),
                tags$br(),
                tags$b("Período com mais acessos: "), periodo_top
            )
        })

        output$tabela_auditoria <- renderDT({

            dados <- dadosAuditoriaPeriodo()

            if (nrow(dados) > 0) {

                if (!is.null(input$filtro_periodo) && input$filtro_periodo != "Todos") {
                    dados <- dados[dados$periodo == input$filtro_periodo, ]
                }

                dados$sucesso <- ifelse(dados$sucesso == 1, "Sim", "Não")
                dados <- dados[, c("id", "login", "empresa", "metodo", "sucesso", "datahora", "periodo")]
                names(dados)[names(dados) == "empresa"] <- "empresa"
                names(dados)[names(dados) == "periodo"] <- "período"
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

        # -- Gráficos (acessos por usuário e por empresa, no período) ----

        output$grafico_auditoria_usuarios <- renderPlot({

            dados <- dadosAuditoriaPeriodo()

            if (nrow(dados) == 0) {
                plot.new()
                text(0.5, 0.5, "Nenhum acesso no período selecionado.")
                return(invisible(NULL))
            }

            contagem <- sort(table(dados$login), decreasing = TRUE)
            cores <- grDevices::hcl.colors(length(contagem), palette = "Set2")

            barplot(
                contagem,
                las = 2,
                col = cores,
                main = "Quantidade de acessos por usuário",
                ylab = "Acessos",
                cex.names = 0.85
            )
        })

        output$grafico_auditoria_empresas <- renderPlot({

            dados <- dadosAuditoriaPeriodo()

            if (nrow(dados) == 0) {
                plot.new()
                text(0.5, 0.5, "Nenhum acesso no período selecionado.")
                return(invisible(NULL))
            }

            contagem <- sort(table(dados$empresa), decreasing = TRUE)
            cores <- grDevices::hcl.colors(length(contagem), palette = "Set2")

            pie(
                contagem,
                col = cores,
                labels = paste0(names(contagem), " (", contagem, ")"),
                main = "Acessos por empresa"
            )
        })

        # Acessos por método (AD x TOTP) — barplot HORIZONTAL, um modelo
        # diferente do barplot vertical simples usado em "por usuário".
        output$grafico_auditoria_metodo <- renderPlot({

            dados <- dadosAuditoriaPeriodo()

            if (nrow(dados) == 0) {
                plot.new()
                text(0.5, 0.5, "Nenhum acesso no período selecionado.")
                return(invisible(NULL))
            }

            # "AD:TJSE", "AD:MPRO" etc. viram só "AD" aqui — a quebra por
            # empresa já é mostrada no gráfico de pizza ao lado.
            metodo_base <- sub(":.*$", "", dados$metodo)
            contagem <- sort(table(metodo_base), decreasing = TRUE)
            cores <- grDevices::hcl.colors(length(contagem), palette = "Viridis")

            barplot(
                contagem,
                horiz = TRUE,
                las = 1,
                col = cores,
                main = "Quantidade de acessos por método",
                xlab = "Acessos"
            )
        })

        # Acessos por período do dia, comparando sucesso x falha — barplot
        # AGRUPADO (duas barras lado a lado por período), outro modelo
        # diferente dos dois já usados acima.
        output$grafico_auditoria_periodo <- renderPlot({

            dados <- dadosAuditoriaPeriodo()

            if (nrow(dados) == 0) {
                plot.new()
                text(0.5, 0.5, "Nenhum acesso no período selecionado.")
                return(invisible(NULL))
            }

            niveis <- c("Manhã", "Tarde", "Noite")
            resultado <- ifelse(dados$sucesso == 1, "Sim", "Não")

            tab <- table(
                factor(resultado, levels = c("Sim", "Não")),
                factor(dados$periodo, levels = niveis)
            )

            cores <- grDevices::hcl.colors(2, palette = "Dark 3")

            barplot(
                tab,
                beside = TRUE,
                col = cores,
                main = "Acessos por período do dia (sucesso x falha)",
                ylab = "Acessos",
                legend.text = rownames(tab),
                args.legend = list(x = "topright", bty = "n", cex = 0.85)
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
            tempoUltimoProcessamento(NULL)
        }, ignoreInit = TRUE)

        # ---------------------------------------------------------------
        # TEMPO DE PROCESSAMENTO (rodapé)
        # -----------------------------------------------------------------
        # Mostra a duração da última operação de banco feita por este
        # módulo (consulta ou gravação), qualquer que ela tenha sido —
        # ver registrar_tempo_bd(), chamada logo após cada uma delas.
        # ---------------------------------------------------------------
        output$tempo_processamento_bd <- renderUI({

            info <- tempoUltimoProcessamento()
            req(info)

            tags$div(
                style = paste(
                    "color:#6c6c6c; font-size: 12px; text-align: right;",
                    "margin-top: 24px; padding-top: 8px; border-top: 1px solid #eee;"
                ),
                sprintf(
                    "%s: %.3f s",
                    info$descricao,
                    info$segundos
                )
            )
        })
    })
}
