# =====================================================
# R/python_env.R
# -----------------------------------------------------
# Configuração do Python usado pelo reticulate (login AD via 'ldap3').
#
# Regras:
#   1. O interpretador vem de RETICULATE_PYTHON (.Renviron) ou, na falta
#      dele, do primeiro "python" encontrado no PATH.
#   2. Se o Python já tiver sido inicializado nesta sessão do R com OUTRO
#      interpretador (ex.: RStudio abriu o Python antes), avisamos — o
#      reticulate não troca de interpretador depois de inicializado.
#   3. Os módulos exigidos são verificados UMA vez na inicialização. Se
#      faltar algum:
#        - com PY_AUTO_INSTALL=TRUE no .Renviron, tenta instalar via pip
#          no MESMO interpretador;
#        - caso contrário (ou se a instalação falhar), o app SOBE mesmo
#          assim: o login por Authenticator (TOTP) continua funcionando e
#          apenas o login AD fica indisponível, com mensagem clara.
# =====================================================

PY_MODULOS_OBRIGATORIOS <- c("ldap3")

# Estado global, preenchido por configurar_python().
PYTHON_STATUS <- new.env(parent = emptyenv())
PYTHON_STATUS$ok       <- FALSE
PYTHON_STATUS$python   <- NA_character_
PYTHON_STATUS$faltando <- PY_MODULOS_OBRIGATORIOS
PYTHON_STATUS$mensagem <- "Python ainda não configurado."

.normalizar_caminho <- function(p) {
    normalizePath(p, winslash = "/", mustWork = FALSE)
}

.instalar_modulos_pip <- function(python_exe, modulos) {
    message("Instalando módulo(s) Python ausente(s) via pip: ",
            paste(modulos, collapse = ", "), " em ", python_exe)
    status <- tryCatch(
        system2(python_exe, c("-m", "pip", "install", "--disable-pip-version-check", modulos)),
        error = function(e) 1L
    )
    identical(as.integer(status), 0L)
}

configurar_python <- function(modulos = PY_MODULOS_OBRIGATORIOS) {

    python_path <- Sys.getenv("RETICULATE_PYTHON", unset = "")
    if (!nzchar(python_path)) {
        python_path <- unname(Sys.which("python"))
    }

    if (!nzchar(python_path) || !file.exists(python_path)) {
        PYTHON_STATUS$mensagem <- paste0(
            "Python não encontrado ('", python_path, "'). Defina ",
            "RETICULATE_PYTHON no .Renviron apontando para o python.exe correto."
        )
        warning(PYTHON_STATUS$mensagem, call. = FALSE)
        return(invisible(FALSE))
    }

    # Python já inicializado antes do app (RStudio, outro script etc.)?
    ja_inicializado <- reticulate::py_available(initialize = FALSE)

    if (!ja_inicializado) {
        reticulate::use_python(python_path, required = TRUE)
    }

    py_em_uso <- .normalizar_caminho(reticulate::py_config()$python)
    PYTHON_STATUS$python <- py_em_uso

    if (!identical(.normalizar_caminho(python_path), py_em_uso)) {
        warning(
            "O reticulate já estava usando '", py_em_uso, "' (diferente de '",
            python_path, "', configurado em RETICULATE_PYTHON). Reinicie a ",
            "sessão do R (Ctrl+Shift+F10) antes de executar o app.",
            call. = FALSE
        )
    }

    verificar <- function() {
        modulos[!vapply(modulos, reticulate::py_module_available, logical(1))]
    }

    faltando <- verificar()

    if (length(faltando) > 0 &&
        isTRUE(as.logical(Sys.getenv("PY_AUTO_INSTALL", "FALSE")))) {
        if (.instalar_modulos_pip(py_em_uso, faltando)) {
            faltando <- verificar()
        }
    }

    PYTHON_STATUS$faltando <- faltando
    PYTHON_STATUS$ok       <- length(faltando) == 0

    PYTHON_STATUS$mensagem <- if (PYTHON_STATUS$ok) {
        paste0("Python OK: ", py_em_uso)
    } else {
        paste0(
            "Módulo(s) Python ausente(s): ", paste(faltando, collapse = ", "),
            ". Python em uso: ", py_em_uso, ". Instale com:  \"", py_em_uso,
            "\" -m pip install ", paste(faltando, collapse = " "),
            "  — e reinicie o app. Enquanto isso, o login AD fica ",
            "indisponível (o login por Authenticator continua funcionando)."
        )
    }

    if (!PYTHON_STATUS$ok) {
        warning(PYTHON_STATUS$mensagem, call. = FALSE)
    } else {
        message(PYTHON_STATUS$mensagem)
    }

    invisible(PYTHON_STATUS$ok)
}
