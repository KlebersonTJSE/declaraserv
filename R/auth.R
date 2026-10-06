# =====================================================
# R/auth.R
# Autenticação via Active Directory (LDAP), usando o
# pacote Python 'ldap3' através do reticulate.
#
# Todas as configurações sensíveis (servidor, porta,
# domínio, base de busca) vêm do .Renviron.
#
# Pacotes (reticulate, jsonlite) são carregados no app.R e aqui
# usados com "pacote::" — sem library() repetido.
# =====================================================

# =====================================================
# PYTHON / LDAP3 — IMPORTAÇÃO SOB DEMANDA (com cache)
# -------------------------------------------------------------------------
# O Python é configurado em R/python_env.R (configurar_python(), chamado
# no app.R ANTES do source() deste arquivo). O módulo 'ldap3' só é
# importado na PRIMEIRA tentativa de login AD e fica em cache — assim:
#   - o app sobe mesmo se o ldap3 estiver ausente (TOTP continua ok);
#   - não há custo de importação para quem só usa TOTP;
#   - o erro, se houver, é mostrado de forma clara na tela de login.
# =====================================================

.ldap_cache <- new.env(parent = emptyenv())

obter_ldap3 <- function() {

    if (!is.null(.ldap_cache$ldap3)) {
        return(.ldap_cache$ldap3)
    }

    if (exists("PYTHON_STATUS", inherits = TRUE) && !isTRUE(PYTHON_STATUS$ok)) {
        stop(PYTHON_STATUS$mensagem, call. = FALSE)
    }

    .ldap_cache$ldap3 <- reticulate::import("ldap3")
    .ldap_cache$conv  <- reticulate::import("ldap3.utils.conv")
    .ldap_cache$ldap3
}

# Atributos realmente usados pelo app (cabeçalho + foto). Buscar só estes,
# em vez de ALL_ATTRIBUTES, reduz bastante o tráfego e o tempo do login.
LDAP_ATRIBUTOS <- c(
    "displayName", "department", "whenCreated", "lastLogonTimestamp",
    "manager", "thumbnailPhoto", "mail", "sAMAccountName"
)

# =====================================================
# CONFIGURAÇÕES LDAP (MULTI-EMPRESA)
# -------------------------------------------------------------------------
# {DISTRO}_LDAP_SERVER, {DISTRO}_LDAP_PORT, {DISTRO}_LDAP_DOMAIN,
# {DISTRO}_LDAP_SEARCH_BASE — ex.: TJSE_LDAP_SERVER, MPRO_LDAP_SERVER.
# =====================================================

obter_config_ldap <- function(distro) {

    list(
        server = Sys.getenv(distro_env(distro, "LDAP_SERVER")),
        port   = suppressWarnings(as.integer(Sys.getenv(distro_env(distro, "LDAP_PORT")))),
        domain = Sys.getenv(distro_env(distro, "LDAP_DOMAIN")),
        base   = Sys.getenv(distro_env(distro, "LDAP_SEARCH_BASE"))
    )
}

# Aceita o código da empresa (como antes) ou uma config já lida por
# obter_config_ldap() — evita ler o .Renviron duas vezes no login.
validar_config_ldap <- function(distro_ou_cfg) {

    cfg <- if (is.list(distro_ou_cfg)) {
        distro_ou_cfg
    } else {
        if (is.null(distro_ou_cfg) || trimws(distro_ou_cfg) == "") return(FALSE)
        obter_config_ldap(distro_ou_cfg)
    }

    campos <- c(cfg$server, cfg$domain, cfg$base)
    all(!is.na(campos) & campos != "") && !is.na(cfg$port)
}

# =====================================================
# AUTENTICAÇÃO ACTIVE DIRECTORY
# =====================================================

authenticate_ad <- function(usuario, senha, distro) {

    if (is.null(distro) || trimws(distro) == "") {
        stop("Empresa não informada para o login AD.")
    }

    cfg <- obter_config_ldap(distro)

    if (!validar_config_ldap(cfg)) {
        stop(
            "Configuração LDAP não encontrada ou incompleta para a empresa '",
            distro, "'. Verifique ", distro_env(distro, "LDAP_SERVER"), ", ",
            distro_env(distro, "LDAP_PORT"), ", ", distro_env(distro, "LDAP_DOMAIN"),
            " e ", distro_env(distro, "LDAP_SEARCH_BASE"), " no .Renviron."
        )
    }

    if (is.null(usuario) || is.null(senha)) {
        return(NULL)
    }

    usuario <- trimws(usuario)

    # A senha NÃO é aparada (trimws): espaços podem fazer parte dela.
    # Senha vazia precisa ser barrada aqui — o AD aceita "bind" sem senha
    # como anônimo, o que daria um falso positivo de autenticação.
    if (usuario == "" || senha == "") {
        return(NULL)
    }

    ldap3 <- obter_ldap3()

    servidor <- ldap3$Server(
        cfg$server,
        port = cfg$port,
        use_ssl = TRUE,
        get_info = ldap3$NONE,
        connect_timeout = 5L
    )

    conexao <- ldap3$Connection(
        servidor,
        user = paste0(usuario, "@", cfg$domain),
        password = senha,
        auto_bind = FALSE,
        receive_timeout = 10L
    )

    on.exit(try(conexao$unbind(), silent = TRUE), add = TRUE)

    if (!isTRUE(tryCatch(conexao$bind(), error = function(e) FALSE))) {
        return(NULL)
    }

    tryCatch(
        {
            # Escapa caracteres especiais (*, (, ), \ ...) para evitar
            # injeção no filtro LDAP.
            usuario_esc <- .ldap_cache$conv$escape_filter_chars(usuario)

            conexao$search(
                search_base   = cfg$base,
                search_filter = paste0("(sAMAccountName=", usuario_esc, ")"),
                attributes    = as.list(LDAP_ATRIBUTOS),
                size_limit    = 1L
            )

            entradas <- conexao$entries

            if (length(entradas) == 0) {
                return(NULL)
            }

            reticulate::py_to_r(entradas[[1]]$entry_attributes_as_dict)
        },
        error = function(e) NULL
    )
}

# =====================================================
# OBTÉM FOTO DO USUÁRIO
# =====================================================

obter_foto_usuario <- function(dados_usuario) {

    if (is.null(dados_usuario) || is.null(dados_usuario$thumbnailPhoto) ||
        length(dados_usuario$thumbnailPhoto) == 0) {
        return(NULL)
    }

    tryCatch(
        {
            bytes <- as.raw(unlist(dados_usuario$thumbnailPhoto))

            paste0("data:image/jpeg;base64,", jsonlite::base64_enc(bytes))
        },
        error = function(e) NULL
    )
}

# =====================================================
# TESTE DE CONFIGURAÇÃO LDAP
# -----------------------------------------------------
# Mantido por compatibilidade. Observação: ldap3$Server() NÃO abre
# conexão de rede — isto só valida a configuração/objeto do servidor.
# =====================================================

testar_ldap <- function(distro) {

    cfg <- obter_config_ldap(distro)

    tryCatch(
        {
            ldap3 <- obter_ldap3()
            ldap3$Server(cfg$server, port = cfg$port, use_ssl = TRUE,
                         get_info = ldap3$NONE)
            TRUE
        },
        error = function(e) FALSE
    )
}

# =====================================================
# OBTÉM NOME COMPLETO / EMAIL / LOGIN / DEPARTAMENTO
# =====================================================

.primeiro_valor <- function(dados_usuario, campo) {
    valor <- dados_usuario[[campo]]
    if (is.null(valor) || length(valor) == 0) "" else as.character(valor[[1]])
}

obter_nome_usuario         <- function(d) .primeiro_valor(d, "displayName")
obter_email_usuario        <- function(d) .primeiro_valor(d, "mail")
obter_login_usuario        <- function(d) .primeiro_valor(d, "sAMAccountName")
obter_departamento_usuario <- function(d) .primeiro_valor(d, "department")
