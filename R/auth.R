# =====================================================
# R/auth.R
# Autenticação via Active Directory (LDAP), usando o
# pacote Python 'ldap3' através do reticulate.
#
# Todas as configurações sensíveis (servidor, porta,
# domínio, base de busca) vêm do .Renviron.
# =====================================================

library(reticulate)
library(jsonlite)

# =====================================================
# PYTHON
# =====================================================

python_path <- Sys.getenv("RETICULATE_PYTHON", unset = Sys.which("python"))
if (!nzchar(python_path) || !file.exists(python_path)) {
    stop(
        "Python não encontrado em '", python_path, "'. ",
        "Verifique a instalação do Python ou defina RETICULATE_PYTHON no .Renviron ",
        "apontando para o python.exe correto."
    )
}
use_python(python_path, required = TRUE)

ldap3 <- import("ldap3")

# =====================================================
# CONFIGURAÇÕES LDAP E VALIDAÇÃO DINÂMICA (MULTI-EMPRESA)
# -------------------------------------------------------------------------
# Cada empresa listada em DISTRO_1, DISTRO_2... (ver listar_distros() em
# R/utils.R) tem sua própria configuração de LDAP no .Renviron, prefixada
# pelo código da empresa: {DISTRO}_LDAP_SERVER, {DISTRO}_LDAP_PORT,
# {DISTRO}_LDAP_DOMAIN, {DISTRO}_LDAP_SEARCH_BASE — ex.: TJSE_LDAP_SERVER,
# MPRO_LDAP_SERVER.
# =====================================================

obter_config_ldap <- function(distro) {

    list(
        server = Sys.getenv(distro_env(distro, "LDAP_SERVER")),
        port   = suppressWarnings(as.integer(Sys.getenv(distro_env(distro, "LDAP_PORT")))),
        domain = Sys.getenv(distro_env(distro, "LDAP_DOMAIN")),
        base   = Sys.getenv(distro_env(distro, "LDAP_SEARCH_BASE"))
    )
}

validar_config_ldap <- function(distro) {

    if (is.null(distro) || trimws(distro) == "") {
        return(FALSE)
    }

    cfg <- obter_config_ldap(distro)

    campos <- c(cfg$server, cfg$domain, cfg$base)

    all(!is.na(campos) & campos != "") && !is.na(cfg$port)
}

# =====================================================
# AUTENTICAÇÃO ACTIVE DIRECTORY
# =====================================================

authenticate_ad <- function(usuario, senha, distro) {

    if (!validar_config_ldap(distro)) {
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
    senha   <- trimws(senha)

    if (usuario == "" || senha == "") {
        return(NULL)
    }

    cfg <- obter_config_ldap(distro)

    servidor <- ldap3$Server(
        cfg$server,
        port = cfg$port,
        use_ssl = TRUE,
        get_info = ldap3$NONE
    )

    usuario_ad <- paste0(usuario, "@", cfg$domain)

    conexao <- ldap3$Connection(
        servidor,
        user = usuario_ad,
        password = senha,
        auto_bind = FALSE
    )

    autenticado <- tryCatch(
        conexao$bind(),
        error = function(e) FALSE
    )

    if (!isTRUE(autenticado)) {
        return(NULL)
    }

    resultado <- tryCatch(
        {
            filtro <- paste0("(sAMAccountName=", usuario, ")")

            conexao$search(
                search_base = cfg$base,
                search_filter = filtro,
                attributes = ldap3$ALL_ATTRIBUTES
            )

            if (length(conexao$entries) == 0) {
                conexao$unbind()
                return(NULL)
            }

            entry <- conexao$entries[[1]]

            atributos <- py_to_r(entry$entry_attributes_as_dict)

            conexao$unbind()

            atributos
        },
        error = function(e) {
            try(conexao$unbind(), silent = TRUE)
            NULL
        }
    )

    resultado
}

# =====================================================
# OBTÉM FOTO DO USUÁRIO
# =====================================================

obter_foto_usuario <- function(dados_usuario) {

    if (is.null(dados_usuario)) {
        return(NULL)
    }

    if (is.null(dados_usuario$thumbnailPhoto)) {
        return(NULL)
    }

    tryCatch(
        {
            bytes <- as.raw(unlist(dados_usuario$thumbnailPhoto))

            paste0(
                "data:image/jpeg;base64,",
                jsonlite::base64_enc(bytes)
            )
        },
        error = function(e) NULL
    )
}

# =====================================================
# TESTE DE CONECTIVIDADE LDAP
# =====================================================

testar_ldap <- function(distro) {

    cfg <- obter_config_ldap(distro)

    tryCatch(
        {
            ldap3$Server(
                cfg$server,
                port = cfg$port,
                use_ssl = TRUE,
                get_info = ldap3$NONE
            )

            TRUE
        },
        error = function(e) FALSE
    )
}

# =====================================================
# OBTÉM NOME COMPLETO / EMAIL / LOGIN / DEPARTAMENTO
# =====================================================

obter_nome_usuario <- function(dados_usuario) {
    if (is.null(dados_usuario) || is.null(dados_usuario$displayName)) {
        return("")
    }
    as.character(dados_usuario$displayName[[1]])
}

obter_email_usuario <- function(dados_usuario) {
    if (is.null(dados_usuario) || is.null(dados_usuario$mail)) {
        return("")
    }
    as.character(dados_usuario$mail[[1]])
}

obter_login_usuario <- function(dados_usuario) {
    if (is.null(dados_usuario) || is.null(dados_usuario$sAMAccountName)) {
        return("")
    }
    as.character(dados_usuario$sAMAccountName[[1]])
}

obter_departamento_usuario <- function(dados_usuario) {
    if (is.null(dados_usuario) || is.null(dados_usuario$department)) {
        return("")
    }
    as.character(dados_usuario$department[[1]])
}
