# =====================================================
# R/utils.R
# -----------------------------------------------------
# Funções utilitárias genéricas usadas pela tela de login
# e pelo cabeçalho do usuário autenticado.
# =====================================================

# -----------------------------------------------------
# Extrai um campo de uma lista de atributos LDAP (ou de
# qualquer lista nomeada), tratando ausência/valor vazio.
# -----------------------------------------------------
obter_campo <- function(lista, nome) {

    valor <- lista[[nome]]

    if (is.null(valor)) {
        return("")
    }

    if (length(valor) == 0) {
        return("")
    }

    paste(as.character(valor), collapse = "; ")
}

# -----------------------------------------------------
# Extrai o nome (CN) a partir do DN completo do atributo
# "manager" do Active Directory.
# -----------------------------------------------------
extrair_manager <- function(manager) {

    if (is.null(manager)) {
        return("")
    }

    if (length(manager) == 0) {
        return("")
    }

    manager <- as.character(manager)[1]

    sub("^CN=([^,]+),.*$", "\\1", manager)
}

# -----------------------------------------------------
# Formata o atributo "whenCreated" do AD
# (formato: AAAAMMDDHHMMSS.0Z) para dd/mm/aaaa hh:mm:ss.
# -----------------------------------------------------
formatar_whenCreated <- function(x) {

    if (is.null(x) || length(x) == 0) {
        return("")
    }

    x <- as.character(x)[1]

    if (is.na(x) || x == "") {
        return("")
    }

    dt <- tryCatch(
        as.POSIXct(
            gsub("\\.0Z$", "", x),
            format = "%Y%m%d%H%M%S",
            tz = "UTC"
        ),
        error = function(e) NA
    )

    if (is.na(dt)) {
        return(x)
    }

    format(dt, "%d/%m/%Y %H:%M:%S")
}

# -----------------------------------------------------
# Formata o atributo "lastLogonTimestamp" do AD
# (Windows FILETIME: nº de intervalos de 100ns desde
# 1601-01-01) para dd/mm/aaaa hh:mm:ss.
# -----------------------------------------------------
formatar_lastLogon <- function(x) {

    if (is.null(x) || length(x) == 0) {
        return("Nunca acessou")
    }

    x <- as.character(x)[1]

    if (is.na(x) || x == "" || x == "0") {
        return("Nunca acessou")
    }

    valor_numerico <- suppressWarnings(as.numeric(x))

    if (is.na(valor_numerico)) {
        return("Nunca acessou")
    }

    format(
        as.POSIXct(
            (valor_numerico / 10000000) - 11644473600,
            origin = "1970-01-01",
            tz = "UTC"
        ),
        "%d/%m/%Y %H:%M:%S"
    )
}

# -----------------------------------------------------
# MULTI-EMPRESA (DISTRO)
# -------------------------------------------------------------------------
# Lê a lista de empresas/unidades configuradas no .Renviron como
# DISTRO_1, DISTRO_2, DISTRO_3... (sob o comentário "DISTRO ENTERPRISE").
# Não há limite fixo: a leitura para assim que encontra o primeiro
# "DISTRO_N" não definido. Cada valor retornado (ex.: "TJSE", "MPRO") é
# usado como prefixo das demais variáveis daquela empresa no .Renviron
# (ex.: TJSE_LDAP_SERVER, TJSE_IRIS_URL, MPRO_LDAP_SERVER, MPRO_IRIS_URL),
# tanto em R/auth.R (Active Directory) quanto em R/database.R (IRIS).
#
# Adicionar uma nova empresa é só: DISTRO_3=NOVAEMPRESA no .Renviron, mais
# as variáveis NOVAEMPRESA_LDAP_* e NOVAEMPRESA_IRIS_* — nenhum código
# precisa mudar.
# -----------------------------------------------------
listar_distros <- function() {

    distros <- character(0)
    i <- 1

    repeat {
        valor <- trimws(Sys.getenv(paste0("DISTRO_", i), unset = ""))

        if (valor == "") {
            break
        }

        distros <- c(distros, valor)
        i <- i + 1
    }

    distros
}

# -----------------------------------------------------
# Nome da variável de ambiente de uma empresa específica,
# ex.: distro_env("TJSE", "LDAP_SERVER") -> "TJSE_LDAP_SERVER".
# -----------------------------------------------------
distro_env <- function(distro, sufixo) {
    paste0(toupper(trimws(distro)), "_", sufixo)
}
