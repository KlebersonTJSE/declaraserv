# =====================================================
# R/database.R
# -----------------------------------------------------
# Conexão com o IRIS — MULTI-EMPRESA.
#
# Cada empresa listada em DISTRO_1, DISTRO_2... no .Renviron (ver
# listar_distros() em R/utils.R) tem sua própria URL/usuário/senha de
# banco, prefixados pelo código da empresa:
#   {DISTRO}_IRIS_URL
#   {DISTRO}_IRIS_USER
#   {DISTRO}_IRIS_PASSWORD
#
# O driver JDBC (classe + .jar) é normalmente o MESMO para todas as
# empresas (variáveis compartilhadas IRIS_DRIVER_CLASS / IRIS_JAR_PATH),
# mas cada empresa pode sobrescrever isso individualmente definindo
# {DISTRO}_IRIS_DRIVER_CLASS / {DISTRO}_IRIS_JAR_PATH no .Renviron, caso
# um dia seja necessário (ex.: versão diferente do driver).
#
# Adicionar uma nova empresa não exige tocar neste arquivo: basta incluir
# DISTRO_3=NOVAEMPRESA e as variáveis NOVAEMPRESA_IRIS_* no .Renviron.
# =====================================================

library(DBI)
library(RJDBC)

conectar_banco <- function(distro) {

    if (is.null(distro) || trimws(distro) == "") {
        stop(
            "Nenhuma empresa (distro) informada para conectar_banco(). ",
            "Verifique se o login selecionou uma empresa válida."
        )
    }

    distro <- toupper(trimws(distro))

    driver_class <- Sys.getenv(
        distro_env(distro, "IRIS_DRIVER_CLASS"),
        unset = Sys.getenv(
            "IRIS_DRIVER_CLASS",
            unset = "com.intersystems.jdbc.IRISDriver"
        )
    )

    jar_path <- Sys.getenv(
        distro_env(distro, "IRIS_JAR_PATH"),
        unset = Sys.getenv("IRIS_JAR_PATH", unset = "")
    )

    url      <- Sys.getenv(distro_env(distro, "IRIS_URL"), unset = "")
    user     <- Sys.getenv(distro_env(distro, "IRIS_USER"), unset = "")
    password <- Sys.getenv(distro_env(distro, "IRIS_PASSWORD"), unset = "")

    if (url == "" || user == "" || password == "") {
        stop(
            "Configuração de banco incompleta para a empresa '", distro,
            "'. Verifique ", distro_env(distro, "IRIS_URL"), ", ",
            distro_env(distro, "IRIS_USER"), " e ",
            distro_env(distro, "IRIS_PASSWORD"), " no .Renviron."
        )
    }

    if (jar_path == "" || !file.exists(jar_path)) {
        stop(
            "Driver JDBC do IRIS não encontrado para a empresa '", distro,
            "' em: '", jar_path, "'. Verifique IRIS_JAR_PATH (ou ",
            distro_env(distro, "IRIS_JAR_PATH"), ") no .Renviron."
        )
    }

    drv <- JDBC(
        driverClass = driver_class,
        classPath = jar_path
    )

    dbConnect(
        drv,
        url,
        user = user,
        password = password
    )
}
