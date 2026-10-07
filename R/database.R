# =====================================================
# R/database.R
# -----------------------------------------------------
# Conexão com o IRIS — MULTI-EMPRESA.
#
# Cada empresa listada em DISTRO_1, DISTRO_2... no .Renviron (ver
# listar_distros() em R/utils.R) tem sua própria URL/usuário/senha:
#   {DISTRO}_IRIS_URL / {DISTRO}_IRIS_USER / {DISTRO}_IRIS_PASSWORD
#
# O driver JDBC (classe + .jar) é normalmente o MESMO para todas as
# empresas (IRIS_DRIVER_CLASS / IRIS_JAR_PATH), mas cada empresa pode
# sobrescrever com {DISTRO}_IRIS_DRIVER_CLASS / {DISTRO}_IRIS_JAR_PATH.
#
# Pacotes (DBI, RJDBC) são carregados no app.R — sem library() aqui.
#
# As CONSULTAS (IRIS e dados de exemplo no SQLite) ficam em
# R/fonte_dados.R — inclusive consultar_matricula(), que antes ficava
# aqui.
# =====================================================

# -----------------------------------------------------
# CACHE DO DRIVER JDBC
# -----------------------------------------------------
# RJDBC::JDBC() adiciona o .jar ao classpath da JVM e carrega a classe do
# driver — antes isso era refeito a CADA consulta. Agora o driver é criado
# uma única vez por combinação (classe, jar) e reaproveitado.
# -----------------------------------------------------
.jdbc_cache <- new.env(parent = emptyenv())

obter_driver_jdbc <- function(driver_class, jar_path) {

    chave <- paste(driver_class, jar_path, sep = "|")

    if (is.null(.jdbc_cache[[chave]])) {

        if (!requireNamespace("RJDBC", quietly = TRUE)) {
            stop("Pacote 'RJDBC' indisponível — não é possível conectar ao IRIS.")
        }

        .jdbc_cache[[chave]] <- RJDBC::JDBC(
            driverClass = driver_class,
            classPath   = jar_path
        )
    }

    .jdbc_cache[[chave]]
}

# Lê a primeira variável de ambiente definida (não vazia) da lista.
env_primeira <- function(nomes, padrao = "") {
    for (nome in nomes) {
        valor <- Sys.getenv(nome, unset = "")
        if (nzchar(valor)) return(valor)
    }
    padrao
}

conectar_banco <- function(distro) {

    if (is.null(distro) || trimws(distro) == "") {
        stop(
            "Nenhuma empresa (distro) informada para conectar_banco(). ",
            "Verifique se o login selecionou uma empresa válida."
        )
    }

    distro <- toupper(trimws(distro))

    driver_class <- env_primeira(
        c(distro_env(distro, "IRIS_DRIVER_CLASS"), "IRIS_DRIVER_CLASS"),
        padrao = "com.intersystems.jdbc.IRISDriver"
    )

    jar_path <- env_primeira(c(distro_env(distro, "IRIS_JAR_PATH"), "IRIS_JAR_PATH"))

    url  <- env_primeira(distro_env(distro, "IRIS_URL"))
    user <- env_primeira(distro_env(distro, "IRIS_USER"))

    # {DISTRO}_IRIS_PASS é o nome usado nos .Rmd originais — aceito como
    # alternativa a {DISTRO}_IRIS_PASSWORD.
    password <- env_primeira(c(distro_env(distro, "IRIS_PASSWORD"), distro_env(distro, "IRIS_PASS")))

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

    DBI::dbConnect(
        obter_driver_jdbc(driver_class, jar_path),
        url,
        user = user,
        password = password
    )
}
