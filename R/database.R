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
        .jdbc_cache[[chave]] <- RJDBC::JDBC(
            driverClass = driver_class,
            classPath   = jar_path
        )
    }

    .jdbc_cache[[chave]]
}

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
        unset = Sys.getenv("IRIS_DRIVER_CLASS", unset = "com.intersystems.jdbc.IRISDriver")
    )

    jar_path <- Sys.getenv(
        distro_env(distro, "IRIS_JAR_PATH"),
        unset = Sys.getenv("IRIS_JAR_PATH", unset = "")
    )

    url      <- Sys.getenv(distro_env(distro, "IRIS_URL"),      unset = "")
    user     <- Sys.getenv(distro_env(distro, "IRIS_USER"),     unset = "")
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

    DBI::dbConnect(
        obter_driver_jdbc(driver_class, jar_path),
        url,
        user = user,
        password = password
    )
}

# -----------------------------------------------------
# CONSULTA DA MATRÍCULA (movida do app.R)
# -----------------------------------------------------
# Usa parâmetro (?) em vez de interpolar o valor no SQL: o IRIS pode
# reaproveitar o plano de execução e elimina qualquer risco de injeção.
# -----------------------------------------------------
SQL_CONSULTA_MATRICULA <- "
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
    Servidor->MATRICULA = ?
ORDER BY
    ProvDocumento_DataDoc DESC
"

consultar_matricula <- function(matricula_num, distro) {

    con_iris <- conectar_banco(distro)
    on.exit(try(DBI::dbDisconnect(con_iris), silent = TRUE), add = TRUE)

    DBI::dbGetQuery(con_iris, SQL_CONSULTA_MATRICULA, as.integer(matricula_num))
}
