# =====================================================
# R/database.R
# -----------------------------------------------------
# Conexão única com o IRIS, reutilizada por
# consultar_matricula() em app.R.
#
# Usa os MESMOS nomes de variável de ambiente que o
# restante do app.R já valida no início (MPRO_IRIS_*),
# para não haver duas convenções diferentes (uma delas
# nunca lida) para a mesma credencial.
# =====================================================

library(DBI)
library(RJDBC)

conectar_banco <- function() {

    drv <- JDBC(
        driverClass = Sys.getenv(
            "MPRO_IRIS_DRIVER_CLASS",
            unset = "com.intersystems.jdbc.IRISDriver"
        ),
        classPath = Sys.getenv("MPRO_IRIS_JAR_PATH")
    )

    dbConnect(
        drv,
        Sys.getenv("MPRO_IRIS_URL"),
        user = Sys.getenv("MPRO_IRIS_USER"),
        password = Sys.getenv("MPRO_IRIS_PASS")
    )

}

# # =====================================================
# # R/database.R
# # =====================================================
#
# library(DBI)
# library(RJDBC)
#
# conectar_banco <- function(){
#
#     drv <- JDBC(
#         driverClass = Sys.getenv("IRIS_DRIVER"),
#         classPath = Sys.getenv("IRIS_JAR")
#     )
#
#     dbConnect(
#         drv,
#         Sys.getenv("IRIS_URL"),
#         user = Sys.getenv("IRIS_USER"),
#         password = Sys.getenv("IRIS_PASSWORD")
#     )
#
# }
