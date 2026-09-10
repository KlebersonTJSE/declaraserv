# =====================================================
# R/utils.R
# -----------------------------------------------------
# Funções utilitárias genéricas usadas pela tela de login
# e pelo cabeçalho do usuário autenticado.
#
# OBS: as funções específicas do domínio "Rejeitados" do
# radarsocial (carregar_rejeitados, extrair_matricula,
# extrair_nome, formatar_periodo) foram removidas nesta
# adaptação para o declaraserv, pois dependiam de objetos
# (CAMINHO_REJEITADOS) e pacotes (dplyr/purrr/stringr) que
# não fazem parte deste app e não seriam usados.
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

# # R/utils.R
#
# obter_campo <- function(lista, nome){
#
#   valor <- lista[[nome]]
#
#   if(is.null(valor))
#     return("")
#
#   if(length(valor) == 0)
#     return("")
#
#   paste(as.character(valor), collapse = "; ")
#
# }
#
# extrair_manager <- function(manager){
#
#   if(is.null(manager))
#     return("")
#
#   if(length(manager) == 0)
#     return("")
#
#   manager <- as.character(manager)[1]
#
#   sub("^CN=([^,]+),.*$", "\\1", manager)
#
# }
#
# formatar_when_created <- function(whenCreated){
#
#   if(is.null(whenCreated))
#     return("")
#
#   valor <- as.character(whenCreated)[1]
#
#   dt <- tryCatch({
#
#     as.POSIXct(
#       valor,
#       format="%Y%m%d%H%M%S.0Z",
#       tz="UTC"
#     )
#
#   }, error=function(e){
#
#     NA
#
#   })
#
#   if(is.na(dt))
#     return(valor)
#
#   format(
#     dt,
#     "%d/%m/%Y %H:%M:%S"
#   )
#
# }
#
# formatar_whenCreated <- function(x) {
#     if (is.null(x) || is.na(x) || x == "") return("")
#
#     format(
#         as.POSIXct(
#             gsub("\\.0Z$", "", x),
#             format = "%Y%m%d%H%M%S",
#             tz = "UTC"
#         ),
#         "%d/%m/%Y %H:%M:%S"
#     )
# }
#
# formatar_lastLogon <- function(x) {
#     if (is.null(x) || is.na(x) || x == "" || x == "0") {
#         return("Nunca acessou")
#     }
#
#     format(
#         as.POSIXct(
#             (as.numeric(x) / 10000000) - 11644473600,
#             origin = "1970-01-01",
#             tz = "UTC"
#         ),
#         "%d/%m/%Y %H:%M:%S"
#     )
# }
#
# carregar_rejeitados <- function(){
#
#     arquivos <- list.files(
#         CAMINHO_REJEITADOS,
#         pattern = "\\.csv$",
#         full.names = TRUE
#     )
#
#     if(length(arquivos) == 0){
#         return(data.frame())
#     }
#
#     resultados <- map(
#         arquivos,
#         processar_rejeitado
#     )
#
#     bind_rows(
#         resultados
#     ) %>%
#         distinct()
#
# }
#
# # =====================================================
# # EXTRAI MATRÍCULA
# # =====================================================
#
# extrair_matricula <- function(texto){
#
#     sapply(texto, function(x){
#
#         partes <- str_split(
#             x,
#             " - ",
#             n = 2
#         )[[1]]
#
#         trimws(partes[1])
#
#     })
#
# }
#
# # =====================================================
# # EXTRAI NOME
# # =====================================================
#
# extrair_nome <- function(texto){
#
#     sapply(texto, function(x){
#
#         partes <- str_split(
#             x,
#             " - ",
#             n = 2
#         )[[1]]
#
#         if(length(partes) >= 2){
#
#             trimws(partes[2])
#
#         } else {
#
#             NA_character_
#
#         }
#
#     })
#
# }
#
# # =====================================================
# # EXTRAI PERIODO
# # =====================================================
#
# formatar_periodo <- function(periodo){
#
#     paste0(
#         str_extract(periodo, "\\d{4}$"),
#         str_pad(
#             str_extract(periodo, "^\\d{1,2}"),
#             width = 2,
#             pad = "0"
#         )
#     )
#
# }
