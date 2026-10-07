# =====================================================
# R/fonte_dados.R
# -----------------------------------------------------
# FONTE DE DADOS DAS DECLARAÇÕES: IRIS (produção) ou SQLite (EXEMPLO)
#
# Todas as consultas usadas pela tela "Declaração" e pelos relatórios
# RMarkdown (rmark/<EMPRESA>/*.Rmd) ficam aqui, em duas versões:
#
#   - "iris"    -> as MESMAS consultas que antes ficavam dentro de cada
#                  .Rmd, executadas no InterSystems IRIS de produção
#                  (credenciais no .Renviron, ver R/database.R);
#   - "exemplo" -> consultas equivalentes sobre a tabela
#                  exemplo_relatorios do SQLite da aplicação
#                  (data/declaraserv.db), com DADOS FICTÍCIOS que devolvem
#                  exatamente as mesmas colunas (mesmos nomes/aliases).
#
# QUAL FONTE É USADA (variável DECLARASERV_FONTE_DADOS no .Renviron):
#   - "auto" (padrão) -> usa o IRIS quando ele está acessível; se não
#                        estiver (rede, servidor fora do ar, credencial,
#                        driver/Java ausente), usa os dados de EXEMPLO;
#   - "iris"          -> sempre IRIS (sem fallback — erro se indisponível);
#   - "exemplo"       -> sempre os dados de EXEMPLO (ex.: demonstração).
#
# O resultado do teste de acesso ao IRIS fica em cache por
# DECLARASERV_IRIS_TTL_MIN minutos (padrão 5), para não tentar conectar a
# cada consulta. DECLARASERV_IRIS_TIMEOUT_SEG (padrão 5) limita o tempo
# do teste.
#
# Este arquivo também funciona fora do Shiny (renderização isolada de um
# .Rmd): carrega o .Renviron e os arquivos de que depende, se preciso.
# =====================================================

# -----------------------------------------------------
# DEPENDÊNCIAS (quando usado fora do app.R)
# -----------------------------------------------------
local({
    raiz <- getwd()

    if (!nzchar(Sys.getenv("DISTRO_1")) && file.exists(file.path(raiz, ".Renviron"))) {
        readRenviron(file.path(raiz, ".Renviron"))
    }

    if (!exists("distro_env", mode = "function", envir = globalenv())) {
        assign(
            "distro_env",
            function(distro, sufixo) paste0(toupper(trimws(distro)), "_", sufixo),
            envir = globalenv()
        )
    }

    if (!exists("conectar_banco", mode = "function", envir = globalenv()) &&
        file.exists(file.path(raiz, "R", "database.R"))) {
        sys.source(file.path(raiz, "R", "database.R"), envir = globalenv())
    }
})

TABELA_EXEMPLO <- "exemplo_relatorios"

# -----------------------------------------------------
# BANCO SQLite DOS DADOS DE EXEMPLO
# -----------------------------------------------------
# Mesmo arquivo do controle de acesso (data/declaraserv.db). Quando
# chamado pelo app, DB_LOGIN_PATH já está definido; fora dele, usa o
# caminho padrão a partir da pasta do projeto.
caminho_banco_exemplo <- function() {
    if (exists("DB_LOGIN_PATH", envir = globalenv())) {
        return(get("DB_LOGIN_PATH", envir = globalenv()))
    }
    file.path(getwd(), "data", "declaraserv.db")
}

conectar_exemplo <- function() {
    caminho <- caminho_banco_exemplo()
    if (!file.exists(caminho)) {
        stop("Banco de dados de exemplo não encontrado em: ", caminho)
    }
    DBI::dbConnect(RSQLite::SQLite(), caminho)
}

# =====================================================
# TABELA exemplo_relatorios
# -----------------------------------------------------
# Uma única tabela atende a todos os relatórios. TIPO_REGISTRO diz o que
# cada linha representa — o equivalente às tabelas do IRIS:
#
#   SERVIDOR            -> RHCADSERVIDOR (dados funcionais + gestor da
#                          lotação de exercício)          [vínculo funcional]
#   CARGO_EFETIVO       -> RHCadCargoEfetivo (portaria/diário de
#                          nomeação)        [certidão, consolidada, contribuição]
#   AFASTAMENTO         -> RHCadAfastamento               [afastamentos]
#   MOVIMENTACAO        -> RHCadMovimentacao              [tempo de serviço]
#   FUNCAO_COMISSIONADA -> RHCadFuncaoComissionada        [vínculo funcional]
#   LOTACAO             -> RHTabLotacao (gestor da DGP, que assina)
#
# As colunas têm os MESMOS nomes dos aliases das consultas dos .Rmd.
# Datas "Formatadas" do IRIS ficam como texto dd/mm/aaaa; DATAFIM do
# servidor (DataDesligamento) fica como aaaa-mm-dd — como no IRIS.
# Campos calculados no IRIS (PERIODO, ANOS, MESES, TEMPO...) são
# calculados também na consulta do SQLite, a partir de DIAS/datas.
# =====================================================

garantir_dados_exemplo <- function(con) {

    DBI::dbExecute(con, sprintf("
        CREATE TABLE IF NOT EXISTS %s (
            ID                  INTEGER PRIMARY KEY AUTOINCREMENT,
            TIPO_REGISTRO       TEXT NOT NULL,
            MATRICULA           INTEGER,
            NOME                TEXT,
            CPF                 TEXT,
            CARGO               TEXT,
            CLASSENIVEL         TEXT,
            REGIMEJUR           TEXT,
            FORMAINGRESSO       TEXT,
            DATAINGRESSO        TEXT,
            DATAINICIOEXERCICIO TEXT,
            SITUACAO            TEXT,
            LOTACAO             TEXT,
            LOTACAOEXERCICIO    TEXT,
            DATAINICIO          TEXT,
            DATAFIM             TEXT,
            DATAFINAL           TEXT,
            PORTARIA_TIPO       TEXT,
            PORTARIA_NUMERO     TEXT,
            PORTARIA_DATA       TEXT,
            DIARIO_TIPO         TEXT,
            DIARIO_NUMERO       TEXT,
            DIARIO_DATA         TEXT,
            COD_AFASTAMENTO     INTEGER,
            AFASTAMENTO         TEXT,
            DIAS                INTEGER,
            CARGOFUNCAO         TEXT,
            UNIDADE             TEXT,
            ATOADMIN            TEXT,
            GESTOR_NOME         TEXT,
            GESTOR_CARGO        TEXT,
            GESTOR_MATRICULA    INTEGER,
            SIGLA               TEXT,
            EMAIL               TEXT,
            TELEFONE            TEXT
        )", TABELA_EXEMPLO))

    DBI::dbExecute(con, sprintf(
        "CREATE INDEX IF NOT EXISTS idx_%1$s_tipo_matricula ON %1$s (TIPO_REGISTRO, MATRICULA)",
        TABELA_EXEMPLO
    ))

    # Versão da carga gravada no banco (tabela exemplo_meta). A tabela é
    # (re)carregada quando está vazia ou quando a versão gravada é
    # diferente de VERSAO_SEMENTE_EXEMPLO — por exemplo, um banco com a
    # carga antiga, de 4 servidores. Fora isso, nada é sobrescrito.
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS exemplo_meta (chave TEXT PRIMARY KEY, valor TEXT)")

    versao_gravada <- DBI::dbGetQuery(
        con, "SELECT valor FROM exemplo_meta WHERE chave = 'versao_semente'"
    )$valor
    versao_gravada <- if (length(versao_gravada) == 0) NA_integer_ else suppressWarnings(as.integer(versao_gravada))

    qtd <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", TABELA_EXEMPLO))$n

    if (qtd == 0 || !identical(versao_gravada, VERSAO_SEMENTE_EXEMPLO)) {
        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, sprintf("DELETE FROM %s", TABELA_EXEMPLO))
            DBI::dbAppendTable(con, TABELA_EXEMPLO, dados_exemplo_semente())
            DBI::dbExecute(
                con,
                "INSERT OR REPLACE INTO exemplo_meta (chave, valor) VALUES ('versao_semente', ?)",
                params = list(as.character(VERSAO_SEMENTE_EXEMPLO))
            )
        })
        message("Dados de exemplo carregados (versão ", VERSAO_SEMENTE_EXEMPLO, ").")
    }

    invisible(TRUE)
}

# -----------------------------------------------------
# DADOS FICTÍCIOS (carga inicial) — 50 SERVIDORES
# -----------------------------------------------------
# Pessoas, CPFs, matrículas, portarias, e-mails e telefones INVENTADOS,
# gerados de forma determinística (semente fixa: a carga é sempre a
# mesma). Matrículas 900001 a 900050; gestores na faixa 8000xx.
#
# Cada servidor tem registros para TODOS os relatórios:
#   - SERVIDOR            dados funcionais completos + gestor da lotação
#   - CARGO_EFETIVO       1 a 2 atos (nomeação e, se houve, lotação atual)
#   - AFASTAMENTO         2 a 4 (às vezes um de código excluído pelo filtro)
#   - MOVIMENTACAO        2 a 3 períodos (+ um período descontado em alguns)
#   - FUNCAO_COMISSIONADA 1 a 2
# Ficam sem data de fim apenas o que está EM CURSO (vínculo ativo,
# período/função atual) — como no IRIS. 1 em cada 5 servidores está
# desligado (todas as datas de fim preenchidas).
#
# VERSAO_SEMENTE_EXEMPLO: ao mudar a carga, aumente o número — o app
# recria a tabela na próxima inicialização (ver garantir_dados_exemplo).
# -----------------------------------------------------
VERSAO_SEMENTE_EXEMPLO <- 2L

dados_exemplo_semente <- function() {

    # Semente fixa sem alterar o gerador aleatório da sessão.
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        semente_anterior <- get(".Random.seed", envir = globalenv())
        on.exit(assign(".Random.seed", semente_anterior, envir = globalenv()), add = TRUE)
    } else {
        on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
    }
    set.seed(20261006)

    COLUNAS <- c(
        "TIPO_REGISTRO", "MATRICULA", "NOME", "CPF", "CARGO", "CLASSENIVEL",
        "REGIMEJUR", "FORMAINGRESSO", "DATAINGRESSO", "DATAINICIOEXERCICIO",
        "SITUACAO", "LOTACAO", "LOTACAOEXERCICIO", "DATAINICIO", "DATAFIM",
        "DATAFINAL", "PORTARIA_TIPO", "PORTARIA_NUMERO", "PORTARIA_DATA",
        "DIARIO_TIPO", "DIARIO_NUMERO", "DIARIO_DATA", "COD_AFASTAMENTO",
        "AFASTAMENTO", "DIAS", "CARGOFUNCAO", "UNIDADE", "ATOADMIN",
        "GESTOR_NOME", "GESTOR_CARGO", "GESTOR_MATRICULA", "SIGLA", "EMAIL",
        "TELEFONE"
    )
    INTEIROS <- c("MATRICULA", "COD_AFASTAMENTO", "DIAS", "GESTOR_MATRICULA")

    linhas <- list()
    linha <- function(...) {
        x <- list(...)
        valores <- lapply(COLUNAS, function(cl) {
            v <- x[[cl]]
            if (cl %in% INTEIROS) {
                if (is.null(v)) NA_integer_ else as.integer(v)
            } else {
                if (is.null(v)) NA_character_ else as.character(v)
            }
        })
        names(valores) <- COLUNAS
        linhas[[length(linhas) + 1]] <<- as.data.frame(valores, stringsAsFactors = FALSE)
    }

    br  <- function(d) format(d, "%d/%m/%Y")
    dias_entre <- function(a, b) as.integer(b - a) + 1L
    sorteia <- function(x) x[sample.int(length(x), 1)]
    data_entre <- function(a, b) {
        if (b <= a) return(a)
        a + sample.int(as.integer(b - a) + 1L, 1) - 1L
    }
    num_ato <- function(d) sprintf("%03d/%s", sample(1:1999, 1), format(d, "%Y"))

    DIARIO   <- "Diário Oficial Eletrônico do MPRO"
    DATA_REF <- as.Date("2026-09-30")   # "hoje" da carga (dados determinísticos)

    # Unidades e seus gestores (fictícios).
    UNIDADES <- data.frame(
        UNIDADE = c(
            "DIRETORIA-GERAL DE PESSOAL", "DIRETORIA DE TECNOLOGIA DA INFORMAÇÃO",
            "DIRETORIA DE ADMINISTRAÇÃO", "DIRETORIA DE ORÇAMENTO E FINANÇAS",
            "PROMOTORIA DE JUSTIÇA DE ARIQUEMES", "PROMOTORIA DE JUSTIÇA DE JI-PARANÁ",
            "PROMOTORIA DE JUSTIÇA DE VILHENA", "PROMOTORIA DE JUSTIÇA DE CACOAL",
            "PROMOTORIA DE JUSTIÇA DE GUAJARÁ-MIRIM", "CENTRO DE APOIO OPERACIONAL CÍVEL",
            "CORREGEDORIA-GERAL", "OUVIDORIA-GERAL"
        ),
        GESTOR_NOME = c(
            "Patrícia Almeida Nunes", "Ricardo Gomes Fernandes", "Luciana Barros Teixeira",
            "Marcelo Duarte Pinheiro", "Fernanda Oliveira Rocha", "Gustavo Henrique Prado",
            "Renata Cavalcanti Moura", "Sérgio Lopes Andrade", "Juliana Freitas Campos",
            "Eduardo Rangel Siqueira", "Beatriz Monteiro Sales", "André Luiz Carvalho"
        ),
        GESTOR_CARGO = c(
            "Diretora-Geral de Pessoal", "Diretor de Tecnologia da Informação",
            "Diretora de Administração", "Diretor de Orçamento e Finanças",
            "Promotora de Justiça Coordenadora", "Promotor de Justiça Coordenador",
            "Promotora de Justiça Coordenadora", "Promotor de Justiça Coordenador",
            "Promotora de Justiça Coordenadora", "Promotor de Justiça Coordenador",
            "Corregedora-Geral", "Ouvidor-Geral"
        ),
        GESTOR_MATRICULA = 800010 + (0:11) * 11,
        stringsAsFactors = FALSE
    )

    CARGOS <- c(
        "ANALISTA JURÍDICO", "ANALISTA DE SISTEMAS", "ANALISTA CONTÁBIL",
        "ANALISTA ADMINISTRATIVO", "TÉCNICO ADMINISTRATIVO", "TÉCNICO EM INFORMÁTICA",
        "OFICIAL DE DILIGÊNCIAS", "AGENTE ADMINISTRATIVO", "ASSISTENTE SOCIAL",
        "PSICÓLOGO", "ENGENHEIRO CIVIL", "ARQUIVISTA"
    )

    FUNCOES <- c(
        "ASSESSOR TÉCNICO", "CHEFE DE DIVISÃO", "CHEFE DE SEÇÃO",
        "COORDENADOR DE NÚCLEO", "ASSESSOR DE GABINETE", "SECRETÁRIO DE PROMOTORIA",
        "GERENTE DE PROJETOS", "SUPERVISOR DE EQUIPE"
    )

    # Tipos de afastamento: código, descrição, faixa de dias. O código 55
    # está na lista de exclusão do relatório (não aparece no documento).
    AFAST <- list(
        list(1L,  "FÉRIAS REGULAMENTARES",                           c(30, 30)),
        list(20L, "LICENÇA-PRÊMIO POR ASSIDUIDADE",                  c(30, 90)),
        list(31L, "LICENÇA PARA CAPACITAÇÃO",                        c(15, 90)),
        list(24L, "LICENÇA MATERNIDADE",                             c(180, 180)),
        list(25L, "LICENÇA PATERNIDADE",                             c(20, 20)),
        list(27L, "LICENÇA POR MOTIVO DE CASAMENTO (GALA)",          c(8, 8)),
        list(28L, "LICENÇA POR MOTIVO DE FALECIMENTO (NOJO)",        c(8, 8)),
        list(33L, "AFASTAMENTO PARA SERVIR A OUTRO ÓRGÃO",           c(60, 365)),
        list(55L, "LICENÇA PARA TRATAMENTO DA PRÓPRIA SAÚDE",        c(5, 30))
    )

    NOMES <- c(
        "Ana", "Bruno", "Carla", "Diego", "Eduarda", "Fábio", "Gabriela", "Heitor",
        "Isabela", "João", "Karina", "Lucas", "Mariana", "Nicolas", "Olívia",
        "Paulo", "Rafaela", "Samuel", "Tatiana", "Vinícius", "Yasmin", "Leonardo",
        "Camila", "Rodrigo", "Letícia"
    )
    SOBRENOMES <- c(
        "Pereira", "Costa", "Ribeiro", "Martins", "Souza", "Almeida", "Carvalho",
        "Gomes", "Barbosa", "Rocha", "Dias", "Moreira", "Cardoso", "Teixeira",
        "Correia", "Mendes", "Nogueira", "Batista", "Freitas", "Lima", "Araújo",
        "Monteiro", "Vieira", "Pinto", "Machado"
    )

    # =============================== LOTAÇÃO DGP (quem assina)
    linha(TIPO_REGISTRO = "LOTACAO", SIGLA = "DGP",
          LOTACAO = "DIRETORIA-GERAL DE PESSOAL",
          GESTOR_NOME = UNIDADES$GESTOR_NOME[1],
          GESTOR_CARGO = UNIDADES$GESTOR_CARGO[1],
          GESTOR_MATRICULA = UNIDADES$GESTOR_MATRICULA[1],
          EMAIL = "dgp.exemplo@exemplo.invalid",
          TELEFONE = "(69) 0000-0000")

    nomes_usados <- character(0)

    for (i in 1:50) {

        matricula <- 900000L + i

        repeat {
            nome <- paste(sorteia(NOMES), sorteia(SOBRENOMES), sorteia(SOBRENOMES))
            partes <- strsplit(nome, " ")[[1]]
            if (partes[2] != partes[3] && !(nome %in% nomes_usados)) break
        }
        nomes_usados <- c(nomes_usados, nome)

        cpf <- sprintf("000.%03d.%03d-%02d", sample(0:999, 1), sample(0:999, 1), i)

        cargo    <- sorteia(CARGOS)
        ingresso <- data_entre(as.Date("2000-01-10"), as.Date("2023-12-15"))
        exercicio_ini <- ingresso + sample(3:10, 1)

        desligado <- (i %% 5 == 0)
        fim_vinculo <- if (desligado) {
            min(DATA_REF - 30, exercicio_ini + sample(3:15, 1) * 365 + sample(0:300, 1))
        } else {
            NA
        }
        fim_ref <- if (desligado) fim_vinculo else DATA_REF
        if (desligado && fim_vinculo <= exercicio_ini + 400) fim_vinculo <- fim_ref <- exercicio_ini + 400

        situacao <- if (!desligado) {
            "EFETIVO - ATIVO"
        } else {
            sorteia(c("EXONERADO(A) A PEDIDO", "APOSENTADO(A)", "VACÂNCIA - POSSE EM OUTRO CARGO"))
        }

        # ---- Movimentações (2 a 3 períodos consecutivos)
        n_mov <- sample(2:3, 1)
        cortes <- sort(sample(seq(exercicio_ini + 180, fim_ref - 180, by = "day"), n_mov - 1))
        inicios <- c(exercicio_ini, cortes + 1)
        fins    <- c(cortes, fim_ref)
        unidades_mov <- UNIDADES[sample(nrow(UNIDADES), n_mov), ]
        unidade_atual <- unidades_mov[n_mov, ]

        # ---- SERVIDOR
        linha(TIPO_REGISTRO = "SERVIDOR", MATRICULA = matricula,
              NOME = nome, CPF = cpf, CARGO = cargo,
              CLASSENIVEL = sprintf("Classe %s - Padrão %s", sorteia(c("I", "II", "III")), sorteia(LETTERS[1:5])),
              REGIMEJUR = "ESTATUTÁRIO",
              FORMAINGRESSO = if (i %% 9 == 0) "NOMEAÇÃO" else "CONCURSO",
              DATAINGRESSO = br(ingresso),
              DATAFIM = if (desligado) format(fim_vinculo, "%Y-%m-%d") else NULL,
              DATAINICIOEXERCICIO = br(exercicio_ini),
              SITUACAO = situacao,
              LOTACAO = if (grepl("^PROMOTORIA", unidade_atual$UNIDADE)) unidade_atual$UNIDADE else "PROCURADORIA-GERAL DE JUSTIÇA",
              LOTACAOEXERCICIO = unidade_atual$UNIDADE,
              GESTOR_NOME = unidade_atual$GESTOR_NOME,
              GESTOR_CARGO = unidade_atual$GESTOR_CARGO,
              GESTOR_MATRICULA = unidade_atual$GESTOR_MATRICULA)

        # ---- CARGO_EFETIVO: nomeação (+ ato de lotação atual)
        d_nom <- ingresso - sample(10:30, 1)
        linha(TIPO_REGISTRO = "CARGO_EFETIVO", MATRICULA = matricula,
              PORTARIA_TIPO = "Ato de Nomeação", PORTARIA_NUMERO = num_ato(d_nom),
              PORTARIA_DATA = br(d_nom),
              DIARIO_TIPO = DIARIO, DIARIO_NUMERO = sprintf("%03d", sample(1:250, 1)),
              DIARIO_DATA = br(d_nom + 2))

        if (n_mov > 1 || i %% 2 == 0) {
            d_lot <- inicios[n_mov] - sample(2:8, 1)
            linha(TIPO_REGISTRO = "CARGO_EFETIVO", MATRICULA = matricula,
                  PORTARIA_TIPO = "Portaria", PORTARIA_NUMERO = num_ato(d_lot),
                  PORTARIA_DATA = br(d_lot),
                  DIARIO_TIPO = DIARIO, DIARIO_NUMERO = sprintf("%03d", sample(1:250, 1)),
                  DIARIO_DATA = br(d_lot + 2))
        }

        for (k in seq_len(n_mov)) {
            em_curso <- (k == n_mov && !desligado)
            linha(TIPO_REGISTRO = "MOVIMENTACAO", MATRICULA = matricula,
                  DATAINICIO = br(inicios[k]),
                  DATAFIM = if (em_curso) NULL else br(fins[k]),
                  DIAS = if (em_curso) NULL else dias_entre(inicios[k], fins[k]),
                  CARGOFUNCAO = cargo, UNIDADE = unidades_mov$UNIDADE[k])
        }

        # Período descontado (não computado) em 1 de cada 6 servidores.
        if (i %% 6 == 0) {
            d_ini <- data_entre(exercicio_ini + 60, fim_ref - 200)
            dur   <- sample(30:120, 1)
            linha(TIPO_REGISTRO = "MOVIMENTACAO", MATRICULA = matricula,
                  DATAINICIO = br(d_ini), DATAFIM = br(d_ini + dur - 1), DIAS = -dur,
                  CARGOFUNCAO = "LICENÇA PARA TRATAR DE INTERESSES PARTICULARES (NÃO COMPUTADA)",
                  UNIDADE = unidades_mov$UNIDADE[1])
        }

        # ---- AFASTAMENTOS (2 a 4; o primeiro é sempre férias)
        n_af <- sample(2:4, 1)
        tipos <- c(1L, sample(2:length(AFAST), n_af - 1))
        for (t in tipos) {
            af  <- AFAST[[t]]
            dur <- if (af[[3]][1] == af[[3]][2]) af[[3]][1] else sample(af[[3]][1]:af[[3]][2], 1)
            d_ini <- data_entre(exercicio_ini + 30, fim_ref - dur - 1)
            linha(TIPO_REGISTRO = "AFASTAMENTO", MATRICULA = matricula,
                  COD_AFASTAMENTO = af[[1]], AFASTAMENTO = af[[2]],
                  DATAINICIO = br(d_ini), DATAFINAL = br(d_ini + dur - 1), DIAS = dur)
        }

        # ---- FUNÇÕES COMISSIONADAS (1 a 2, consecutivas)
        n_fc <- sample(1:2, 1)
        fc_ini <- data_entre(exercicio_ini + 90, fim_ref - 400)
        for (k in seq_len(n_fc)) {
            ultima   <- (k == n_fc)
            em_curso <- ultima && !desligado && (i %% 3 != 0)
            fc_fim   <- if (ultima) fim_ref else data_entre(fc_ini + 120, fc_ini + 900)
            fc_fim   <- min(fc_fim, fim_ref)
            linha(TIPO_REGISTRO = "FUNCAO_COMISSIONADA", MATRICULA = matricula,
                  DATAINICIO = br(fc_ini),
                  DATAFIM = if (em_curso) NULL else br(fc_fim),
                  CARGOFUNCAO = sorteia(FUNCOES),
                  ATOADMIN = sprintf("Portaria - %04d/%s", sample(1:1999, 1), format(fc_ini, "%Y")))
            fc_ini <- fc_fim + 1
            if (fc_ini >= fim_ref - 30) break
        }
    }

    do.call(rbind, linhas)
}

# Matrículas disponíveis nos dados de exemplo (combobox da tela
# "Declaração" quando a fonte é o SQLite de exemplo).
listar_matriculas_exemplo <- function() {
    con <- conectar_exemplo()
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    DBI::dbGetQuery(con, sprintf(
        "SELECT MATRICULA, NOME FROM %s WHERE TIPO_REGISTRO = 'SERVIDOR' ORDER BY NOME",
        TABELA_EXEMPLO
    ))
}

# =====================================================
# CONSULTAS
# -----------------------------------------------------
# Cada chave tem a consulta IRIS (a mesma de antes, com "?" no lugar da
# matrícula interpolada) e a equivalente no SQLite, com as MESMAS
# colunas. Linhas sem data de fim (em curso) têm DIAS calculado até hoje.
# =====================================================

# dd/mm/aaaa -> aaaa-mm-dd (para ordenar e calcular datas no SQLite).
.iso <- function(col) sprintf("(substr(%1$s,7,4)||'-'||substr(%1$s,4,2)||'-'||substr(%1$s,1,2))", col)

.dias_ate_hoje <- function(col_inicio) {
    sprintf("(CAST(julianday(date('now','localtime')) - julianday(%s) AS INTEGER) + 1)", .iso(col_inicio))
}

SQL_RELATORIOS <- list(

    # Certidão / Funcional Consolidada / Tempo de Contribuição + tela Declaração
    cargo_efetivo = list(
        iris = "
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
    ProvDocumento_DataDoc DESC",
        exemplo = sprintf("
SELECT
    s.NOME, s.DATAINGRESSO AS DATAINICIO, s.DATAFIM,
    c.PORTARIA_TIPO, c.PORTARIA_NUMERO, c.PORTARIA_DATA,
    c.DIARIO_TIPO, c.DIARIO_NUMERO, c.DIARIO_DATA,
    s.LOTACAOEXERCICIO AS LOTACAO, s.MATRICULA,
    s.GESTOR_NOME, s.GESTOR_CARGO, s.GESTOR_MATRICULA
FROM %1$s c
JOIN %1$s s ON s.TIPO_REGISTRO = 'SERVIDOR' AND s.MATRICULA = c.MATRICULA
WHERE c.TIPO_REGISTRO = 'CARGO_EFETIVO' AND c.MATRICULA = ?
ORDER BY %2$s DESC
LIMIT 1", TABELA_EXEMPLO, .iso("c.PORTARIA_DATA"))
    ),

    # Afastamentos funcionais
    afastamentos = list(
        iris = "
SELECT
    Servidor->Nome AS NOME,
    Servidor->Pessoa->CpfFormatado AS CPF,
    Servidor->Matricula AS MATRICULA,
    Servidor->Funcional->CargoEfetivo->Descricao AS CARGO,
    Afastamento->Descricao AS AFASTAMENTO,
    DataInicialFormatada AS DATAINICIO,
    DataFinalFormatada AS DATAFINAL,
    Dias AS DIAS,
    YEAR(DataInicial) AS PERIODO,
    Servidor->Funcional->LotacaoExercicio->Gestor->Nome AS GESTOR_NOME,
    Servidor->Funcional->LotacaoExercicio->Gestor->Funcional->CargoFuncao->Descricao AS GESTOR_CARGO,
    Servidor->Funcional->LotacaoExercicio->Gestor->Matricula AS GESTOR_MATRICULA
FROM
    RHCadAfastamento
WHERE
    Afastamento NOT IN (55,74,75,82,46,114,116,89)
    AND Servidor->Matricula = ?
ORDER BY
    DataInicial",
        exemplo = sprintf("
SELECT
    s.NOME, s.CPF, s.MATRICULA, s.CARGO,
    a.AFASTAMENTO, a.DATAINICIO, a.DATAFINAL,
    COALESCE(a.DIAS, %2$s) AS DIAS,
    CAST(substr(a.DATAINICIO, 7, 4) AS INTEGER) AS PERIODO,
    s.GESTOR_NOME, s.GESTOR_CARGO, s.GESTOR_MATRICULA
FROM %1$s a
JOIN %1$s s ON s.TIPO_REGISTRO = 'SERVIDOR' AND s.MATRICULA = a.MATRICULA
WHERE a.TIPO_REGISTRO = 'AFASTAMENTO'
  AND a.COD_AFASTAMENTO NOT IN (55,74,75,82,46,114,116,89)
  AND a.MATRICULA = ?
ORDER BY %3$s", TABELA_EXEMPLO, .dias_ate_hoje("a.DATAINICIO"), .iso("a.DATAINICIO"))
    ),

    # Tempo de serviço
    periodos_exercicio = list(
        iris = "
SELECT
    Servidor->Nome AS NOME,
    Servidor->Pessoa->CpfFormatado AS CPF,
    Servidor->Matricula AS MATRICULA,
    DataFormatada AS DATAINICIO,
    DataFimFormatada AS DATAFIM,
    Dias AS DIAS,
    FLOOR(Dias / 365) AS ANOS,
    FLOOR(MOD(Dias, 365) / 30) AS MESES,
    MOD(MOD(Dias, 365), 30) AS DIAS_RESTANTES,
    FLOOR(Dias / 365) || ' anos, ' ||
    FLOOR(MOD(Dias, 365) / 30) || ' meses, ' ||
    MOD(MOD(Dias, 365), 30) || ' dias' AS TEMPO,
    YEAR(Data) AS PERIODO,
    Servidor->Funcional->CargoEfetivo->Descricao AS CARGOFUNCAO,
    Exercicio->Descricao AS UNIDADE,
    Servidor->Funcional->LotacaoExercicio->Gestor->Nome AS GESTOR_NOME,
    Servidor->Funcional->LotacaoExercicio->Gestor->Funcional->CargoFuncao->Descricao AS GESTOR_CARGO,
    Servidor->Funcional->LotacaoExercicio->Gestor->Matricula AS GESTOR_MATRICULA
FROM
    RHCadMovimentacao
WHERE
    Servidor->Matricula = ?
ORDER BY
    Data",
        exemplo = sprintf("
WITH m AS (
    SELECT *, COALESCE(DIAS, %2$s) AS D
    FROM %1$s
    WHERE TIPO_REGISTRO = 'MOVIMENTACAO' AND MATRICULA = ?
)
SELECT
    s.NOME, s.CPF, s.MATRICULA,
    m.DATAINICIO, m.DATAFIM,
    m.D AS DIAS,
    ABS(m.D) / 365 AS ANOS,
    (ABS(m.D) %% 365) / 30 AS MESES,
    (ABS(m.D) %% 365) %% 30 AS DIAS_RESTANTES,
    CASE WHEN m.D < 0 THEN '(-) ' ELSE '' END ||
    (ABS(m.D) / 365) || ' anos, ' ||
    ((ABS(m.D) %% 365) / 30) || ' meses, ' ||
    ((ABS(m.D) %% 365) %% 30) || ' dias' AS TEMPO,
    CAST(substr(m.DATAINICIO, 7, 4) AS INTEGER) AS PERIODO,
    m.CARGOFUNCAO, m.UNIDADE,
    s.GESTOR_NOME, s.GESTOR_CARGO, s.GESTOR_MATRICULA
FROM m
JOIN %1$s s ON s.TIPO_REGISTRO = 'SERVIDOR' AND s.MATRICULA = m.MATRICULA
ORDER BY %3$s", TABELA_EXEMPLO, .dias_ate_hoje("DATAINICIO"), .iso("m.DATAINICIO"))
    ),

    # Vínculo funcional — dados do servidor
    servidor = list(
        iris = "
SELECT
    Nome AS NOME,
    Pessoa->CpfFormatado AS CPF,
    Matricula AS MATRICULA,
    Funcional->CargoEfetivo->Descricao AS CARGO,
    Funcional->Classe AS CLASSENIVEL,
    Funcional->RegimeJuridico->Descricao AS REGIMEJUR,
    CASE
        WHEN Funcional->CargoEfetivo IS NOT NULL THEN 'CONCURSO'
        WHEN Funcional->CargoFuncao IS NOT NULL THEN 'NOMEAÇÃO'
        ELSE 'CONTRATAÇÃO OU OUTRA'
    END AS FORMAINGRESSO,
    Funcional->DataIngOrgaoFormatada AS DATAINGRESSO,
    Financeiro->DataDesligamento AS DATAFIM,
    Funcional->DataIngCargoEfetivoFormatada AS DATAINICIOEXERCICIO,
    Funcional->TipoServidor->Descricao AS SITUACAO,
    Funcional->LotacaoOrigem->Descricao AS LOTACAO,
    Funcional->LotacaoExercicio->Descricao AS LOTACAOEXERCICIO
FROM
    RHCADSERVIDOR
WHERE
    Matricula = ?",
        exemplo = sprintf("
SELECT
    NOME, CPF, MATRICULA, CARGO, CLASSENIVEL, REGIMEJUR, FORMAINGRESSO,
    DATAINGRESSO, DATAFIM, DATAINICIOEXERCICIO, SITUACAO, LOTACAO,
    LOTACAOEXERCICIO
FROM %s
WHERE TIPO_REGISTRO = 'SERVIDOR' AND MATRICULA = ?", TABELA_EXEMPLO)
    ),

    # Vínculo funcional — histórico de funções comissionadas
    funcoes_comissionadas = list(
        iris = "
SELECT
    DtPosseFormatada AS DATAINICIO,
    DtExoneracaoFormatada AS DATAFIM,
    CargoFuncao->Descricao AS CARGOFUNCAO,
    DocNomeacaoDesignacao_Tipo->Descricao || ' - '
    || DocNomeacaoDesignacao_PublicacaoNumero AS ATOADMIN
FROM
    RHCadFuncaoComissionada
WHERE
    Servidor->Matricula = ?
ORDER BY
    DtPosseFormatada",
        exemplo = sprintf("
SELECT DATAINICIO, DATAFIM, CARGOFUNCAO, ATOADMIN
FROM %s
WHERE TIPO_REGISTRO = 'FUNCAO_COMISSIONADA' AND MATRICULA = ?
ORDER BY %s", TABELA_EXEMPLO, .iso("DATAINICIO"))
    ),

    # Gestor da DGP (assinatura) — sem matrícula
    gestor_dgp = list(
        iris = "
SELECT
    Gestor->Nome AS GESTOR_NOME,
    Gestor->Funcional->CargoFuncao->Descricao AS GESTOR_CARGO,
    Gestor->Matricula AS GESTOR_MATRICULA,
    Email AS EMAIL,
    TelefonePrincipal AS TELEFONE
FROM
    RHTabLotacao
WHERE
    Sigla = 'DGP'",
        exemplo = sprintf("
SELECT GESTOR_NOME, GESTOR_CARGO, GESTOR_MATRICULA, EMAIL, TELEFONE
FROM %s
WHERE TIPO_REGISTRO = 'LOTACAO' AND SIGLA = 'DGP'", TABELA_EXEMPLO)
    )
)

# -----------------------------------------------------
# EXECUÇÃO
# -----------------------------------------------------
# fonte: "iris" ou "exemplo" (use resolver_fonte_dados() antes).
consultar_relatorio <- function(chave, matricula = NULL, distro = "MPRO", fonte = "iris") {

    sql <- SQL_RELATORIOS[[chave]]
    if (is.null(sql)) stop("Consulta desconhecida: ", chave)

    fonte <- match.arg(fonte, c("iris", "exemplo"))

    if (identical(fonte, "exemplo")) {
        con <- conectar_exemplo()
        on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

        if (is.null(matricula)) {
            DBI::dbGetQuery(con, sql$exemplo)
        } else {
            DBI::dbGetQuery(con, sql$exemplo, params = list(as.integer(matricula)))
        }
    } else {
        con <- conectar_banco(distro)
        on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

        if (is.null(matricula)) {
            DBI::dbGetQuery(con, sql$iris)
        } else {
            DBI::dbGetQuery(con, sql$iris, as.integer(matricula))
        }
    }
}

# Tela "Declaração" (antes em R/database.R).
consultar_matricula <- function(matricula_num, distro, fonte = "iris") {
    consultar_relatorio("cargo_efetivo", matricula_num, distro, fonte)
}

# Gestor da DGP (assinatura dos documentos): primeira linha, ou uma linha
# de NAs se a consulta falhar — mesmo comportamento que os .Rmd tinham.
consultar_gestor_dgp <- function(distro = "MPRO", fonte = "iris") {

    vazio <- data.frame(
        GESTOR_NOME = NA, GESTOR_CARGO = NA, GESTOR_MATRICULA = NA,
        EMAIL = NA, TELEFONE = NA,
        stringsAsFactors = FALSE
    )

    res <- tryCatch(
        consultar_relatorio("gestor_dgp", NULL, distro, fonte),
        error = function(e) {
            warning("Falha ao consultar o gestor da DGP: ", conditionMessage(e))
            vazio
        }
    )

    if (nrow(res) > 0) res[1, , drop = FALSE] else vazio
}

# =====================================================
# QUAL FONTE USAR — teste de acesso ao IRIS (com cache)
# =====================================================

.fonte_cache <- new.env(parent = emptyenv())

modo_fonte_configurado <- function() {
    modo <- tolower(trimws(Sys.getenv("DECLARASERV_FONTE_DADOS", unset = "auto")))
    if (modo %in% c("auto", "iris", "exemplo")) modo else "auto"
}

.num_env <- function(nome, padrao) {
    v <- suppressWarnings(as.numeric(Sys.getenv(nome, unset = "")))
    if (is.na(v) || v <= 0) padrao else v
}

# Teste rápido de rede (TCP) a partir da URL JDBC
# (jdbc:IRIS://host:porta/NAMESPACE): falha em segundos quando o servidor
# está inacessível, em vez de esperar o timeout longo do driver.
.porta_acessivel <- function(url, timeout) {

    m <- regmatches(url, regexec("^jdbc:[^:]+://([^:/]+):([0-9]+)", url, ignore.case = TRUE))[[1]]
    if (length(m) < 3) return(TRUE)   # formato desconhecido: deixa o driver decidir

    sock <- tryCatch(
        suppressWarnings(socketConnection(
            host = m[2], port = as.integer(m[3]), open = "r+b",
            blocking = TRUE, timeout = timeout
        )),
        error = function(e) NULL
    )

    if (is.null(sock)) return(FALSE)
    close(sock)
    TRUE
}

# Conecta de fato e executa uma consulta mínima (erro se não conseguir).
.testar_conexao_iris <- function(distro, timeout) {

    if (requireNamespace("rJava", quietly = TRUE)) {
        try({
            rJava::.jinit()
            rJava::.jcall("java/sql/DriverManager", "V", "setLoginTimeout", as.integer(timeout))
        }, silent = TRUE)
    }

    con <- conectar_banco(distro)
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

    DBI::dbGetQuery(con, "SELECT 1 AS OK")
    invisible(TRUE)
}

# TRUE/FALSE, com cache por empresa. O motivo da falha fica em
# motivo_iris_indisponivel(distro).
iris_acessivel <- function(distro, forcar = FALSE) {

    distro <- toupper(trimws(distro))
    ttl <- .num_env("DECLARASERV_IRIS_TTL_MIN", 5) * 60
    item <- .fonte_cache[[distro]]

    if (!forcar && !is.null(item) &&
        as.numeric(difftime(Sys.time(), item$quando, units = "secs")) < ttl) {
        return(item$ok)
    }

    timeout <- .num_env("DECLARASERV_IRIS_TIMEOUT_SEG", 5)

    resultado <- tryCatch({

        url <- Sys.getenv(distro_env(distro, "IRIS_URL"), unset = "")
        if (!nzchar(url)) stop("URL do IRIS não configurada (", distro_env(distro, "IRIS_URL"), ").")

        if (!.porta_acessivel(url, timeout)) {
            stop("servidor IRIS inacessível na rede (", sub("^jdbc:[^:]+://", "", url), ").")
        }

        .testar_conexao_iris(distro, timeout)

        list(ok = TRUE, motivo = NULL)

    }, error = function(e) list(ok = FALSE, motivo = conditionMessage(e)))

    assign(distro, c(resultado, list(quando = Sys.time())), envir = .fonte_cache)

    resultado$ok
}

motivo_iris_indisponivel <- function(distro) {
    item <- .fonte_cache[[toupper(trimws(distro))]]
    if (is.null(item) || isTRUE(item$ok)) NULL else item$motivo
}

# "auto" (ou vazio) -> decide pelo modo configurado e pelo teste de
# acesso; "iris"/"exemplo" -> devolve como veio.
resolver_fonte_dados <- function(fonte = "auto", distro = "MPRO") {

    fonte <- tolower(trimws(if (is.null(fonte) || is.na(fonte)) "auto" else fonte))
    if (fonte %in% c("iris", "exemplo")) return(fonte)

    modo <- modo_fonte_configurado()
    if (modo != "auto") return(modo)

    if (iris_acessivel(distro)) "iris" else "exemplo"
}

# =====================================================
# AVISO "DOCUMENTO DE EXEMPLO" (usado pelos .Rmd)
# -----------------------------------------------------
# Faixa fixa no topo + marca d'água em todas as páginas do documento
# gerado com dados de exemplo — para que ele nunca seja confundido com
# um documento oficial.
# =====================================================
html_aviso_exemplo_documento <- function() {
    paste0(
        "<style>",
        ".aviso-exemplo-doc{position:sticky;top:0;z-index:1000;background:#b45309;color:#fff;",
        "text-align:center;font:700 14px/1.4 Arial,sans-serif;letter-spacing:.04em;",
        "padding:10px 12px;margin:-40px -24px 24px -24px;}",
        ".marca-exemplo-doc{position:fixed;top:45%;left:50%;transform:translate(-50%,-50%) rotate(-30deg);",
        "font:700 110px/1 Arial,sans-serif;color:rgba(180,83,9,.12);pointer-events:none;",
        "z-index:999;white-space:nowrap;}",
        "@media print{.aviso-exemplo-doc{position:static;}}",
        "</style>",
        "<div class='aviso-exemplo-doc'>DOCUMENTO DE EXEMPLO — DADOS FICTÍCIOS — SEM VALIDADE</div>",
        "<div class='marca-exemplo-doc'>EXEMPLO</div>"
    )
}
