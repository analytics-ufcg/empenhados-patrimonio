library(readr)
library(dplyr)

# Dados de declaração de bens para 2012 e 2016
declaracao_2012 <- read_delim("data/bem_candidato_2012_PB.txt", delim = ";", col_names = FALSE, col_types = "cciccciccncc",
                              locale = locale(encoding = "latin1"))

colunas_declaracao <- c("dataGeracao", "horaGeracao", "anoEleicao", "descEleicao", "siglaUF", "sequencialCandidato", "codTipoBem", "descricaoTipoBem", 
                        "detalheBem", "valorBem", "dataUltimaAtualizacao", "horaUltimaAtualizacao")

colnames(declaracao_2012) <- colunas_declaracao


declaracao_2016 <- read_delim("data/bem_candidato_2016_PB.txt", delim = ";", col_names = FALSE, col_types = "cciccciccncc",
                              locale = locale(encoding = "latin1"))

colnames(declaracao_2016) <- colunas_declaracao


# Dados de candidatos para 2012 e 2016

candidatos_2012 <- read_delim("consulta_cand_2012_PB.txt", delim = ";", col_names = FALSE,
                              locale = locale(encoding = "latin1"),
                              col_types = "cciccccciccccccicicccccciccccicicicicccccicc")

colnames(candidatos_2012) <- c("dataGeracao", "horaGeracao", "anoEleicao", "numTurno", "descEleicao", "siglaUF", "siglaUnidEleitoral",
                               "descUnidEleitoral", "codCargo", "descCargo", "nomeCandidato", "sequencialCandidato", "numeroCandidato", 
                               "cpfCandidato", "nomeUrnaCandidato", "codSituacaoCandidatura", "descSituacaoCandidatura", "numeroPartido", 
                               "siglaPartido", "nomePartido", "codLegenda", "siglaLegenda", "composicaoLegenda", "nomeLegenda", "codOcupacao",
                               "descOcupacao", "dataNascimento", "numTituloEleitoralCand", "idadeCandDataEleicao", "codSexo", "descSexo", 
                               "codGrauInstrucao", "descGrauInstrucao", "codEstadoCivil", "descEstadoCivil", "codNacionalidade", 
                               "descNacionalidade", "siglaUFNasc", "codMunicipioNasc", "nomeMunicipioNasc", "despesaMaxCampanha", 
                               "codSituacaoEleito", "descSituacaoEleito", "email")


candidatos_2016 <- read_delim("data/consulta_cand_2016_PB.txt", delim = ";", col_names = FALSE,
                              locale = locale(encoding = "latin1"),
                              col_types = "cciccccciccccccicicccccciccccicicicicicccccicc")

colnames(candidatos_2016) <- c("dataGeracao", "horaGeracao", "anoEleicao", "numTurno", "descEleicao", "siglaUF", "siglaUnidEleitoral",
                               "descUnidEleitoral", "codCargo", "descCargo", "nomeCandidato", "sequencialCandidato", "numeroCandidato", 
                               "cpfCandidato", "nomeUrnaCandidato", "codSituacaoCandidatura", "descSituacaoCandidatura", "numeroPartido", 
                               "siglaPartido", "nomePartido", "codLegenda", "siglaLegenda", "composicaoLegenda", "nomeLegenda", "codOcupacao",
                               "descOcupacao", "dataNascimento", "numTituloEleitoralCand", "idadeCandDataEleicao", "codSexo", "descSexo", 
                               "codGrauInstrucao", "descGrauInstrucao", "codEstadoCivil", "descEstadoCivil", "codCorRaca", "descCorRaca",
                               "codNacionalidade", "descNacionalidade", "siglaUFNasc", "codMunicipioNasc", "nomeMunicipioNasc", 
                               "despesaMaxCampanha", "codSituacaoEleito", "descSituacaoEleito", "email")


