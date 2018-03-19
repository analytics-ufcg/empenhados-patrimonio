library(readr)
library(dplyr)

importDecalaracao2008 <- function(dataPath) {
  declaracao_2008 <- read_delim(dataPath, delim = ";", col_names = FALSE, col_types = "cciccciccncc",
                                locale = locale(encoding = "latin1")) 
  
  colunas_declaracao <- c("dataGeracao", "horaGeracao", "anoEleicao", "descEleicao", "siglaUF", 
                         "sequencialCandidato", "codTipoBem", "descricaoTipoBem", 
                         "detalheBem", "valorBem", "dataUltimaAtualizacao", "horaUltimaAtualizacao")
  
  colnames(declaracao_2008) <- colunas_declaracao

  return(declaracao_2008)
}


importDecalaracao2012 <- function(dataPath){
  declaracao_2012 <- read_delim(dataPath, delim = ";", col_names = FALSE, col_types = "cciccciccncc",
                                locale = locale(encoding = "latin1"))
  
  colunas_declaracao <- c("dataGeracao", "horaGeracao", "anoEleicao", "descEleicao", "siglaUF", "sequencialCandidato", "codTipoBem", "descricaoTipoBem", 
                          "detalheBem", "valorBem", "dataUltimaAtualizacao", "horaUltimaAtualizacao")
  
  colnames(declaracao_2012) <- colunas_declaracao
  
  return(declaracao_2012)
}

importDecalaracao2016 <- function(dataPath){
  colunas_declaracao <- c("dataGeracao", "horaGeracao", "anoEleicao", "descEleicao", "siglaUF", "sequencialCandidato", "codTipoBem", "descricaoTipoBem", 
                          "detalheBem", "valorBem", "dataUltimaAtualizacao", "horaUltimaAtualizacao")

  declaracao_2016 <- read_delim(dataPath, delim = ";", col_names = FALSE, col_types = "cciccciccncc",
                                locale = locale(encoding = "latin1"))
  
  colnames(declaracao_2016) <- colunas_declaracao
  
  return(declaracao_2016)
  
}

importCandidatos2008 <- function(dataPath){
  candidatos_2008 <- read_delim(dataPath, delim = ";", col_names = FALSE, 
                                col_types = "cciicccciccciccicicciccciccccicicicicccciic",
                                locale = locale(encoding = "latin1"))
  
  colunas_candidatos <- c("dataGeracao", "horaGeracao", "anoEleicao", "numTurno", "descEleicao",  "siglaUF", 
                          "siglaUE", "descricaoUE", "codCargo", "descCargo", "nomeCandidato", "sequencialCandidato", 
                          "numeroCandidato", "cpfCandidato","nomeUrnaCandidato", "codSituacaoCandidatura", "desSituacaoCandidatura", 
                          "numeroPartido", "siglaPartido", "nomePartido", "codLegenda", "siglaLegenda", 
                          "composicaoLegenda", "nomeLegenda", "codOcupacao", "descOcupacao", "dataNascimento", 
                          "numTituloEleitoralCand", "idadeCandDataEleicao", "codSexo", "descSexo", 
                          "codGrauInstrucao", "descGrauInstrucao", "codEstadoCivil", "descEstadoCivil", 
                          "codNacionalidade", "descNacionalidade", "siglaUFNasc", "codMunicipioNasc", 
                          "nomeMunicipioNasc", "despesaMaxCampanha", "codSituacaoTurno", "descSituacaoTurno")
  colnames(candidatos_2008) <- colunas_candidatos
  
  return(candidatos_2008)
}


importCandidatos2012 <- function(dataPath){
  candidatos_2012 <- read_delim(dataPath, delim = ";", col_names = FALSE,
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
  
  return(candidatos_2012)
}

importCandidatos2016 <- function(dataPath){
  
  candidatos_2016 <- read_delim(dataPath, delim = ";", col_names = FALSE,
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
  return(candidatos_2016)
  
}

