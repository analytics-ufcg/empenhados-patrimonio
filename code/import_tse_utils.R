library(readr)
library(dplyr)


importDecalaracao <- function(dataPath) {
    declaracao <- read_delim(dataPath, delim = ";", col_names = FALSE, col_types = "cciccciccncc",
                                  locale = locale(encoding = "latin1")) 
    
    colunas_declaracao <- c("dataGeracao", "horaGeracao", "anoEleicao", "descEleicao", "siglaUF", 
                            "sequencialCandidato", "codTipoBem", "descricaoTipoBem", 
                            "detalheBem", "valorBem", "dataUltimaAtualizacao", "horaUltimaAtualizacao")
    
    colnames(declaracao) <- colunas_declaracao
    
    return(declaracao)
}

importCandidatos <- function(dataPath, ano) {
    if(ano == 2008){
        return(importCandidatos2008(dataPath))
    } else if (ano == 2010) {
        return(importCandidatos2010(dataPath))
    } else if (ano == 2012) {
        return(importCandidatos2012(dataPath))
    } else if (ano == 2014) {
        return(importCandidatos2014(dataPath))
    }
    return(importCandidatos2016(dataPath))
}
importCandidatos2008 <- function(dataPath){
    candidatos_2008 <- read_delim(dataPath, delim = ";", col_names = FALSE, 
                                  col_types = "cciccciciccciccicicciccciccciicicicicccciic",
                                  locale = locale(encoding = "latin1"))

    colunas_candidatos <- c("dataGeracao", "horaGeracao", "anoEleicao", "numTurno", "descEleicao",  "siglaUF", 
                            "siglaUnidEleitoral", "descUnidEleitoral", "codCargo", "descCargo", "nomeCandidato", "sequencialCandidato", 
                            "numeroCandidato", "cpfCandidato","nomeUrnaCandidato", "codSituacaoCandidatura", "descSituacaoCandidatura", 
                            "numeroPartido", "siglaPartido", "nomePartido", "codLegenda", "siglaLegenda", 
                            "composicaoLegenda", "nomeLegenda", "codOcupacao", "descOcupacao", "dataNascimento", 
                            "numTituloEleitoralCand", "idadeCandDataEleicao", "codSexo", "descSexo", 
                            "codGrauInstrucao", "descGrauInstrucao", "codEstadoCivil", "descEstadoCivil", 
                            "codNacionalidade", "descNacionalidade", "siglaUFNasc", "codMunicipioNasc", 
                            "nomeMunicipioNasc", "despesaMaxCampanha", "codSituacaoEleito", "descSituacaoEleito")

    colnames(candidatos_2008) <- colunas_candidatos
    
    candidatos_2008 <- candidatos_2008 %>%
        mutate(email = "#NE") %>%
        mutate(codCorRaca = -3) %>%
        mutate(descCorRaca = "#NE") %>%
        select(colunas_candidatos[1:35], "codCorRaca", "descCorRaca", colunas_candidatos[36:43], "email")
    
    return(candidatos_2008)
}

importCandidatos2010 <- function(dataPath){
  candidatos_2010 <- read_delim(dataPath, delim = ";", col_names = FALSE, 
                                col_types = "cciicccciccciccicicccccciccciicicicicccciic",
                                locale = locale(encoding = "latin1"))
  
  colunas_candidatos <- c("dataGeracao", "horaGeracao", "anoEleicao", "numTurno", "descEleicao",  "siglaUF", 
                          "siglaUnidEleitoral", "descUnidEleitoral", "codCargo", "descCargo", "nomeCandidato", "sequencialCandidato", 
                          "numeroCandidato", "cpfCandidato","nomeUrnaCandidato", "codSituacaoCandidatura", "descSituacaoCandidatura", 
                          "numeroPartido", "siglaPartido", "nomePartido", "codLegenda", "siglaLegenda", 
                          "composicaoLegenda", "nomeLegenda", "codOcupacao", "descOcupacao", "dataNascimento", 
                          "numTituloEleitoralCand", "idadeCandDataEleicao", "codSexo", "descSexo", 
                          "codGrauInstrucao", "descGrauInstrucao", "codEstadoCivil", "descEstadoCivil", 
                          "codNacionalidade", "descNacionalidade", "siglaUFNasc", "codMunicipioNasc", 
                          "nomeMunicipioNasc", "despesaMaxCampanha", "codSituacaoEleito", "descSituacaoEleito")
  
  
  colnames(candidatos_2010) <- colunas_candidatos
  
  candidatos_2010 <- candidatos_2010 %>%
    mutate(email = "#NE") %>%
    mutate(codCorRaca = -3) %>%
    mutate(descCorRaca = "#NE") %>%
    select(colunas_candidatos[1:35], "codCorRaca", "descCorRaca", colunas_candidatos[36:43], "email")
  
  return(candidatos_2010)
}

importCandidatos2012 <- function(dataPath){
    candidatos_2012 <- read_delim(dataPath, delim = ";", col_names = FALSE,
                                  col_types = "cciccccciccccccicicccccciccccicicicicccccicc",
                                  locale = locale(encoding = "latin1"))
    
    colunas_candidatos <- c("dataGeracao", "horaGeracao", "anoEleicao", "numTurno", "descEleicao", "siglaUF", "siglaUnidEleitoral",
                                   "descUnidEleitoral", "codCargo", "descCargo", "nomeCandidato", "sequencialCandidato", "numeroCandidato", 
                                   "cpfCandidato", "nomeUrnaCandidato", "codSituacaoCandidatura", "descSituacaoCandidatura", "numeroPartido", 
                                   "siglaPartido", "nomePartido", "codLegenda", "siglaLegenda", "composicaoLegenda", "nomeLegenda", "codOcupacao",
                                   "descOcupacao", "dataNascimento", "numTituloEleitoralCand", "idadeCandDataEleicao", "codSexo", "descSexo", 
                                   "codGrauInstrucao", "descGrauInstrucao", "codEstadoCivil", "descEstadoCivil", "codNacionalidade", 
                                   "descNacionalidade", "siglaUFNasc", "codMunicipioNasc", "nomeMunicipioNasc", "despesaMaxCampanha", 
                                   "codSituacaoEleito", "descSituacaoEleito", "email")
    colnames(candidatos_2012) <- colunas_candidatos
    candidatos_2012 <- candidatos_2012 %>%
        mutate(codCorRaca = -3) %>%
        mutate(descCorRaca = "#NE") %>%
        select(colunas_candidatos[1:35], "codCorRaca", "descCorRaca", colunas_candidatos[36:44])
    return(candidatos_2012)
}

importCandidatos2014 <- function(dataPath){
    return(importCandidatos2016(dataPath))
}

importCandidatos2016 <- function(dataPath){
  candidatos_2016 <- read_delim(dataPath, delim = ";", col_names = FALSE,
                                locale = locale(encoding = "latin1"),
                                col_types = "cciccccciccccccicicccccciccccicicicicicccccicc")
  
  colunas_candidatos <- c("dataGeracao", "horaGeracao", "anoEleicao", "numTurno", "descEleicao", "siglaUF", "siglaUnidEleitoral",
                                 "descUnidEleitoral", "codCargo", "descCargo", "nomeCandidato", "sequencialCandidato", "numeroCandidato", 
                                 "cpfCandidato", "nomeUrnaCandidato", "codSituacaoCandidatura", "descSituacaoCandidatura", "numeroPartido", 
                                 "siglaPartido", "nomePartido", "codLegenda", "siglaLegenda", "composicaoLegenda", "nomeLegenda", "codOcupacao",
                                 "descOcupacao", "dataNascimento", "numTituloEleitoralCand", "idadeCandDataEleicao", "codSexo", "descSexo", 
                                 "codGrauInstrucao", "descGrauInstrucao", "codEstadoCivil", "descEstadoCivil", "codCorRaca", "descCorRaca",
                                 "codNacionalidade", "descNacionalidade", "siglaUFNasc", "codMunicipioNasc", "nomeMunicipioNasc", 
                                 "despesaMaxCampanha", "codSituacaoEleito", "descSituacaoEleito", "email")
  colnames(candidatos_2016) <- colunas_candidatos
  return(candidatos_2016)
  
}
