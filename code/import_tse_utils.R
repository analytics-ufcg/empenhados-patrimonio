library(readr)
library(dplyr)


importDecalaracao <- function(dataPath, ano) {
    if (ano %in% c(2014, 2016, 2018)) {
        return(importDecalaracao2018(dataPath))
    } else {
        declaracao <- read_delim(dataPath, delim = ";", col_names = FALSE, col_types = "cciccciccncc",
                                 locale = locale(encoding = "latin1")) 
        
        colunas_declaracao <- c("dataGeracao", "horaGeracao", "anoEleicao", "descEleicao", "siglaUF", 
                                "sequencialCandidato", "codTipoBem", "descricaoTipoBem", 
                                "detalheBem", "valorBem", "dataUltimaAtualizacao", "horaUltimaAtualizacao")
        
        colnames(declaracao) <- colunas_declaracao
        
        return(declaracao)
    }
}

importDecalaracao2018 <- function(dataPath) {
    declaracao_2018 <- read_csv2(dataPath, col_names = TRUE, col_types = "cciiciccccccciccncc",
                             locale = locale(encoding = "latin1"))
    
    colunas_declaracao <- c("dataGeracao", "horaGeracao", "anoEleicao", "codTipoEleicao", "descTipoEleicao", "codEleicao", "descEleicao", 
                            "dataEleicao", "siglaUF", "siglaUE", "descUE", "sequencialCandidato", "numOrdemCandidato",
                            "codTipoBem", "descricaoTipoBem", "detalheBem", "valorBem", "dataUltimaAtualizacao", "horaUltimaAtualizacao")
    
    colnames(declaracao_2018) <- colunas_declaracao
    
    declaracao_2018 <- declaracao_2018 %>% 
        select("dataGeracao", "horaGeracao", "anoEleicao", "descEleicao", "siglaUF", 
               "sequencialCandidato", "codTipoBem", "descricaoTipoBem", 
               "detalheBem", "valorBem", "dataUltimaAtualizacao", "horaUltimaAtualizacao")
    
    return(declaracao_2018)
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
    } else if (ano == 2016) {
        return(importCandidatos2016(dataPath))    
    }
    return(importCandidatos2018(dataPath))
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
    return(importCandidatos2018(dataPath))
}

importCandidatos2016 <- function(dataPath){
    return(importCandidatos2018(dataPath))
}

importCandidatos2018 <- function(dataPath){
    candidatos_2018 <- read_delim(dataPath, delim = ";", col_names = TRUE,
                                  locale = locale(encoding = "latin1"),
                                  col_types = "cciicciccccccicccccccciciccicccccicccccicicicicicicniccccc")
    
    colunas_candidatos <- c("dataGeracao", "horaGeracao", "anoEleicao", "codTipoEleicao", "descTipoEleicao", "numTurno", "codEleicao", "descEleicao", 
                            "dataEleicao", "tipoAbrangencia", "siglaUF", "siglaUnidEleitoral",
                            "descUnidEleitoral", "codCargo", "descCargo", "sequencialCandidato", "numeroCandidato", "nomeCandidato",
                            "nomeUrnaCandidato", "nomeSocialCandidato", "cpfCandidato", "email", "codSituacaoCandidatura", "descSituacaoCandidatura", 
                            "codDetalheSituacaoCand", "descDetalheSituacaoCand", "tipoAgremiacao", "numeroPartido", "siglaPartido", 
                            "nomePartido", "codLegenda", "nomeLegenda", "composicaoLegenda", "codNacionalidade", "descNacionalidade",
                            "siglaUFNasc", "codMunicipioNasc", "nomeMunicipioNasc", "dataNascimento", "idadeCandDataEleicao", "numTituloEleitoralCand",
                            "codSexo", "descSexo", "codGrauInstrucao", "descGrauInstrucao","codEstadoCivil", "descEstadoCivil", "codCorRaca", "descCorRaca",
                            "codOcupacao", "descOcupacao", "despesaMaxCampanha", "codSituacaoEleito", "descSituacaoEleito", "situacaoReeleicao",
                            "situacaoDeclararBens", "numProtocoloCandidatura", "numProcesso")
    
    colnames(candidatos_2018) <- colunas_candidatos
    
    candidatos_2018 <- candidatos_2018 %>% 
        mutate(siglaLegenda = "#NE#") %>%
        select("dataGeracao", "horaGeracao", "anoEleicao", "numTurno", "descEleicao",            
               "siglaUF", "siglaUnidEleitoral", "descUnidEleitoral", "codCargo", "descCargo",
               "nomeCandidato", "sequencialCandidato", "numeroCandidato", "cpfCandidato", "nomeUrnaCandidato",
               "codSituacaoCandidatura", "descSituacaoCandidatura", "numeroPartido", "siglaPartido", "nomePartido",
               "codLegenda", "siglaLegenda", "composicaoLegenda", "nomeLegenda", "codOcupacao",
               "descOcupacao", "dataNascimento", "numTituloEleitoralCand", "idadeCandDataEleicao", "codSexo",
               "descSexo", "codGrauInstrucao", "descGrauInstrucao", "codEstadoCivil", "descEstadoCivil",        
               "codCorRaca", "descCorRaca", "codNacionalidade", "descNacionalidade", "siglaUFNasc",
               "codMunicipioNasc", "nomeMunicipioNasc", "despesaMaxCampanha", "codSituacaoEleito", "descSituacaoEleito", "email")
        
    return(candidatos_2018)
}
