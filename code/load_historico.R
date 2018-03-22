read_historico_tse <- function(){
    library(dplyr)
    source(here("code/import_data_candidatos_bens.R"))
    
    declaracao_2012 <- importDecalaracao2012("../data/bem_candidato_2012_PB.txt")
    candidatos_2012 <- importCandidatos2012("../data/consulta_cand_2012_PB.txt")
    declaracao_2016 <- importDecalaracao2016("../data/bem_candidato_2016_PB.txt")
    candidatos_2016 <- importCandidatos2016("../data/consulta_cand_2016_PB.txt")
    
    prefeitos_atuais <- candidatos_2016 %>% 
      filter(codCargo == 11, codSituacaoEleito %in% c(1, 2, 3)) %>% 
      select(sequencialCandidato2016 = sequencialCandidato, siglaUnidEleitoral, descUnidEleitoral, nomeCandidato, nomeUrnaCandidato, siglaPartido, 
             codCargo, descCargo, cpfCandidato, descSituacaoEleito)
    
    historico_prefeitos_atuais <- prefeitos_atuais %>% 
      left_join(
        candidatos_2012 %>% 
          select(sequencialCandidato2012 = sequencialCandidato, cpfCandidato, codCargo2012 = codCargo, descCargo2012 = descCargo, 
                 descSituacaoEleito2012 = descSituacaoEleito, codSituacaoEleito2012 = codSituacaoEleito),
        by = c("cpfCandidato")
      )
    
    declaracao_prefeitos_atuais2012 <- historico_prefeitos_atuais %>% 
      select(sequencialCandidato2012) %>% 
      left_join(declaracao_2012 %>% select(sequencialCandidato, valorBem),
                by = c("sequencialCandidato2012" = "sequencialCandidato")) %>% 
      
      group_by(sequencialCandidato2012) %>% 
      summarise(totalBens2012 = sum(valorBem))
    
    declaracao_prefeitos_atuais2016 <- historico_prefeitos_atuais %>% 
      select(sequencialCandidato2016) %>% 
      left_join(declaracao_2016 %>% select(sequencialCandidato, valorBem),
                by = c("sequencialCandidato2016" = "sequencialCandidato")) %>% 
      
      group_by(sequencialCandidato2016) %>% 
      summarise(totalBens2016 = sum(valorBem))
    
    historico_bens_prefeitos_atuais <- historico_prefeitos_atuais %>% 
      left_join(declaracao_prefeitos_atuais2012) %>% 
      left_join(declaracao_prefeitos_atuais2016) %>% 
      filter(codSituacaoEleito2012 != 6 | is.na(codSituacaoEleito2012)) %>% 
      filter(!(cpfCandidato == "34303197491" & codSituacaoEleito2012 == -1))
    
    # Caso particular de JOSE FERNANDES GORGONHO NETO em 2012 (foi candidato a prefeito em 2012 mas teve sua campanha renunciada)
    # Foi removido as ocorrências de segundo turno também

    return(historico_bens_prefeitos_atuais)
} 