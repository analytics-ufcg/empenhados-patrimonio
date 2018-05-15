library(here)
library(tidyverse)
source(here("code/load_historico.R"))
source(here("code/import_tse_utils.R"))

# Depois de descompactar consulta_cand_2016.zip e bem_candidato_2012.zip 
# tal qual eles vêm do TSE via get_data.sh

estados = c("AC" , "AL" , "AM" , "AP" , "BA" , "CE" , "ES" , "GO" , "MA" , "MG" , "MS" , "MT" , "PA" , "PB" , "PE" , "PI" , "PR" , "RJ" , "RN" , "RO" , "RR" , "RS" , "SC" , "SE" , "SP" , "TO")

consulta_candidatos_all <- data_frame()

for (ano in c(2008, 2010, 2012, 2014, 2016)) {
  for(estado in estados) {
    message("Lendo dados: ", ano, ", ", estado)
    dataPath <- cria_nome_tse("candidato", ano, estado)
    consulta_candidatos_all <- consulta_candidatos_all %>%
      rbind(importCandidatos(dataPath, ano))
  }
}


define_situacao_eleito <- function(descSituacaoEleito) {
  
  situacao_um_eleito <- function(descSituacaoEleito) {
    eleito <- c("ELEITO", "ELEITO POR QP", "ELEITO POR MÉDIA", "MÉDIA")
    
    nao_eleito <- c("RENÚNCIA/FALECIMENTO/CASSAÇÃO ANTES DA ELEIÇÃO", "NÃO ELEITO",
                    "RENÚNCIA/FALECIMENTO/CASSAÇÃO APÓS A ELEIÇÃO", "REGISTRO NEGADO ANTES DA ELEIÇÃO",
                    "REGISTRO NEGADO APÓS A ELEIÇÃO", "SUBSTITUÍDO", "INDEFERIDO COM RECURSO", "CASSADO COM RECURSO")
    
    # suplente <- c("SUPLENTE")
    # segundo_turno <- c("2º TURNO")
    # indefinido <- c("#NE#", "#NULO#", NA)
    
    if (is.na(descSituacaoEleito)) {
      situacao = "INDEFINIDO"
    } else if (descSituacaoEleito %in% eleito) {
      situacao = "ELEITO"
    } else if (descSituacaoEleito %in% nao_eleito) {
      situacao = "NÃO ELEITO"
    } else if (descSituacaoEleito == "2º TURNO") {
      situacao = "2º TURNO"
    } else if (descSituacaoEleito == "SUPLENTE") {
      situacao = "SUPLENTE"
    } else {
      situacao = "INDEFINIDO"
    }
    
    return(situacao)
  }
  
  situacao = c()
  for (i in 1:length(descSituacaoEleito)) {
    situacao[i] <- situacao_um_eleito(descSituacaoEleito[i])
  }
  
  return(situacao)
  
}

consulta_candidatos_all <- consulta_candidatos_all %>%
  mutate(situacaoTotalizacaoEleicao = define_situacao_eleito(descSituacaoEleito))

consulta_candidatos_all %>% group_by(situacaoTotalizacaoEleicao) %>% summarise(n())

consulta_candidatos_all %>% 
  group_by(codSituacaoCandidatura, descSituacaoCandidatura) %>%
  summarise(n()) %>%
  View()

consulta_candidatos_all %>% 
  group_by(codSituacaoEleito, descSituacaoEleito) %>%
  summarise(n()) %>%
  View()

consulta_candidatos_all %>% 
  filter(is.na(codSituacaoCandidatura), !(descSituacaoCandidatura %in% codSituacaoCandidatura)) %>%
  # group_by(descSituacaoCandidatura) %>%
  # summarise(quantidade = n()) %>% 
  View()

cod_desc_situacaoCandidatura <- consulta_candidatos_all %>%
  filter(!is.na(codSituacaoCandidatura)) %>%
  group_by(codSituacaoCandidatura, descSituacaoCandidatura) %>%
  summarise(n())


apto <- c(2, 4, 16, 17, 18, 19)
inapto <- c(5, 6, 7, 10, 13, 14)

# indefinidos ainda = 
# 8 AGUARDANDO JULGAMENTO
# 9 INELEGÍVEL
# 11 IMPUGNADO


consulta_candidatos_all %>%
  mutate()



