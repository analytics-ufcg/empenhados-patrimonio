library(here)
library(tidyverse)
source(here("code/load_historico.R"))
source(here("code/import_tse_utils.R"))

# Use o script get_data.sh para baixar os dados
# Descompacte todos os zips baixados 
# Execute o script clean_data.sh para realizar limpeza nos 
# Agora você pode executar o código abaixo

gera_consulta_candidados_all <- function(){
    
  cria_nome_tse = function(tipo, ano, estado) {
      extensao = ".txt"
      if (ano == 2018) {
          extensao = ".csv"
      } 
      
      prefix = ifelse(tipo == "bem", "bem_candidato_", "consulta_cand_")
      here::here(paste0("data/",
                        prefix,
                        ano,
                        "/",
                        prefix,
                        ano,
                        "_",
                        estado,
                        extensao)) %>% 
          return()
  }

  estados_mun = c("AC" , "AL" , "AM" , "AP" , "BA" , "CE" , "ES" , "GO" , "MA" , "MG" , "MS" , "MT" , "PA" , "PB" , "PE" , "PI" , "PR" , "RJ" , "RN" , "RO" , "RR" , "RS" , "SC" , "SE" , "SP" , "TO")
  estados_df_br = c("AC" , "AL" , "AM" , "AP" , "BA" , "BR", "CE", "DF", "ES" , "GO" , "MA" , "MG" , "MS" , "MT" , "PA" , "PB" , "PE" , "PI" , "PR" , "RJ" , "RN" , "RO" , "RR" , "RS" , "SC" , "SE" , "SP" , "TO")
  
  consulta_candidatos_all <- data_frame()
  bens_candidatos_all <- data_frame()
  
  for (ano in c(2008, 2010, 2012, 2014, 2016, 2018)) {
    
    if (ano %in% c(2010, 2014, 2018)) {
      estados = estados_df_br
    } else {
      estados = estados_mun
    }
    
    for(estado in estados) {
      message("Lendo dados: ", ano, ", ", estado)
      message("Lendo candidaturas")
      dataPath <- cria_nome_tse("candidato", ano, estado)
      
      consulta_candidatos_all <- consulta_candidatos_all %>%
        rbind(importCandidatos(dataPath, ano))
      
      message("Lendo Bens")
      dataPath_bens <- cria_nome_tse("bem", ano, estado)
      bens_candidatos_all <- bens_candidatos_all %>%
        rbind(importDecalaracao(dataPath_bens, ano))
    }
  }
  
  return(list(consulta_candidatos_all = consulta_candidatos_all, bens_candidatos_all = bens_candidatos_all))
  
}

candidatos_all <- gera_consulta_candidados_all()

consulta_candidatos_all <- candidatos_all$consulta_candidatos_all
bens_candidatos_all <- candidatos_all$bens_candidatos_all

bens_seq_2008 <- bens_candidatos_all %>%
  filter(anoEleicao == 2008) %>%
  select(sequencialCandidato)

candidatos_seq_2008 <- consulta_candidatos_all %>%
  filter(anoEleicao == 2008) %>%
  select(sequencialCandidato)

patrimonio_candidatos_all <- bens_candidatos_all %>%
  mutate(descEleicao = toupper(descEleicao)) %>%
  group_by(descEleicao, siglaUF, sequencialCandidato) %>%
  summarise(totalBens = sum(valorBem))


consulta_candidatos_all <- consulta_candidatos_all %>% 
  mutate(descEleicao = toupper(descEleicao)) %>%
  left_join(patrimonio_candidatos_all,by = c('descEleicao', 'siglaUF', 'sequencialCandidato')) 


apto <- c(2, 4, 8, 16, 17, 18, 19)
inapto <- c(5, 6, 7, 9, 10, 11, 13, 14)

eleito <- c("ELEITO", "ELEITO POR QP", "ELEITO POR MÉDIA", "MÉDIA")

nao_eleito <- c("SUPLENTE", "RENÚNCIA/FALECIMENTO/CASSAÇÃO ANTES DA ELEIÇÃO", "NÃO ELEITO",
                "RENÚNCIA/FALECIMENTO/CASSAÇÃO APÓS A ELEIÇÃO", "REGISTRO NEGADO ANTES DA ELEIÇÃO",
                "REGISTRO NEGADO APÓS A ELEIÇÃO", "SUBSTITUÍDO", "INDEFERIDO COM RECURSO", "CASSADO COM RECURSO")

consulta_candidatos_all <- consulta_candidatos_all %>%
  mutate(codSituacaoCandidatura = ifelse(is.na(codSituacaoCandidatura), descSituacaoCandidatura, codSituacaoCandidatura)) %>%
  mutate(descSituacaoCandidatura = case_when(
    codSituacaoCandidatura == 2 ~ "DEFERIDO",
    codSituacaoCandidatura == 4 ~ "INDEFERIDO COM RECURSO",
    codSituacaoCandidatura == 5 ~ "CANCELADO",
    codSituacaoCandidatura == 6 ~ "RENÚNCIA",
    codSituacaoCandidatura == 7 ~ "FALECIDO",
    codSituacaoCandidatura == 8 ~ "AGUARDANDO JULGAMENTO",
    codSituacaoCandidatura == 9 ~ "INELEGÍVEL",
    codSituacaoCandidatura == 10 ~ "CASSADO",
    codSituacaoCandidatura == 11 ~ "IMPUGNADO",
    codSituacaoCandidatura == 13 ~ "NÃO CONHECIMENTO DO PEDIDO",
    codSituacaoCandidatura == 14 ~ "INDEFERIDO",
    codSituacaoCandidatura == 16 ~ "DEFERIDO COM RECURSO",
    codSituacaoCandidatura == 17 ~ "PENDENTE DE JULGAMENTO",
    codSituacaoCandidatura == 18 ~ "CASSADO COM RECURSO",
    codSituacaoCandidatura == 19 ~ "CANCELADO COM RECURSO",
    is.na(codSituacaoCandidatura) ~ "#NE#",
    TRUE ~ codSituacaoCandidatura
  )) %>%
  mutate(classeSituacaoCandidatura = case_when(
    codSituacaoCandidatura %in% apto ~ "APTO",
    codSituacaoCandidatura %in% inapto ~ "INAPTO",
    TRUE ~ "INDEFINIDO"
    
  )) %>%
  mutate(classeSituacaoEleicao = case_when(
    is.na(descSituacaoEleito) ~ "INDEFINIDO",
    descSituacaoEleito %in% eleito ~ "ELEITO",
    descSituacaoEleito %in% nao_eleito ~ "NÃO ELEITO",
    descSituacaoEleito == "2º TURNO" ~ "2º TURNO",
    TRUE ~ "INDEFINIDO"
  ))


consulta_candidatos_all %>% filter(descSituacaoEleito == 2) %>% View()

unique(consulta_candidatos_all$descSituacaoEleito)

consulta_candidatos_all %>% 
  group_by(codSituacaoCandidatura, descSituacaoCandidatura) %>%
  summarise(n())

consulta_candidatos_all %>% 
  filter(is.na(codSituacaoEleito)) %>%
  View()

consulta_candidatos_all %>% 
  filter(is.na(codSituacaoCandidatura), !(descSituacaoCandidatura %in% codSituacaoCandidatura)) %>%
   group_by(descSituacaoCandidatura) %>%
  # summarise(quantidade = n()) %>% 
  View()

cod_desc_situacaoCandidatura <- consulta_candidatos_all %>%
  filter(!is.na(codSituacaoCandidatura)) %>%
  group_by(codSituacaoCandidatura, descSituacaoCandidatura) %>%
  summarise(n())

write.csv(consulta_candidatos_all, here("data/candidatos.csv"), row.names = FALSE)
