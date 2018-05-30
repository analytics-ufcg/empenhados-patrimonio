library(here)
library(tidyverse)
source(here("code/load_historico.R"))

# Depois de descompactar consulta_cand_2016.zip e bem_candidato_2012.zip 
# tal qual eles vêm do TSE via get_data.sh

estados = c("AC" , "AL" , "AM" , "AP" , "BA" , "CE" , "ES" , "GO" , "MA" , "MG" , "MS" , "MT" , "PA" , "PB" , "PE" , "PI" , "PR" , "RJ" , "RN" , "RO" , "RR" , "RS" , "SC" , "SE" , "SP" , "TO")

dados_tse_2012 <- tibble(estado = estados) %>%
  mutate(dados = map(
    estado,
    read_tse_uma_uf,
    cod_cargo = c(11:13),
    ano_eleicao1 = 2008,
    ano_eleicao2 = 2012
  )) %>% 
  unnest(dados) %>% 
  filter(!is.na(resultado_1)) %>%
  mutate(ano_um = 2008) %>%
  mutate(ano_dois = 2012)

dados_tse_2014 <- tibble(estado = estados) %>%
  mutate(dados = map(
    estado,
    read_tse_uma_uf,
    cod_cargo = c(3:11),
    ano_eleicao1 = 2010,
    ano_eleicao2 = 2014
  )) %>% 
  unnest(dados) %>% 
  filter(!is.na(resultado_1)) %>%
  mutate(ano_um = 2010) %>%
  mutate(ano_dois = 2014)

dados_tse_2016 <- tibble(estado = estados) %>%
  mutate(dados = map(
    estado,
    read_tse_uma_uf,
    cod_cargo = c(11:13),
    ano_eleicao1 = 2012,
    ano_eleicao2 = 2016
  )) %>% 
  unnest(dados) %>% 
  filter(!is.na(resultado_1))  %>%
  mutate(ano_um = 2012) %>%
  mutate(ano_dois = 2016)


dados_tse_all <- dados_tse_2012 %>%
  rbind(dados_tse_2014) %>%
  rbind(dados_tse_2016)

