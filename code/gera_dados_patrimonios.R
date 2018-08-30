library(here)
library(tidyverse)
source(here("code/load_historico.R"))

# Use o script get_data.sh para baixar os dados
# Descompacte todos os zips baixados 
# Execute o script clean_data.sh para realizar limpeza nos 
# Agora você pode executar o código abaixo

estados = c("AC" , "AL" , "AM" , "AP" , "BA" , "CE" , "ES" , "GO" , "MA" , "MG" , "MS" , "MT" , "PA" , "PB" , "PE" , "PI" , "PR" , "RJ" , "RN" , "RO" , "RR" , "RS" , "SC" , "SE" , "SP" , "TO")
estados_df_br = c("AC" , "AL" , "AM" , "AP" , "BA" , "BR", "CE", "DF", "ES" , "GO" , "MA" , "MG" , "MS" , "MT" , "PA" , "PB" , "PE" , "PI" , "PR" , "RJ" , "RN" , "RO" , "RR" , "RS" , "SC" , "SE" , "SP" , "TO")

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

dados_tse_2014 <- tibble(estado = estados_df_br) %>%
  mutate(dados = map(
    estado,
    read_tse_uma_uf,
    cod_cargo = c(1:10),
    ano_eleicao1 = 2010,
    ano_eleicao2 = 2014
  )) %>% 
  unnest(dados) %>% 
  filter(!is.na(resultado_1)) %>% 
  distinct(cpf, .keep_all = TRUE) %>% 
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
  filter(!is.na(resultado_1)) %>% 
  mutate(ano_um = 2012) %>%
  mutate(ano_dois = 2016)

dados_tse_2018 <- tibble(estado = estados_df_br) %>%
    mutate(dados = map(
        estado,
        read_tse_uma_uf,
        cod_cargo = c(1:10),
        ano_eleicao1 = 2014,
        ano_eleicao2 = 2018
    )) %>% 
    unnest(dados) %>% 
    filter(!is.na(resultado_1)) %>%
    distinct(cpf, .keep_all = TRUE) %>% 
    mutate(ano_um = 2014) %>%
    mutate(ano_dois = 2018)


dados_tse_all <- dados_tse_2012 %>%
  rbind(dados_tse_2014) %>%
  rbind(dados_tse_2016) %>% 
  rbind(dados_tse_2018)

dados_tse_all %>%
  write.csv(here("data/patrimonio_candidatos.csv"), row.names = FALSE)
