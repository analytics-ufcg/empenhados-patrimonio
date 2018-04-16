library(tidyverse)
library(here)
library(purrr)

source(here("code/load_historico.R"))

# Depois de descompactar consulta_cand_2016.zip e bem_candidato_2012.zip 
# tal qual eles vêm do TSE via get_data.sh

estados = c("AC" , "AL" , "AM" , "AP" , "BA" , "CE" , "ES" , "GO" , "MA" , "MG" , "MS" , "MT" , "PA" , "PB" , "PE" , "PI" , "PR" , "RJ" , "RN" , "RO" , "RR" , "RS" , "SC" , "SE" , "SP" , "TO")

dados_tse = tibble(estado = estados) %>%
    mutate(dados = map(
        estado,
        read_tse_uma_uf,
        ano_eleicao1 = 2012,
        ano_eleicao2 = 2016
    )) %>% 
    unnest(dados) %>% 
    filter(!is.na(resultado_1), 
           grepl("ELEITO|SUPLENTE", resultado_1), 
           grepl("ELEITO|SUPLENTE", resultado_2))  # participou em 2012 e 2016

dados_tse %>% 
    filter(grepl("PREFEITO", cargo_pleiteado_2)) %>% 
    write_csv(here("data/ganhos_prefeitos_br.csv"))

dados_tse %>% 
    filter(grepl("VEREADOR", cargo_pleiteado_2)) %>% 
    write_csv(here("data/ganhos_vereadores_br.csv"))
