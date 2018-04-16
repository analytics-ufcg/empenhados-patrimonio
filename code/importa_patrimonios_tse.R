library(tidyverse)
library(here)
library(purrr)

source(here("code/load_historico.R"))


historico_bens <- read_historico_tse(cod_cargo = c(11:13)) %>% 
    filter(!is.na(descSituacaoEleito2012)) # participou em 2012 e 2016

historico_bens %>% 
    patrimonios_tidy() %>% 
    write_csv(here("data/ganhos_pb.csv"))
