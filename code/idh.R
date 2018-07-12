library(tidyverse)
library(here)

candidatos <- read.csv(here("eleicoes/candidatos_eleicao.csv"))

idh_municipios <- read.csv(here("AtlasBrasil_Consulta-IDH-Municipios.csv"), dec=",", stringsAsFactors = FALSE)

estados <- idh_municipios %>% filter(Código < 100) %>% select(Código, Espacialidades)
names(estados) <- c('Código', 'Estado')
estados$Sigla <- 
  c("BR", "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
idh_municipios <- idh_municipios %>%
  mutate(estadoCod = as.integer(substr(Código, 1, 2))) %>%
  left_join(estados, by=c('estadoCod' = 'Código'))


unidade_eleitoral <- read.csv(here("eleicoes/cod_unidade_eleitoral.csv"), stringsAsFactors = FALSE)
unidade_eleitoral <- unidade_eleitoral %>%
  left_join(candidatos %>% 
              group_by(siglaUnidEleitoral) %>%
              summarise(siglaUF = first(siglaUF)))

idh_municipios <- idh_municipios %>%
  mutate(Espacialidades = case_when(
    Espacialidades == "Espigão D'Oeste" ~ 'Espigão Do Oeste',
    Espacialidades == "Alta Floresta D'Oeste" ~ "Alta Floresta Do Oeste",
    Espacialidades == "Alvorada D'Oeste" ~ "Alvorada Do Oeste",
    Espacialidades == "Santa Luzia D'Oeste" ~ "Santa Luzia Do Oeste",
    Espacialidades == "Nova Brasilândia D'Oeste" ~ "Nova Brasilândia Do Oeste",
    Espacialidades == "Machadinho D'Oeste" ~ "Machadinho Do Oeste",
    Espacialidades == "São Felipe D'Oeste" ~ "São Felipe Do Oeste",
    Espacialidades == "Januário Cicco" ~ "Boa Saúde",
    Espacialidades == "Olho-D'Água do Borges" ~ "Olho D'Água do Borges",
    Espacialidades == "Presidente Juscelino" & Sigla == "RN" ~ "Serra Caiada", 
    Espacialidades == "Couto Magalhães" ~ "Couto De Magalhães",
    Espacialidades == "Sant'Ana do Livramento" ~ "Santana do Livramento",
    Espacialidades == "Açu" ~ "Assu",
    Espacialidades == "Augusto Severo" ~ "Campo Grande",
    Espacialidades == "São Domingos" & Sigla == "PB" ~ "São Domingos de Pombal",
    Espacialidades == "Seridó" ~ "São Vicente do Seridó", 
    Espacialidades == "Tacima" ~ "Campo de Santana",
    Espacialidades == "Belém do São Francisco" ~ "Belém de São Francisco",
    Espacialidades == "Ilha de Itamaracá" ~ "Itamaracá",
    Espacialidades == "Gracho Cardoso" ~ "Graccho Cardoso",
    Espacialidades == "Barro Preto" ~ "Governador Lomanto Júnior",
    Espacialidades == "Muquém de São Francisco" ~ "Muquém do São Francisco",
    Espacialidades == "Quijingue" ~ "Quinjingue",
    Espacialidades == "Santa Teresinha" & Sigla == "BA" ~ "Santa Terezinha",
    Espacialidades == "Sem-Peixe" ~ "Sem Peixe",
    Espacialidades == "Olhos-D'Água" ~ "Olhos D'Água",
    Espacialidades == "Pingo-D'Água" ~ "Pingo D'Água",
    Espacialidades == "Passa-Vinte" ~ "Passa Vinte",
    Espacialidades == "Paraty" ~ "Parati",
    Espacialidades == "Trajano de Moraes" ~ "Trajano de Morais",
    Espacialidades == "Biritiba-Mirim" ~ "Biritiba Mirim",
    Espacialidades == "Florínia" ~ "Florínea",
    Espacialidades == "Grão Pará" ~ "Grão-Pará",
    Espacialidades == "Luiz Alves" ~ "Luis Alves",
    Espacialidades == "Presidente Castello Branco" ~ "Presidente Castelo Branco",
    Espacialidades == "Poxoréo" ~ "Poxoréu",
    Espacialidades == "São Valério" ~ "São Valério do Tocantins",
    TRUE ~ Espacialidades
  ))

# NEW
# PINTO BANDEIRA, Mojuí dos Campos, PARAÍSO DAS ÁGUAS, BALNEÁRIO RINCÃO, PESCARIA BRAVA,

# Não Identifciado
# ÁGUA BRANCA DO AMAPARI


idh_municipios$desc_Unid_Eleitoral_lower <- tolower(iconv(idh_municipios$Espacialidades, from="UTF-8", to="ASCII//TRANSLIT"))
unidade_eleitoral$desc_Unid_Eleitoral_lower <- tolower(iconv(unidade_eleitoral$descUnidEleitoral, to="ASCII//TRANSLIT"))

unidade_eleitoral_estados <- unidade_eleitoral %>% filter(siglaUnidEleitoral %in% estados$Sigla)
unidade_eleitoral_municipios <-  unidade_eleitoral %>% filter(!siglaUnidEleitoral %in% estados$Sigla)

unidade_eleitoral <- unidade_eleitoral %>% right_join(idh_municipios, by=c('siglaUF' = 'Sigla', 'desc_Unid_Eleitoral_lower' = 'desc_Unid_Eleitoral_lower'))

to_fix <- unidade_eleitoral %>% filter(is.na(Código), !descUnidEleitoral %in% 
                                         # Cidades criadas após o censo
                                         c("PINTO BANDEIRA", "MOJUÍ DOS CAMPOS", "PARAÍSO DAS ÁGUAS", "BALNEÁRIO RINCÃO", "PESCARIA BRAVA"))

idh_geral <- unidade_eleitoral %>%
  select(Código, siglaUnidEleitoral, Espacialidades, estadoCod, Estado, siglaUF, 
         IDHM.2010, IDHM.Renda.2010, IDHM.Longevidade.2010, IDHM.Educação.2010) %>%
  mutate(siglaUnidEleitoral = as.numeric(siglaUnidEleitoral)) %>%
  mutate(siglaUnidEleitoral = ifelse(is.na(siglaUnidEleitoral), Código, siglaUnidEleitoral)) %>%
  arrange(siglaUnidEleitoral)

idh_geral$Código <- as.character(idh_geral$Código)
idh_geral$siglaUnidEleitoral <- as.character(idh_geral$siglaUnidEleitoral)
write.csv(idh_geral, here("idh.csv"), row.names = FALSE)

