library(tidyverse)
library(here)

candidatos <- read.csv(here("data/candidatos.csv"), stringsAsFactors = FALSE)

# UNIDADE ELEITORAL
cod_unidade_eleitoral <- candidatos %>%
  select(siglaUnidEleitoral, descUnidEleitoral) %>%
  distinct(siglaUnidEleitoral, .keep_all = TRUE) %>%
  na.omit() %>% 
  arrange(siglaUnidEleitoral)

candidatos_alt <- candidatos %>%
  select(-descUnidEleitoral)

## CARGO
cod_cargo <- candidatos_alt %>%
  select(codCargo, descCargo) %>%
  distinct(codCargo, .keep_all = TRUE) %>% 
  na.omit() %>% 
  arrange(codCargo)

candidatos_alt <- candidatos_alt %>%
  select(-descCargo)

## SITUAÇÃO CANDIDATURA
cod_situacao_candidatura <- candidatos_alt %>%
  select(codSituacaoCandidatura, descSituacaoCandidatura) %>%
  distinct(codSituacaoCandidatura, .keep_all = TRUE) %>% 
  na.omit() %>% 
  arrange(codSituacaoCandidatura)

candidatos_alt <- candidatos_alt %>%
  select(-descSituacaoCandidatura)

## PARTIDO
cod_partido <- candidatos_alt %>%
  select(numeroPartido, siglaPartido, nomePartido) %>%
  distinct(numeroPartido, .keep_all = TRUE) %>% 
  na.omit() %>% 
  arrange(numeroPartido)

candidatos_alt <- candidatos_alt %>%
  select(-c(siglaPartido, nomePartido))

## OCUPAÇÃO
cod_ocupacao <- candidatos_alt %>%
  select(codOcupacao, descOcupacao) %>%
  distinct(codOcupacao, .keep_all = TRUE) %>% 
  na.omit() %>% 
  arrange(codOcupacao)

candidatos_alt <- candidatos_alt %>%
  select(-descOcupacao)

## SEXO
cod_sexo <- candidatos_alt %>%
  select(codSexo, descSexo) %>%
  distinct(codSexo, .keep_all = TRUE) %>%
  na.omit() %>% 
  arrange(codSexo)

candidatos_alt <- candidatos_alt %>%
  select(-descSexo)

## GRAU DE INSTRUÇÃO
cod_grau_instrucao <- candidatos_alt %>%
  select(codGrauInstrucao, descGrauInstrucao) %>%
  distinct(codGrauInstrucao, .keep_all = TRUE) %>%
  na.omit() %>%
  arrange(codGrauInstrucao)

candidatos_alt <- candidatos_alt %>%
  select(-descGrauInstrucao)

## ESTADO CIVIL
cod_estado_civil <- candidatos_alt %>%
  select(codEstadoCivil, descEstadoCivil) %>%
  distinct(codEstadoCivil, .keep_all = TRUE) %>%
  na.omit() %>%
  arrange(codEstadoCivil)

candidatos_alt <- candidatos_alt %>%
  select(-descEstadoCivil)

## COR RAÇA
cod_cor_raca <- candidatos_alt %>%
  select(codCorRaca, descCorRaca) %>% 
  distinct(codCorRaca, .keep_all = TRUE) %>%
  na.omit() %>%
  arrange(codCorRaca)

candidatos_alt <- candidatos_alt %>%
  select(-descCorRaca)

## NACIONALIDADE
cod_nacionalidade <- candidatos_alt %>%
  select(codNacionalidade, descNacionalidade) %>%
  distinct(codNacionalidade, .keep_all = TRUE) %>%
  na.omit() %>%
  arrange(codNacionalidade)

candidatos_alt <- candidatos_alt %>%
  select(-descNacionalidade)

## SITUAÇÃO ELEITO
cod_situacao_eleito <- candidatos_alt %>%
  select(codSituacaoEleito, descSituacaoEleito) %>%
  distinct(codSituacaoEleito, .keep_all = TRUE) %>%
  na.omit() %>%
  arrange(codSituacaoEleito)

candidatos_alt <- candidatos_alt %>%
  select(-descSituacaoEleito)


## GERANDO CSV's

write.csv(candidatos_alt, here("data/candidatos_eleicao.csv"), row.names = FALSE)

write.csv(cod_unidade_eleitoral, here("data/cod_unidade_eleitoral.csv"), row.names = FALSE)
write.csv(cod_cargo, here("data/cod_cargo.csv"), row.names = FALSE)
write.csv(cod_situacao_candidatura, here("data/cod_situacao_candidatura.csv"), row.names = FALSE)
write.csv(cod_partido, here("data/cod_partido.csv"), row.names = FALSE)
write.csv(cod_ocupacao, here("data/cod_ocupacao.csv"), row.names = FALSE)
write.csv(cod_sexo, here("data/cod_sexo.csv"), row.names = FALSE)
write.csv(cod_grau_instrucao, here("data/cod_grau_instrucao.csv"), row.names = FALSE)
write.csv(cod_estado_civil, here("data/cod_estado_civil.csv"), row.names = FALSE)
write.csv(cod_cor_raca, here("data/cod_cor_raca.csv"), row.names = FALSE)
write.csv(cod_nacionalidade, here("data/cod_nacionalidade.csv"), row.names = FALSE)
write.csv(cod_situacao_eleito, here("data/cod_situacao_eleito.csv"), row.names = FALSE)
