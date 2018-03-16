library(dplyr)
source("import_data_candidatos_bens.R")

declaracao_2012 <- importDecalaracao2012()
candidatos_2012 <- importCandidatos2012()
declaracao_2016 <- importDecalaracao2016()
candidatos_2016 <- importCandidatos2016()
  
# Prefeitos eleitos em 2012
prefeitos2012 <- candidatos_2012 %>% 
  filter(codCargo == 11, codSituacaoEleito %in% c(1, 2, 3)) %>% 
  select(sequencialCandidato, siglaUnidEleitoral, descUnidEleitoral, siglaUF, nomeCandidato, nomeUrnaCandidato, cpfCandidato, siglaPartido, descSituacaoEleito)

# Prefeitos eleitos em 2016
prefeitos2016 <- candidatos_2016 %>% 
  filter(codCargo == 11, codSituacaoEleito %in% c(1, 2, 3)) %>% 
  select(sequencialCandidato, siglaUnidEleitoral, descUnidEleitoral, siglaUF, nomeCandidato, nomeUrnaCandidato, cpfCandidato, siglaPartido, descSituacaoEleito)

# Soma dos valores dos bens declarados por candidatos em 2012
cand_bens_group2012 <- declaracao_2012 %>% 
  group_by(sequencialCandidato) %>% 
  summarise(totalBens2012 = sum(valorBem))

# Soma dos valores dos bens declarados por candidatos em 2016
cand_bens_group2016 <- declaracao_2016 %>% 
  group_by(sequencialCandidato) %>% 
  summarise(totalBens2016 = sum(valorBem))

# Prefeitos eleitos em 2012 e quanto ele declarou
prefeitos_bens2012 <- prefeitos2012 %>% 
  left_join(cand_bens_group2012) %>% 
  arrange(desc(totalBens2012))

rm(prefeitos2012)
# Prefeitos eleitos em 2016 e quanto ele declarou
prefeitos_bens2016 <- prefeitos2016 %>% 
  left_join(cand_bens_group2016) %>% 
  arrange(desc(totalBens2016))

rm(prefeitos2016)

# Prefeitos reeleitos e a diferença entre o total declarado em 2016 e 2012 
prefeitos_reeleitos <- prefeitos_bens2012 %>% 
  inner_join(prefeitos_bens2016 %>% 
               select(cpfCandidato, codMunicipio2016 = siglaUnidEleitoral, siglaPartido2016 = siglaPartido, totalBens2016), 
             by = c("cpfCandidato")) %>% 
  mutate(ganho = totalBens2016 - totalBens2012) %>% 
  arrange(desc(ganho))

