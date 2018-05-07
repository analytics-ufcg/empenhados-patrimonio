read_historico_tse <- function(arquivo_candidatos_1 = "data/consulta_cand_2012_PB.txt", 
                               arquivo_candidatos_2 = "data/consulta_cand_2016_PB.txt",
                               arquivo_bens_1 = "data/bem_candidato_2012_PB.txt", 
                               arquivo_bens_2 = "data/bem_candidato_2016_PB.txt", 
                               cod_cargo = 11,
                               ano_eleicao1 = 2012,
                               ano_eleicao2 = 2016){
    #' Cria um data.frame com o histórico de bens dos atuais eleitos a partir 
    #' dos dados das eleições dos anos passados como parâmetro.
    library(dplyr)
    library(stringr)
    source(here::here("code/import_tse_utils.R"))

    declaracao_1 <- importDecalaracao(arquivo_bens_1)
    candidatos_1 <- importCandidatos(arquivo_candidatos_1, ano_eleicao1)

    declaracao_2 <- importDecalaracao(arquivo_bens_2)
    candidatos_2 <- importCandidatos(arquivo_candidatos_2, ano_eleicao2)

    atuais_eleitos <- candidatos_2 %>%
        filter(codCargo %in% cod_cargo, codSituacaoEleito != -1, !grepl("SUPLEMENTAR", descEleicao)) %>% # remove #NULO e eleições suplementares
        select(
            sequencialCandidato2 = sequencialCandidato,
            siglaUnidEleitoral,
            descUnidEleitoral,
            nomeCandidato,
            nomeUrnaCandidato,
            siglaPartido,
            codCargo,
            descCargo,
            cpfCandidato,
            descSituacaoEleito
        )

    historico_atuais_eleitos <- atuais_eleitos %>%
        left_join(
            candidatos_1 %>%
              filter(codSituacaoEleito != -1, !grepl("SUPLEMENTAR", descEleicao)) %>%
                select(
                    sequencialCandidato1 = sequencialCandidato,
                    cpfCandidato,
                    codCargo1 = codCargo,
                    descCargo1 = descCargo,
                    descSituacaoEleito1 = descSituacaoEleito,
                    codSituacaoEleito1 = codSituacaoEleito
                ),
            by = c("cpfCandidato")
        )
    
    declaracao_atuais_eleitos1 <- historico_atuais_eleitos %>% 
      select(sequencialCandidato1) %>% 
      left_join(declaracao_1 %>% select(sequencialCandidato, valorBem),
                by = c("sequencialCandidato1" = "sequencialCandidato")) %>% 
      group_by(sequencialCandidato1) %>% 
      summarise(totalBens1 = sum(valorBem)) 

    declaracao_atuais_eleitos2 <- historico_atuais_eleitos %>% 
      select(sequencialCandidato2) %>% 
      left_join(declaracao_2 %>% select(sequencialCandidato, valorBem),
                by = c("sequencialCandidato2" = "sequencialCandidato")) %>% 
      group_by(sequencialCandidato2) %>% 
      summarise(totalBens2 = sum(valorBem)) 
    
    historico_bens_atuais_eleitos <- historico_atuais_eleitos %>% 
      left_join(declaracao_atuais_eleitos1, by = "sequencialCandidato1") %>% 
      left_join(declaracao_atuais_eleitos2, "sequencialCandidato2") %>% 
      filter(codSituacaoEleito1 != 6 | is.na(codSituacaoEleito1)) %>%
      filter(!(cpfCandidato == "34303197491" & codSituacaoEleito1 == -1))
    # Caso particular de JOSE FERNANDES GORGONHO NETO em 2012 (foi candidato a prefeito em 2012 mas teve sua campanha renunciada)
    # Foi removido as ocorrências de segundo turno também

    historico_bens_atuais_eleitos %>% 
        mutate_at(c("nomeUrnaCandidato", "descUnidEleitoral"), str_to_title) %>% 
        return()
} 

read_tse_uma_uf = function(estado, ano_eleicao1, ano_eleicao2, cod_cargo){
    #' Lê e processa dados de uma UF do TSE para criar ganhos de patrimônio 
    #' já agregados. 
    message("Lendo dados: ", estado, ", ", ano_eleicao1, "-", ano_eleicao2)
    cria_nome_tse = function(tipo, ano, estado) {
        prefix = ifelse(tipo == "bem", "bem_candidato_", "consulta_cand_")
        here::here(paste0("data/",
                          prefix,
                          ano,
                          "/",
                          prefix,
                          ano,
                          "_",
                          estado,
                          ".txt")) %>% 
            return()
    }
    arquivo_bens_ano1 = cria_nome_tse("bem", ano_eleicao1, estado)
    arquivo_candidatos_ano1 = cria_nome_tse("candidato", ano_eleicao1, estado)
    arquivo_bens_ano2 = cria_nome_tse("bem", ano_eleicao2, estado)
    arquivo_candidatos_ano2 = cria_nome_tse("candidato", ano_eleicao2, estado)

    read_historico_tse(
        arquivo_candidatos_ano1, 
        arquivo_candidatos_ano2,
        arquivo_bens_ano1, 
        arquivo_bens_ano2, 
        cod_cargo = cod_cargo,
        ano_eleicao1 = ano_eleicao1,
        ano_eleicao2 = ano_eleicao2) %>% 
        patrimonios_tidy(ano_eleicao1 = ano_eleicao1, ano_eleicao2 = ano_eleicao2) %>% 
        return()
}


patrimonios_em_wide <- function(historico){
    historico %>%
        filter(!is.na(totalBens2),
               !is.na(totalBens1)) %>% # APENAS QUEM DECLAROU EM AMBOS
        mutate(ganho = totalBens2 - totalBens1, 
               ganho_relativo = totalBens2 / totalBens1) %>% 
        select(
            cpfCandidato,
            nomeCandidato,
            ano_eleicao1 = totalBens1,
            ano_eleicao2 = totalBens2,
            ganho,
            ganho_relativo,
            nomeUrnaCandidato,
            siglaPartido,
            descUnidEleitoral, 
            descCargo
        ) %>%
        mutate(rank_ganho = row_number(-ganho), 
               rank_ganho_relativo = row_number(-ganho_relativo)) %>% 
        tidyr::gather("ano", "totalBens", 3:4) %>%
        mutate_at(c("ganho", "ganho_relativo"), 
                  funs(if_else(ano == ano_eleicao2, ., NA_real_))) 
}

patrimonios_em_historico <- function(historico_completo, patrimonios_wide){
    cargos <- historico_completo %>%
        select(cpfCandidato, `2012` = descCargo2012, `2016` = descCargo) %>%
        mutate(`2012` = if_else(is.na(`2012`), "Nada", `2012`)) %>%
        mutate(`2016` = if_else(is.na(`2016`), "Nada", `2016`)) %>%
        tidyr::gather("ano", "cargo", 2:3)
    
    situacoes <- historico_completo %>%
        select(cpfCandidato, `2012` = descSituacaoEleito1, `2016` = descSituacaoEleito) %>%
        mutate(`2012` = if_else(is.na(`2012`), "Nada", `2012`)) %>%
        mutate(`2016` = if_else(is.na(`2016`), "Nada", `2016`)) %>%
        tidyr::gather("ano", "situacaoEleito", 2:3)
    
    patrimonios_wide %>% 
        left_join(cargos, by = c("cpfCandidato", "ano")) %>% 
        left_join(situacoes, by = c("cpfCandidato", "ano")) %>% 
        mutate(ano = as.numeric(ano))
}


patrimonios_tidy <- function(historico, ano_eleicao1, ano_eleicao2){
    historico %>%
        filter(!is.na(totalBens2),
               !is.na(totalBens1)) %>%
        mutate(
            ganho = totalBens2 - totalBens1, 
            ganho_relativo = totalBens2 / totalBens1,
            eleicao_1 = ano_eleicao1,
            eleicao_2 = ano_eleicao2, 
            UF = "PB"
        ) %>% 
        select(
            nome_urna = nomeUrnaCandidato,
            unidade_eleitoral = descUnidEleitoral, 
            ganho,
            ganho_relativo,
            patrimonio_eleicao_1 = totalBens1,
            patrimonio_eleicao_2 = totalBens2,
            sigla_partido = siglaPartido,
            cargo_pleiteado_1 = descCargo1,
            resultado_1 = descSituacaoEleito1,
            cargo_pleiteado_2 = descCargo,
            resultado_2 = descSituacaoEleito,
            cpf = cpfCandidato,
            nome_completo = nomeCandidato
        ) 
}
