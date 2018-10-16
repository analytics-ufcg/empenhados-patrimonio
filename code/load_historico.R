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
  
    determina_situacao <- function(data_candidatos) {
      
      apto <- c(2, 4, 8, 16, 17, 18, 19)
      inapto <- c(5, 6, 7, 9, 10, 11, 13, 14)
      
      eleito <- c("ELEITO", "ELEITO POR QP", "ELEITO POR MÉDIA", "MÉDIA")
      
      nao_eleito <- c("SUPLENTE", "RENÚNCIA/FALECIMENTO/CASSAÇÃO ANTES DA ELEIÇÃO", "NÃO ELEITO",
                      "RENÚNCIA/FALECIMENTO/CASSAÇÃO APÓS A ELEIÇÃO", "REGISTRO NEGADO ANTES DA ELEIÇÃO",
                      "REGISTRO NEGADO APÓS A ELEIÇÃO", "SUBSTITUÍDO", "INDEFERIDO COM RECURSO", "CASSADO COM RECURSO")
      
      data <- data_candidatos %>%
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
          TRUE ~ as.character(codSituacaoCandidatura)
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
    
    return(data)
  }

    declaracao_1 <- importDecalaracao(arquivo_bens_1, ano_eleicao1)
    candidatos_1 <- importCandidatos(arquivo_candidatos_1, ano_eleicao1) 
    
    ## Tratamento especial para o ano de 2014. Os dados do TSE estão inconsistentes para este ano.
    if (ano_eleicao1 == 2014) {
        candidatos_1 <- ajusta_situacao_2014(candidatos_1)
    }
    
    candidatos_1 <- determina_situacao(candidatos_1)

    declaracao_2 <- importDecalaracao(arquivo_bens_2, ano_eleicao2)
    candidatos_2 <- importCandidatos(arquivo_candidatos_2, ano_eleicao2) 
    
    ## Tratamento especial para o ano de 2014. Os dados do TSE estão inconsistentes para este ano.
    if (ano_eleicao2 == 2014) {
        candidatos_2 <- ajusta_situacao_2014(candidatos_2)
    }
    
    candidatos_2 <- determina_situacao(candidatos_2)
    
    ## Caso especial para 2018, segundo turno ainda não realizado
    ## TODO: ATUALIZAR quando o segundo turno for realizado
    if (ano_eleicao2 == 2018) {
        candidatos_2 <- candidatos_2 %>% 
            mutate(codSituacaoEleito = ifelse(codSituacaoEleito == 6, 99, codSituacaoEleito))
    }
    
    atuais_eleitos <- candidatos_2 %>%
        filter(codCargo %in% cod_cargo, codSituacaoEleito != 6, codSituacaoEleito != -1, !grepl("SUPLEMENTAR", descEleicao)) %>% # remove #NULO e eleições suplementares
        select(
            sequencialCandidato2 = sequencialCandidato,
            siglaUnidEleitoral2 = siglaUnidEleitoral,
            siglaUF2 = siglaUF,
            descUnidEleitoral,
            nomeCandidato,
            nomeUrnaCandidato,
            siglaPartido,
            codCargo,
            descCargo,
            cpfCandidato,
            descSituacaoEleito,
            situacaoCandidatura2  = classeSituacaoCandidatura,
            situacaoEleicao2 = classeSituacaoEleicao
        )

    historico_atuais_eleitos <- atuais_eleitos %>%
        left_join(
            candidatos_1 %>%
              filter(codSituacaoEleito != -1, codSituacaoEleito != 6, !grepl("SUPLEMENTAR", descEleicao)) %>%
                select(
                    sequencialCandidato1 = sequencialCandidato,
                    siglaUnidEleitoral1 = siglaUnidEleitoral,
                    siglaUF1 = siglaUF,
                    cpfCandidato,
                    codCargo1 = codCargo,
                    descCargo1 = descCargo,
                    descSituacaoEleito1 = descSituacaoEleito,
                    codSituacaoEleito1 = codSituacaoEleito,
                    situacaoCandidatura1  = classeSituacaoCandidatura,
                    situacaoEleicao1 = classeSituacaoEleicao
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
      select(sequencialCandidato2, sequencialCandidato1) %>% 
      left_join(declaracao_2 %>% select(sequencialCandidato, valorBem),
                by = c("sequencialCandidato2" = "sequencialCandidato")) %>% 
      group_by(sequencialCandidato2, sequencialCandidato1) %>% 
      summarise(totalBens2 = sum(valorBem)) 
    
    historico_bens_atuais_eleitos <- historico_atuais_eleitos %>% 
      left_join(declaracao_atuais_eleitos1, by = "sequencialCandidato1") %>% 
      left_join(declaracao_atuais_eleitos2, by = c("sequencialCandidato2", "sequencialCandidato1")) %>% 
      filter(codSituacaoEleito1 != 6 | is.na(codSituacaoEleito1)) %>%
      filter(!(cpfCandidato == "34303197491" & codSituacaoEleito1 == -1))
    # Caso particular de JOSE FERNANDES GORGONHO NETO em 2012 (foi candidato a prefeito em 2012 mas teve sua campanha renunciada)
    # Foi removido as ocorrências de segundo turno também
    
    historico_bens_atuais_eleitos %>% 
        mutate_at(c("nomeUrnaCandidato", "descUnidEleitoral"), str_to_title) %>% 
        return()
}

cria_nome_tse = function(tipo, ano, estado) {
    extensao = ".txt"
    if (ano %in% c(2014, 2016, 2018)) {
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

read_tse_uma_uf = function(estado, ano_eleicao1, ano_eleicao2, cod_cargo){
    #' Lê e processa dados de uma UF do TSE para criar ganhos de patrimônio 
    #' já agregados. 
    message("Lendo dados: ", estado, ", ", ano_eleicao1, "-", ano_eleicao2)
    
    arquivo_bens_ano1 = cria_nome_tse("bem", ano_eleicao1, estado)
    arquivo_candidatos_ano1 = cria_nome_tse("candidato", ano_eleicao1, estado)
    arquivo_bens_ano2 = cria_nome_tse("bem", ano_eleicao2, estado)
    arquivo_candidatos_ano2 = cria_nome_tse("candidato", ano_eleicao2, estado)
    
    
    ## Verifica candidaturas relacionadas a presidência no ano 2
    dados_presidente_ano2 = data.frame()
    if (ano_eleicao1 %% 4 == 2 && estado != "BR") {
        dados_presidente_ano2 <- patrimonio_progressao_presidente(estado, ano_eleicao1, ano_eleicao2)
    }

    ## Verifica candidaturas a outros cargos não relacionados a presidência no ano 2
    dados_presidente_ano1 = data.frame()
    if (estado == "BR") {
        dados_presidente_ano1 = patrimonio_uf_para_uf("BR", ano_eleicao1, ano_eleicao2)
    }

    read_historico_tse(
        arquivo_candidatos_ano1, 
        arquivo_candidatos_ano2,
        arquivo_bens_ano1, 
        arquivo_bens_ano2, 
        cod_cargo = cod_cargo,
        ano_eleicao1 = ano_eleicao1,
        ano_eleicao2 = ano_eleicao2) %>% 
        rbind(dados_presidente_ano2) %>%
        rbind(dados_presidente_ano1) %>%
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
            sequencialCandidato1,
            sequencialCandidato2,
            cpfCandidato,
            nomeCandidato,
            ano_eleicao1 = totalBens1,
            ano_eleicao2 = totalBens2,
            ganho,
            ganho_relativo,
            nomeUrnaCandidato,
            siglaPartido,
            siglaUnidEleitoral,
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
            sequencial_candidato_1 = sequencialCandidato1,
            sequencial_candidato_2 = sequencialCandidato2,
            nome_urna = nomeUrnaCandidato,
            cd_unidade_eleitoral_1 = siglaUnidEleitoral1,
            cd_unidade_eleitoral_2 = siglaUnidEleitoral2,
            unidade_eleitoral = descUnidEleitoral, 
            sigla_UF_1 = siglaUF1,
            sigla_UF_2 = siglaUF2,
            ganho,
            ganho_relativo,
            patrimonio_eleicao_1 = totalBens1,
            patrimonio_eleicao_2 = totalBens2,
            sigla_partido = siglaPartido,
            cargo_pleiteado_1 = descCargo1,
            resultado_1 = descSituacaoEleito1,
            cargo_pleiteado_2 = descCargo,
            resultado_2 = descSituacaoEleito,
            situacaoCandidatura1,
            situacaoCandidatura2,
            situacaoEleicao1,
            situacaoEleicao2,
            cpf = cpfCandidato,
            nome_completo = nomeCandidato
        ) 
}

patrimonio_progressao_presidente <- function(estado, ano_eleicao1, ano_eleicao2) {
    
    dados_progressao_presidente <- data.frame()
    
    ## Recupera dados considerando progressão de cargo do nível estadual para o federal
    
    arquivo_bens_ano1_federal = cria_nome_tse("bem", ano_eleicao1, estado)
    arquivo_candidatos_ano1_federal = cria_nome_tse("candidato", ano_eleicao1, estado)
    arquivo_bens_ano2_federal = cria_nome_tse("bem", ano_eleicao2, "BR")
    arquivo_candidatos_ano2_federal = cria_nome_tse("candidato", ano_eleicao2, "BR")
    
    message("Lendo dados: UF_ano1 - ", estado, ", UF_ano2 - BR, ", ano_eleicao1, "-", ano_eleicao2)
    
    dados_progressao_presidente <- read_historico_tse(arquivo_candidatos_ano1_federal,
                                                      arquivo_candidatos_ano2_federal,
                                                      arquivo_bens_ano1_federal,
                                                      arquivo_bens_ano2_federal,
                                                      cod_cargo = 1:13,
                                                      ano_eleicao1 = ano_eleicao1,
                                                      ano_eleicao2 = ano_eleicao2)
    
    dados_progressao_presidente %>% 
        return()
}

patrimonio_uf_para_uf <- function(uf, ano_eleicao1, ano_eleicao2) {
    estados = c("AC" , "AL" , "AM" , "AP" , "BA" , "CE" , "ES" , "GO" , "MA" , "MG" , "MS" , "MT" , "PA" , "PB" , "PE" , "PI" , "PR" , "RJ" , "RN" , "RO" , "RR" , "RS" , "SC" , "SE" , "SP" , "TO")
    
    arquivo_bens_ano1_federal = cria_nome_tse("bem", ano_eleicao1, uf)
    arquivo_candidatos_ano1_federal = cria_nome_tse("candidato", ano_eleicao1, uf)
    
    presidente_ano1 <- data.frame()
    
    for (estado in estados) {
        message("Lendo dados: UF_ano1 - ", uf, ", UF_ano2 - ", estado, ", ", ano_eleicao1, "-", ano_eleicao2)
        arquivo_bens_ano2_federal = cria_nome_tse("bem", ano_eleicao2, estado)
        arquivo_candidatos_ano2_federal = cria_nome_tse("candidato", ano_eleicao2, estado)
        
        presidente_ano1 <- presidente_ano1 %>% 
            rbind(read_historico_tse(arquivo_candidatos_ano1_federal,
                                     arquivo_candidatos_ano2_federal,
                                     arquivo_bens_ano1_federal,
                                     arquivo_bens_ano2_federal,
                                     cod_cargo = 1:13,
                                     ano_eleicao1 = ano_eleicao1,
                                     ano_eleicao2 = ano_eleicao2)) %>% 
            filter(descCargo1 %in% c("PRESIDENTE", "VICE-PRESIDENTE"))
    }
    
    presidente_ano1 %>% 
        return()
}

## Tratamento especial para o ano de 2014. Os dados do TSE estão inconsistentes para este ano.
ajusta_situacao_2014 <- function(data) {
    data <- data %>%
        mutate(codSituacaoEleito = ifelse(siglaUF == "BR", 
                                          ifelse(numTurno == 2, codSituacaoEleito, 
                                                 ifelse(sequencialCandidato %in% c("280000000083", "280000000084", "280000000085", "280000000086"),
                                                        -1, 4)),
                                          codSituacaoEleito)) %>%
        mutate(descSituacaoEleito = ifelse(siglaUF == "BR", 
                                           ifelse(numTurno == 2, descSituacaoEleito, 
                                                  ifelse(sequencialCandidato %in% c("280000000083", "280000000084", "280000000085", "280000000086"),
                                                         "INDEFINIDO", "NÃO ELEITO")),
                                           descSituacaoEleito))
    return(data)
}


