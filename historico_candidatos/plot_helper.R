plot_ganho <- function(historico_prefeitos, prefeitos_cargo, prefeitos_situacao, tipoGanho = "relativo"){
  
  tipoGanhoEntrada <- tipoGanho
  
  plot_prefeitos_ganho <- historico_prefeitos %>% 
    mutate(ganho = totalBens2016 - totalBens2012) %>% 
    rowwise() %>% 
    mutate(totalBens2016 = if_else(tipoGanho == "relativo", 100*((totalBens2016 - totalBens2012)/totalBens2012), totalBens2016 - totalBens2012)) %>% 
    mutate(totalBens2012 = 0) %>% 
    select(cpfCandidato, nomeCandidato, `2012` = totalBens2012, `2016` = totalBens2016, ganho, 
           nomeUrnaCandidato, descUnidEleitoral, siglaPartido) %>% 
    tidyr::gather("ano", tipoGanho, 3:4) %>%
    arrange(desc(tipoGanho))
  
  plot_prefeitos_ganho_completo <- plot_prefeitos_ganho %>% 
    left_join(prefeitos_cargo) %>% 
    left_join(prefeitos_situacao) %>% 
    left_join(historico_prefeitos %>% select(cpfCandidato, totalBens2012, totalBens2016))
  
  plot_n_prefeitos_ganho <- (plot_prefeitos_ganho_completo %>% 
                                 head(10) %>% 
                                 select(cpfCandidato))$cpfCandidato
  
  if(tipoGanhoEntrada == "relativo"){
    tipoGanhoEntrada = "Relativo (%)"
  }else{
    tipoGanhoEntrada = "Absoluto (R$)"
  }
  
  plot_prefeitos_ganho_completo %>% 
    filter(cpfCandidato %in% plot_n_prefeitos_ganho) %>%
    mutate(nomeUrnaCandidatoPartido = paste(nomeUrnaCandidato, "(", siglaPartido,")")) %>% 
    plot_ly() %>%
    add_trace(x = ~as.factor(ano), y = ~tipoGanho, color = ~nomeUrnaCandidatoPartido,
              type = "scatter", mode = "lines+markers",
              text = ~paste(
                "Prefeito:", nomeCandidato, "<br>",
                "Nome de urna:", nomeUrnaCandidato, "<br>",
                "Total de bens (2012): R$ ", totalBens2012, "<br>",
                "Total de bens (2016): R$ ", totalBens2016, "<br>",
                paste("Ganho", tipoGanhoEntrada,":"), round(tipoGanho, 2), "<br>",
                "Cargo: ", cargo, "<br>",
                "Situação: ", situacaoEleito, "<br>",
                "Município:", descUnidEleitoral
              ),
              hoverinfo = "text") %>%
    layout(xaxis = list(title = ""),
           legend = list(orientation = "h"),
           yaxis = list(title = paste("Ganho", tipoGanhoEntrada)))
  
}
