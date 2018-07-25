# Os bens e os candidatos

Análises do patrimônio declarado pelos candidatos ao TSE ao longo do tempo. Este repositório contém o ETL e processamento dos dados para serem utilizados na aplicação web que está em https://github.com/analytics-ufcg/empenhados-patrimonio-app 

## Resultados

Relatórios nos notebooks em `reports/`. Dados derivados a partir dos do TSE em `data/ganhos*`

## Gerar nossos dados a partir daqueles do TSE 

No linux/mac/ubuntu ou no ubuntu dentro do windows: 

```
cd data/
./get_data.sh 

for eleicao in 2008 2010 2012 2014 2016; do 
    unzip -d bem_candidato_$eleicao bem_candidato_$eleicao.zip
    unzip -d consulta_cand_$eleicao consulta_cand_$eleicao.zip
done

cd ..
Rscript code/importa_patrimonios_tse.R

# resultados em data/ganhos*csv
```

No windows sem ubuntu, instale linux.
