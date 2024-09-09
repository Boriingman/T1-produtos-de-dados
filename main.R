install.packages("renv", ask = FALSE)
renv::restore(confirm = FALSE)

library(yaml)
library(readr)
library(glue)
library(dplyr)
library(lrgs)
library(coda)
library(jsonlite)
library(MASS)
library(ggplot2)

source("treinamento.R")
source("predicao.R")
source("grafico.R")

# Carregando o arquivo de configuração
config <- read_yaml("configuracao.yaml")

# Carregando os dados
dados <- read_csv2(glue("entradas/{config$tabela}"))

# Criando a matriz X e a variável resposta y
preditores <- config$preditores
resposta <- config$resposta

caminho <- glue("entradas/{config$dados}")
novos_preditores <- fromJSON(caminho)

if(config$modelo == "bayesiano"){
  modelo_bayesiano(resposta, preditores)
  predicao_bayesiano(novos_preditores)
  valores_preditos_json <- fromJSON("saidas/predicoes.json")
  grafico_bayesiano(preditores, dados[[resposta]], valores_preditos_json)
}else{
  modelo_robusto(resposta, preditores)
  predicao_robusto(novos_preditores)
  valores_preditos_json <- fromJSON("saidas/predicoes.json")
  grafico_robusto(dados[[resposta]], valores_preditos_json)
}




