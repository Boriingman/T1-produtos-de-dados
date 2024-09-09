grafico_bayesiano <- function(preditores, valores_observados, valores_preditos_json){
  set.seed(23161719)
  # Para fazer eixo X do modelo bayesiano:
  X <- do.call(cbind, lapply(preditores, function(col) dados[[col]]))
  y_pred_grafico <- matrix(0, nrow = n_samples, ncol = nrow(X))
  for (i in 1:n_samples) {
    beta_sample <- modelo_bayesiano[i, ]
    beta_sample <- beta_sample[-1]
    y_pred_grafico[i, ] <- X %*% beta_sample
  }
  
  y_pred_mean_grafico <- apply(y_pred_grafico, 2, mean)
  
  grafico_bayes <- ggplot() +
    geom_point(aes(x = y_pred_mean_grafico, y = valores_observados), color = "blue") +
    geom_vline(aes(xintercept = valores_preditos_json), color = "red", linetype = "dashed") +
    labs(x = "Valores Preditos", y = "Valores Observados") +
    theme_minimal()
  
  #Salvar o gráfico no formato PDF na pasta 'saidas'
  ggsave("saidas/grafico.pdf", grafico, width = 8, height = 6)
}

grafico_robusto <- function(valores_observados, valores_preditos_json){
  set.seed(23161719)
  modelo_robusto <- readRDS("saidas/modelo_robusto.rds")
  valores_preditos <- modelo_robusto$fitted.values
  #Criar o gráfico
  grafico <- ggplot() +
    geom_point(aes(x = valores_preditos, y = valores_observados), color = "blue") +
    geom_vline(aes(xintercept = valores_preditos_json), color = "red", linetype = "dashed") +
    labs(x = "Valores Preditos", y = "Valores Observados") +
    theme_minimal()
  
  #Salvar o gráfico no formato PDF na pasta 'saidas'
  ggsave("saidas/grafico.pdf", grafico, width = 8, height = 6)
}

