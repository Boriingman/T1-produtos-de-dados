predicao_bayesiano <- function(novos_dados){
  set.seed(23161719)
  modelo_bayesiano <- readRDS("saidas/modelo_bayesiano.rds")
  
  X_novos <- as.matrix(novos_dados)
  X_novos <- as.matrix(X_novos)
  
  n_resposta <- nrow(X_novos)
  n_samples <- nrow(modelo_bayesiano)
  
  y_pred <- matrix(0, nrow = n_samples, ncol = nrow(X_novos))
  X_novos <- matrix(as.numeric(X_novos), nrow = nrow(X_novos))
  
  for (i in 1:n_samples) {
    beta_sample <- modelo_bayesiano[i, ]
    beta_sample <- beta_sample[-1]
    y_pred[i, ] <- X_novos %*% beta_sample
  }
  
  y_pred_mean <- apply(y_pred, 2, mean)
  
  jsonlite::write_json(y_pred_mean, path = "saidas/predicoes.json", pretty = TRUE, auto_unbox = TRUE)
  
}

predicao_robusto <- function(novos_dados){
  set.seed(23161719)
  modelo_robusto <- readRDS("saidas/modelo_robusto.rds")
  predicoes_robustas <- predict(modelo_robusto, novos_dados)
  jsonlite::write_json(predicoes_robustas, path = "saidas/predicoes.json", pretty = TRUE, auto_unbox = TRUE)
  
}






