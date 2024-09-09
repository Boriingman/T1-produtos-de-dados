modelo_bayesiano <- function(resposta, preditores){
  set.seed(23161719)
  X <- do.call(cbind, lapply(preditores, function(col) dados[[col]]))
  y <- dados[[resposta]]
  
  # Definindo os parâmetros da distribuição a priori
  beta_prior_mean <- rep(0, ncol(X) + 1) 
  beta_prior_cov <- diag(10^6, ncol(X) + 1) 
  sigma_prior_scale <- matrix(config$sigma_prior_scale, 1, 1)  
  sigma_prior_dof <- config$sigma_prior_dof
  
  # Definindo os valores iniciais para o Gibbs Sampler
  n <- ncol(X) + 1  # Número de parâmetros incluindo o intercepto
  beta_start_1 <- matrix(rep(config$beta_start_1, n), nrow = n, ncol = 1)
  beta_start_2 <- matrix(rep(config$beta_start_2, n), nrow = n, ncol = 1)
  sigma2_start <- config$sigma2_start
  start_values_1 <- list(B = beta_start_1, Sigma2 = sigma2_start)
  start_values_2 <- list(B = beta_start_2, Sigma2 = sigma2_start)  
  
  # Executando o Gibbs Sampler para duas cadeias
  fit.post.chain1 <- Gibbs.regression(
    X, y,
    M = config$M,
    Nsamples = config$Nsamples,
    Ngauss = config$Ngauss,
    dirichlet = config$dirichlet,
    M.inv = config$M_inv,
    intercept = config$intercept,
    trace = config$trace,
    fix = config$fix,
    start = start_values_1,
    B.prior.mean = beta_prior_mean,
    B.prior.cov = beta_prior_cov,
    Sigma.prior.scale = sigma_prior_scale,
    Sigma.prior.dof = sigma_prior_dof,
    dp.prior.alpha = config$dp_prior_alpha,
    dp.prior.beta = config$dp_prior_beta,
    mention.every = NA,
    save.every = NA,
    save.to = NA
  )
  
  fit.post.chain2 <- Gibbs.regression(
    X, y,
    M = config$M,
    Nsamples = config$Nsamples,
    Ngauss = config$Ngauss,
    dirichlet = config$dirichlet,
    M.inv = config$M_inv,
    intercept = config$intercept,
    trace = config$trace,
    fix = config$fix,
    start = start_values_2,
    B.prior.mean = beta_prior_mean,
    B.prior.cov = beta_prior_cov,
    Sigma.prior.scale = sigma_prior_scale,
    Sigma.prior.dof = sigma_prior_dof,
    dp.prior.alpha = config$dp_prior_alpha,
    dp.prior.beta = config$dp_prior_beta,
    mention.every = NA,
    save.every = NA,
    save.to = NA
  )
  
  # Combinação das cadeias
  coeficientes_1 <- fit.post.chain1$B
  chain_1 <- mcmc(t(coeficientes_1[, , 1:config$Nsamples]))
  
  coeficientes_2 <- fit.post.chain2$B
  chain_2 <- mcmc(t(coeficientes_2[, , 1:config$Nsamples]))
  
  chain1_burned <- window(chain_1)
  chain2_burned <- window(chain_2)
  
  combined_chain <- rbind(chain1_burned, chain2_burned)
  combined_chain <- as.mcmc(combined_chain)
  betas <- colMeans(combined_chain)
  
  # Salvando o modelo ajustado em um arquivo .rds
  saveRDS(combined_chain, file = "saidas/modelo_bayesiano.rds")
  
}

modelo_robusto <- function(resposta, preditores){
  set.seed(23161719)
  X <- preditores
  y <- resposta
  # Preparar fórmula
  formula <- as.formula(paste(y, "~", paste(X, collapse = " + ")))
  modelo_robusto <- rlm(formula, data = dados)
  # Salvando o modelo ajustado em um arquivo .rds
  saveRDS(modelo_robusto, file = "saidas/modelo_robusto.rds")
}