thetap_integral_disc <- function(data, A, W, Z, M, fit_or, fit_pz, fit_pm1, ap = 1, as = 0, aj) {
  
  # fit_or -> as -> 0
  # fit_pz -> aj -> ?
  # fit_pm1 -> ap -> 1
  num_class_M <- nrow(unique(data[, c(M), with = FALSE]))
  num_class_Z <- nrow(unique(data[, c(Z), with = FALSE]))
  
  pz_pred <- matrix(0, nrow(data), num_class_Z)
  data_temp_Aj <- assign_value(data, A, aj)
  
  pz_pred <- predict(fit_pz, data_temp_Aj[,c(A, W), with = F], discrete = F)
  res <- rep(0, nrow(data))
  
  data_temp_As <- assign_value(data, A, as)
  for(z in 0:(num_class_Z - 1)){
    pz_pred_temp <- as.vector(pz_pred[,z + 1])
    
    data_temp_Ap <- assign_value(data, A, ap)
    pm_pred <- predict(fit_pm1, data_temp_Ap[,c(A, W), with = F], discrete = F)
    for(m in 0:(num_class_M - 1)){
      pm_pred_temp <- as.vector(pm_pred[,m + 1])
      pmz_pred_temp <- pm_pred_temp * pz_pred_temp
      
      data_temp <- assign_value(assign_value(data_temp_As, Z, z), M, m)
      E_pred_temp <- predict(fit_or, data_temp[,c(A, W, Z, M), with = F], discrete = F)
      res <- rbind(res, pmz_pred_temp * E_pred_temp)
    }
  }
  
  res <- apply(res, 2, sum)
  return(res)
}

thetap_integral1_disc <- function(data, A, W, Z, M, fit_or, fit_pm1, ap = 1, as = 0) {
  
  # fit_or -> as -> 0
  # fit_pm1 -> ap -> 1
  num_class_M <- nrow(unique(data[, c(M), with = FALSE]))
  
  res <- rep(0, nrow(data))
  
  data_temp_As <- assign_value(data, A, as)
  
  data_temp_Ap <- assign_value(data, A, ap)
  pm_pred <- predict(fit_pm1, data_temp_Ap[,c(A, W), with = F], discrete = F)
  for(m in 0:(num_class_M - 1)){
    pm_pred_temp <- as.vector(pm_pred[,m + 1])
    
    data_temp <- assign_value(data_temp_As, M, m)
    E_pred_temp <- predict(fit_or, data_temp[,c(A, W, Z, M), with = F], discrete = F)
    res <- rbind(res, pm_pred_temp * E_pred_temp)
  }
  
  res <- apply(res, 2, sum)
  return(res)
}

thetap_integral2_disc <- function(data, A, W, Z, M, fit_or, fit_pz, as = 0, aj) {
  
  # fit_or -> as -> 0
  # fit_pz -> aj -> ?
  num_class_Z <- nrow(unique(data[, c(Z), with = FALSE]))
  
  data_temp_Aj <- assign_value(data, A, aj)
  pz_pred <- predict(fit_pz, data_temp_Aj[,c(A, W), with = F], discrete = F)
  
  data_temp_As <- assign_value(data, A, as)
  res <- rep(0, nrow(data))
  
  for(z in 0:(num_class_Z - 1)){
    pz_pred_temp <- as.vector(pz_pred[,z + 1])
    
    data_temp <- assign_value(data_temp_As, Z, z)
    E_pred_temp <- predict(fit_or, data_temp[,c(A, W, Z, M), with = F], discrete = F)
    res <- rbind(res, pz_pred_temp * E_pred_temp)
  }
  
  res <- apply(res, 2, sum)
  return(res)
}
