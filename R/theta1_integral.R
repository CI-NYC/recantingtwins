theta1_integral <- function(data, A, W, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
  
  num_class_M <- nrow(unique(data[, c(M), with = FALSE]))
  num_class_Z <- nrow(unique(data[, c(Z), with = FALSE]))
  
  pz_pred <- matrix(0, nrow(data), num_class_Z)
  data_temp_A1 <- assign_value(data, A, 1)
  
  pz_pred <- predict(fit_pz, data_temp_A1[,c(A, W), with = F], discrete = F)
  res <- rep(0, nrow(data))
  
  data_temp_A0 <- assign_value(data, A, 0)
  for(z in 0:(num_class_Z - 1)){
    pz_pred_temp <- as.vector(pz_pred[,z + 1])
    data_temp <- assign_value(assign_value(data, A, 1), Z, z)
    
    pm_pred <- matrix(0, nrow(data), num_class_M)
    
    pm_pred <- predict(fit_pm2, data_temp[,c(A, W, Z), with = F], discrete = F)
    for(m in 0:(num_class_M - 1)){
      pm_pred_temp <- as.vector(pm_pred[,m + 1])
      pmz_pred_temp <- pm_pred_temp * pz_pred_temp
      
      data_temp <- assign_value(assign_value(data_temp_A0, Z, z), M, m)
      E_pred_temp <- predict(fit_or, data_temp[,c(A, W, Z, M), with = F], discrete = F)
      res <- rbind(res, pmz_pred_temp * E_pred_temp)
    }
  }
  
  res <- apply(res, 2, sum)
  res
}
