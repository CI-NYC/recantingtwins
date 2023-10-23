theta1_integral_disc <- function(data, A, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
  
  num_class_M = nrow(unique(data[, c(M), with = FALSE]))
  num_class_Z = nrow(unique(data[, c(Z), with = FALSE]))
  
  pz_pred = matrix(0, nrow(data), num_class_Z)
  data_temp_Ap = assign_value(data, A, ap)
  
  pz_pred = predict(fit_pz, data_temp_Ap, type = "prob")
  res = rep(0, nrow(data))
  
  data_temp_As = assign_value(data, A, as)

  # Compute the integral with sum. Enumerate m and z.
  for(z in 0:(num_class_Z - 1)){
    pz_pred_temp = as.vector(pz_pred[,z + 1])
    data_temp = assign_value(assign_value(data, A, ap), Z, z)
  
    pm_pred = matrix(0, nrow(data), num_class_M)
 
    pm_pred = predict(fit_pm2, data_temp, type = "prob")
    for(m in 0:(num_class_M - 1)){
      pm_pred_temp = as.vector(pm_pred[,m + 1])
      pmz_pred_temp = pm_pred_temp * pz_pred_temp
      
      data_temp = assign_value(assign_value(data_temp_As, Z, z), M, m)
      E_pred_temp = predict(fit_or, data_temp)
      res = rbind(res, pmz_pred_temp * E_pred_temp)
    }
  }
  
  res = apply(res, 2, sum)
  return(res)
}
