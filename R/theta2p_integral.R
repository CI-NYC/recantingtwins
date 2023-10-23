theta2p_integral_disc <- function(data, A, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
  # fit_pm2 -> ap
  # fit_or -> as
  # fit_pz -> as
  num_class_M = nrow(unique(data[, c(M), with = FALSE]))
  num_class_Z = nrow(unique(data[, c(Z), with = FALSE]))
  
  pzp_pred = matrix(0, nrow(data), num_class_Z)
  data_temp_As = assign_value(data, A, as)
  
  pzp_pred = predict(fit_pz, data_temp_As, type = "prob")
  pz_pred = predict(fit_pz, data_temp_As, type = "prob")
  
  res = rep(0, nrow(data))

  # Compute integral by sum, enumerate z' (zp), z and m.
  for(zp in 0:(num_class_Z - 1)){
    pzp_pred_temp = as.vector(pzp_pred[,zp + 1])
    data_temp_ApZp = assign_value(assign_value(data, A, ap), Z, zp)
    
    pm_pred = predict(fit_pm2, data_temp_ApZp, type = "prob")
    
    for(z in 0:(num_class_Z - 1)){
      pz_pred_temp = as.vector(pz_pred[,z + 1])
      for(m in 0:(num_class_M - 1)){
        pm_pred_temp = as.vector(pm_pred[,m + 1])
        
        data_temp = assign_value(assign_value(data_temp_As, Z, z), M, m)
        E_pred_temp = predict(fit_or, data_temp)
        res = rbind(res, pm_pred_temp * pz_pred_temp * pzp_pred_temp * E_pred_temp)
      }
    }
  }
  
  res = apply(res, 2, sum)
  return(res)
}

theta2p_integral1_disc <- function(data, A, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
  # fit_or -> as
  # fit_pz -> as
  # fit_pm2 -> ap
  out <- matrix(nrow = nrow(data), ncol = 2)
  colnames(out) <- c("E_h1(m,z')", "E_h1(m,z)")
  
  num_class_Z = nrow(unique(data[, c(Z), with = FALSE]))
  num_class_M = nrow(unique(data[, c(M), with = FALSE]))
  
  pzp_pred = matrix(0, nrow(data), num_class_Z)
  data_temp_As = assign_value(data, A, as)
  
  pzp_pred = predict(fit_pz, data_temp_As, type = "prob")
  
  res = rep(0, nrow(data))

    # Compute integral by sum, enumerate z' (zp) and m.
  for(zp in 0:(num_class_Z - 1)){
    pzp_pred_temp = as.vector(pzp_pred[,zp+1])
      data_temp_Ap = assign_value(data, A, ap)
      
      pm_pred = predict(fit_pm2, data_temp_Ap, type = "prob")
      
      for(m in 0:(num_class_M - 1)){
        pm_pred_temp = as.vector(pm_pred[,m + 1])
        
        data_temp = assign_value(assign_value(data_temp_As, Z, zp), M, m)
        E_pred_temp = predict(fit_or, data_temp)
        res = rbind(res, pm_pred_temp * pzp_pred_temp * E_pred_temp)
      }
  }
  
  res = apply(res, 2, sum)
  
  res2 = rep(0, nrow(data))

  # Compute integral by sum, enumerate z' (zp) and m.
  for(zp in 0:(num_class_Z - 1)){
    pzp_pred_temp = as.vector(pzp_pred[,zp+1])
    
    data_temp_ApZp = assign_value(assign_value(data, A, ap), Z, zp)
    
    pm_pred = predict(fit_pm2, data_temp_ApZp, type = "prob")
    
      for(m in 0:(num_class_M - 1)){
        pm_pred_temp = as.vector(pm_pred[,m + 1])
        
        data_temp = assign_value(data_temp_As, M, m)
        E_pred_temp = predict(fit_or, data_temp)
        res2 = rbind(res2, pm_pred_temp * pzp_pred_temp * E_pred_temp)
      }
  }
  
  res2 = apply(res2, 2, sum)
  out[, "E_h1(m,z')"] <- res2
  out[, "E_h1(m,z)"] <- res
  return(out)
}


theta2p_integral2_disc <- function(data, A, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
  # fit_or -> as
  # fit_pz -> as
  # fit_pm2 -> ap
  out <- matrix(nrow = nrow(data), ncol = 2)
  colnames(out) <- c("E_h2(z')", "E_h2(z)")
  
  num_class_M = nrow(unique(data[, c(M), with = FALSE]))
  num_class_Z = nrow(unique(data[, c(Z), with = FALSE]))
  
  pzp_pred = matrix(0, nrow(data), num_class_Z)
  data_temp_As = assign_value(data, A, as)
  
  pzp_pred = predict(fit_pz, data_temp_As, type = "prob")
  
  res = rep(0, nrow(data))

  # Compute integral by sum, enumerate z' (zp).
  for(zp in 0:(num_class_Z - 1)){
    pzp_pred_temp = as.vector(pzp_pred[,zp + 1])
    
      data_temp_ApZp = assign_value(assign_value(data, A, ap), Z, zp)
      
      pm_pred = predict(fit_pm2, data_temp_ApZp, type = "prob")
      
      M_data = as.vector(as.matrix(data[,c(M), with = F]))
      mask_M = one_hot(as.data.table(as.factor(M_data)))
     
      pm_pred_temp = rowSums(pm_pred * mask_M)
        
      res = rbind(res, pm_pred_temp * pzp_pred_temp)
  }
  
  res = apply(res, 2, sum)
  
  res2 = rep(0, nrow(data))

  # Compute integral by sum, enumerate z' (zp).
  for(zp in 0:(num_class_Z - 1)){
    pzp_pred_temp = as.vector(pzp_pred[,zp + 1])
    
        data_temp = assign_value(data_temp_As, Z, zp)
        E_pred_temp = predict(fit_or, data_temp)
        res2 = rbind(res2, pzp_pred_temp * E_pred_temp)
  }
  
  res2 = apply(res2, 2, sum)
  out[, "E_h2(z')"] <- res
  out[, "E_h2(z)"] <- res2
  return(out)
}
