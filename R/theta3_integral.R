theta3_integral <- function(data, A, W, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
  num_class_M <- nrow(unique(data[, c(M), with = FALSE]))
  num_class_Z <- nrow(unique(data[, c(Z), with = FALSE]))

  pz_pred <- matrix(0, nrow(data), num_class_Z)
  data_temp_As <- assign_value(data, A, as)

  pz_pred <- predict(fit_pz, data_temp_As[,c(A, W), with = F], discrete = F)
  res <- rep(0, nrow(data))

  # Compute integral by sum, enumerate z and m.
  for (z in 0:(num_class_Z - 1)) {
    pz_pred_temp <- as.vector(pz_pred[,z + 1])

    data_temp_ApZ <- assign_value(assign_value(data, A, ap), Z, z)
    pm_pred <- predict(fit_pm2, data_temp_ApZ[,c(A, W, Z), with = F], discrete = F)
    for (m in 0:(num_class_M - 1)) {
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

theta3_integral1 <- function(data, A, W, Z, M, fit_or, fit_pm2, ap = 1, as = 0) {
  num_class_M <- nrow(unique(data[, c(M), with = FALSE]))

  data_temp_Ap <- assign_value(data, A, ap)
  data_temp_As <- assign_value(data, A, as)
  res <- rep(0, nrow(data))

  pm_pred <- predict(fit_pm2, data_temp_Ap[,c(A, W, Z), with = F], discrete = F)
  for (m in 0:(num_class_M - 1)) {
    pm_pred_temp <- as.vector(pm_pred[,m + 1])

    data_temp <- assign_value(data_temp_As, M, m)
    E_pred_temp <- predict(fit_or, data_temp[,c(A, W, Z, M), with = F], discrete = F)
    res <- rbind(res, pm_pred_temp * E_pred_temp)
  }

  res <- apply(res, 2, sum)
  return(res)
}
