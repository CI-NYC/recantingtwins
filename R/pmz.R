pmz <- function(data, A, W, M, Z, fit_pz, fit_pm2, .control) {
  # Note that here Y is a vector of data used for one_hot
  out <- matrix(nrow = nrow(data), ncol = 2)
  colnames(out) <- c("P(M,Z|0,W)", "P(M,Z|1,W)")
  
  `P(Z|0,W)` <- fit_pz[, "P(Z|0,W)"]
  `P(Z|1,W)` <- fit_pz[, "P(Z|1,W)"]
  
  `P(M|0,Z,W)` <- fit_pm2[, "P(M|0,Z,W)"]
  `P(M|1,Z,W)` <- fit_pm2[, "P(M|1,Z,W)"]
  
  M_data <- as.vector(as.matrix(data[,c(M), with = F]))
  Z_data <- as.vector(as.matrix(data[,c(Z), with = F]))
  
  # To get P(M|) and P(Z|) with M, Z the observed outcome, 
  # not given values M = m, Z = z
  mask_M <- one_hot(as.data.table(as.factor(M_data)))
  mask_Z <- one_hot(as.data.table(as.factor(Z_data)))
  
  `P(Z|0,W)` <- rowSums(`P(Z|0,W)` * mask_Z)
  `P(M|0,Z,W)` <- rowSums(`P(M|0,Z,W)` * mask_M)
  `P(Z|1,W)` <- rowSums(`P(Z|1,W)` * mask_Z)
  `P(M|1,Z,W)` <- rowSums(`P(M|1,Z,W)` * mask_M)
  
  out[, "P(M,Z|0,W)"] <- `P(Z|0,W)` * `P(M|0,Z,W)`
  out[, "P(M,Z|1,W)"] <- `P(Z|1,W)` * `P(M|1,Z,W)`
  
  list(pred = out)
}
