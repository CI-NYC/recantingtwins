pm1_disc <- function(data, A, W, M, .control) {
  out <- matrix(nrow = nrow(data), ncol = 2)
  colnames(out) <- c("P(M|0,W)", "P(M|1,W)")
  
  formula = paste0("as.factor(", M, ") ~ ", W, " + ", A)
  multi = multinom(as.formula(formula), data=data)
  
  data_temp = assign_value(data, A, 0)
  
  `P(M|0,W)` = predict(multi, data_temp, type = "prob")
  
  data_temp = assign_value(data, A, 1)
  
  `P(M|1,W)` = predict(multi, data_temp, type = "prob")
 
  # To get P(M|) with M the observed outcome, not given values M = m
  M_data = as.vector(as.matrix(data[,c(M), with = F]))
  mask_M = one_hot(as.data.table(as.factor(M_data)))
  
  `P(M|0,W)` = rowSums(`P(M|0,W)` * mask_M)
  `P(M|1,W)` = rowSums(`P(M|1,W)` * mask_M)
  
  out[, "P(M|0,W)"] <- `P(M|0,W)`
  out[, "P(M|1,W)"] <- `P(M|1,W)`
  
  list(pred = out,
       fit = multi)
}

pm2_disc <- function(data, A, W, M, Z, .control) {
  
  out <- matrix(nrow = nrow(data), ncol = 2)
  colnames(out) <- c("P(M|0,Z,W)", "P(M|1,Z,W)")
  
  formula = paste0("as.factor(", M, ") ~ ", W, " + ", A, " + ", Z)
  multi = multinom(as.formula(formula), data=data)
  
  data_temp = assign_value(data, A, 0)
  
  `P(M|0,Z,W)` = predict(multi, data_temp, type = "prob")
  
  data_temp = assign_value(data, A, 1)
  
  `P(M|1,Z,W)` = predict(multi, data_temp, type = "prob")
  
  # To get P(M|) with M the observed outcome, not given values M = m
  M_data = as.vector(as.matrix(data[,c(M), with = F]))
  mask_M = one_hot(as.data.table(as.factor(M_data)))
  
  `P(M|0,Z,W)` = rowSums(`P(M|0,Z,W)` * mask_M)
  `P(M|1,Z,W)` = rowSums(`P(M|1,Z,W)` * mask_M)
 
  out[, "P(M|0,Z,W)"] <- `P(M|0,Z,W)`
  out[, "P(M|1,Z,W)"] <- `P(M|1,Z,W)`
  
  list(pred = out,
       fit = multi)
}
