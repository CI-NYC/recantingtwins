pz_disc <- function(data, A, W, Z, .control) {
  out <- matrix(nrow = nrow(data), ncol = 2)
  colnames(out) <- c("P(Z|0,W)", "P(Z|1,W)")
  
  formula = paste0("as.factor(", Z, ") ~ ", W, " + ", A)
  multi = multinom(as.formula(formula), data=data)
  
  num_class_Z = nrow(unique(data[, c(Z), with = FALSE]))
  
  data_temp = assign_value(data, A, 0)
  
  `P(Z|0,W)` = predict(multi, data_temp, type = "prob")
 
  data_temp = assign_value(data, A, 1)
  
  `P(Z|1,W)` = predict(multi, data_temp, type = "prob")

  # To get P(Z|) with Z the observed outcome, not given values Z = z
 
  Z_data = as.vector(as.matrix(data[,c(Z), with = F]))
  mask_Z = one_hot(as.data.table(as.factor(Z_data)))
  
  `P(Z|0,W)` = rowSums(`P(Z|0,W)` * mask_Z)
  `P(Z|1,W)` = rowSums(`P(Z|1,W)` * mask_Z)
  #xgb_train = NULL
  
  out[, "P(Z|0,W)"] <- `P(Z|0,W)`
  out[, "P(Z|1,W)"] <- `P(Z|1,W)`
  
  list(pred = out,
       fit = multi)
  
}
