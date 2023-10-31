pz <- function(data, A, W, Z, .control) {
  out <- matrix(nrow = nrow(data), ncol = 2)
  colnames(out) <- c("P(Z|0,W)", "P(Z|1,W)")
  
  fit <- mlr3superlearner(data[,c(A, W, Z), with = FALSE],
                          target = Z, 
                          list(list("multinom", trace = FALSE), "lightgbm", "rpart"), 
                          outcome_type = "multiclass")
  
  num_class_Z <- nrow(unique(data[, c(Z), with = FALSE]))
  
  data_temp <- assign_value(data, A, 0)
  
  `P(Z|0,W)` <- predict(rf, data_temp[,c(A, W), with = F], discrete = F)
  
  data_temp <- assign_value(data, A, 1)
  
  `P(Z|1,W)` <- predict(rf, data_temp[,c(A, W), with = F], discrete = F)
  
  Z_data <- as.vector(as.matrix(data[,c(Z), with = F]))
  mask_Z <- one_hot(as.data.table(as.factor(Z_data)))
  
  `P(Z|0,W)` <- rowSums(`P(Z|0,W)` * mask_Z)
  `P(Z|1,W)` <- rowSums(`P(Z|1,W)` * mask_Z)
  
  out[, "P(Z|0,W)"] <- `P(Z|0,W)`
  out[, "P(Z|1,W)"] <- `P(Z|1,W)`
  
  list(pred = out,
       fit = fit)
}
