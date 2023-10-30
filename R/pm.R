pm1 <- function(data, A, W, M, .control) {
  out <- matrix(nrow = nrow(data), ncol = 2)
  colnames(out) <- c("P(M|0,W)", "P(M|1,W)")
  
  fit <- mlr3superlearner(data[,c(A, W, M), with = FALSE],
                          target = M, 
                          list(list("multinom", trace = FALSE), "lightgbm", "rpart"), 
                          outcome_type = "multiclass")
  
  data_temp <- assign_value(data, A, 0)
  
  `P(M|0,W)` <- predict(rf, data_temp[,c(A, W), with = F], discrete = F)
  
  data_temp <- assign_value(data, A, 1)
  
  `P(M|1,W)` <- predict(rf, data_temp[,c(A, W), with = F], discrete = F)
  
  M_data <- as.vector(as.matrix(data[,c(M), with = F]))
  mask_M <- one_hot(as.data.table(as.factor(M_data)))
  
  `P(M|0,W)` <- rowSums(`P(M|0,W)` * mask_M)
  `P(M|1,W)` <- rowSums(`P(M|1,W)` * mask_M)
  
  out[, "P(M|0,W)"] <- `P(M|0,W)`
  out[, "P(M|1,W)"] <- `P(M|1,W)`
  
  list(pred = out,
       fit = fit)
}

pm2 <- function(data, A, W, M, Z, .control) {
  
  out <- matrix(nrow <- nrow(data), ncol = 2)
  colnames(out) <- c("P(M|0,Z,W)", "P(M|1,Z,W)")
  
  fit <- mlr3superlearner(data[,c(A, W, Z, M), with = FALSE],
                          target = M, 
                          list(list("multinom", trace = FALSE), "lightgbm", "rpart"), 
                          outcome_type = "multiclass")
  
  data_temp <- assign_value(data, A, 0)
  
  `P(M|0,Z,W)` <- predict(rf, data_temp[,c(A, W, Z), with = F], discrete = F)
  
  data_temp <- assign_value(data, A, 1)
  
  `P(M|1,Z,W)` <- predict(rf, data_temp[,c(A, W, Z), with = F], discrete = F)
  
  M_data <- as.vector(as.matrix(data[,c(M), with = F]))
  mask_M <- one_hot(as.data.table(as.factor(M_data)))
  
  `P(M|0,Z,W)` <- rowSums(`P(M|0,Z,W)` * mask_M)
  `P(M|1,Z,W)` <- rowSums(`P(M|1,Z,W)` * mask_M)
  
  out[, "P(M|0,Z,W)"] <- `P(M|0,Z,W)`
  out[, "P(M|1,Z,W)"] <- `P(M|1,Z,W)`
  
  list(pred = out,
       fit = fit)
}
