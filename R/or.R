or_disc <- function(data, W, A, Z, M, Y, outcome_type, .control) {
    out <- matrix(nrow = nrow(data), ncol = 2)
    colnames(out) <- c("E(Y|0,Z,M,W)", "E(Y|1,Z,M,W)")
    
    fit <- mlr3superlearner(data = data[, c(Y, A, Z, M, W), with = FALSE],
                            target = Y,
                            library = .control$.m_learners,
                            outcome_type = outcome_type,
                            folds = .control$.m_folds)
    
    data_temp = assign_value(data, A, 0)
    out[, "E(Y|0,Z,M,W)"] <- predict(fit, data_temp[,c(A, W, Z, M), with = F], discrete = F)
    
    data_temp = assign_value(data, A, 1)
    out[, "E(Y|1,Z,M,W)"] <- predict(fit, data_temp[,c(A, W, Z, M), with = F], discrete = F)
    
    #browser()
    list(pred = out,
        fit = fit)
      }
      
or2_disc <- function(data, W, A, Y, outcome_type, .control) {
    out <- matrix(nrow = nrow(data), ncol = 2)
    colnames(out) <- c("E(Y|0,W)", "E(Y|1,W)")

    fit <- mlr3superlearner(data = data[, c(Y, A, W), with = FALSE],
                            target = Y,
                            library = .control$.m_learners,
                            outcome_type = outcome_type,
                            folds = .control$.m_folds)

    data_temp = assign_value(data, A, 0)
    out[, "E(Y|0,W)"] <- predict(fit, data_temp[,c(A, W), with = F], discrete = F)

    data_temp = assign_value(data, A, 1)
    out[, "E(Y|1,W)"] <- predict(fit, data_temp[,c(A, W), with = F], discrete = F)

    #browser()
    list(pred = out,
        fit = fit)
}
