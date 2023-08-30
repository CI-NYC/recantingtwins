or <- function(data, W, A, Z, M, Y, outcome_type, .control) {
    out <- matrix(nrow = nrow(data), ncol = 2)
    colnames(out) <- c("E(Y|0, Z, M, W)", "E(Y|1, Z, M, W)")

    fit <- mlr3superlearner(data = data[, c(Y, A, Z, M, W), with = FALSE],
                            target = Y,
                            library = .control$.m_learners,
                            outcome_type = outcome_type,
                            folds = .control$.m_folds)

    out[, "E(Y|0, Z, M, W)"] <- predict(fit, assign_value(data, A, 0))
    out[, "E(Y|1, Z, M, W)"] <- predict(fit, assign_value(data, A, 1))

    list(pred = out,
         fit = fit)
}
