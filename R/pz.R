pz <- function(data, A, W, Z, .control) {
    out <- matrix(nrow = nrow(data), ncol = 2)
    colnames(out) <- c("P(Z|0,W)", "P(Z|1,W)")

    fit <- drcde(data = data[, c(A, W, Z), with = FALSE],
                 target = Z,
                 learners = .control$.pz_learners,
                 folds = .control$.pz_folds)

    out[, "P(Z|0,W)"] <- predict(fit, assign_value(data, A, 0))
    out[, "P(Z|1,W)"] <- predict(fit, assign_value(data, A, 1))

    list(pred = out,
         fit = fit)
}
