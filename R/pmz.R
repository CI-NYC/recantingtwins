pmz <- function(data, A, W, M, Z, .control) {
    out <- matrix(nrow = nrow(data), ncol = 2)
    colnames(out) <- c("P(M,Z|0,W)", "P(M,Z|1,W)")

    fit <- drcde(data = data[, c(A, W, M, Z), with = FALSE],
                 target = c(M, Z),
                 learners = .control$.pmz_learners,
                 folds = .control$.pmz_folds)

    out[, "P(M,Z|0,W)"] <- predict(fit, assign_value(data, A, 0))
    out[, "P(M,Z|1,W)"] <- predict(fit, assign_value(data, A, 1))

    list(pred = out,
         fit = fit)
}
