pm1 <- function(data, A, W, M, .control) {
    out <- matrix(nrow = nrow(data), ncol = 2)
    colnames(out) <- c("P(M|0,W)", "P(M|1,W)")

    fit <- drcde(data = data[, c(A, W, M), with = FALSE],
                 target = M,
                 learners = .control$.pm1_learners,
                 folds = .control$.pm1_folds)

    out[, "P(M|0,W)"] <- predict(fit, assign_value(data, A, 0))
    out[, "P(M|1,W)"] <- predict(fit, assign_value(data, A, 1))

    list(pred = out,
         fit = fit)
}

pm2 <- function(data, A, W, M, Z, .control) {
    out <- matrix(nrow = nrow(data), ncol = 2)
    colnames(out) <- c("P(M|0,Z,W)", "P(M|1,Z,W)")

    fit <- drcde(data = data[, c(A, W, M, Z), with = FALSE],
                 target = M,
                 learners = .control$.pm2_learners,
                 folds = .control$.pm2_folds)

    out[, "P(M|0,Z,W)"] <- predict(fit, assign_value(data, A, 0))
    out[, "P(M|1,Z,W)"] <- predict(fit, assign_value(data, A, 1))

    list(pred = out,
         fit = fit)
}
