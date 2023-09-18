aipw <- function(data, A, Y, ps, or, a) {
    `I(a)` <- as.numeric(data[[A]] == a)
    `P(a|W)` <- ps[, gl("P({a}|W)")]
    `E(Y|a,W)` <- or[, gl("E(Y|{a},W)")]

    uc_If <- (`I(a)` / `P(a|W)`)*(data[[Y]] - `E(Y|a,W)`) + `E(Y|a,W)`

    theta <- mean(uc_If)
    list(theta = theta,
         If = uc_If - theta)
}
