If_theta1 <- function(data, A, Y, ps, or, pmz, Eh, ap = 1, as = 0) {
    `I(a*)` <- as.numeric(data[[A]] == as)
    `I(a')` <- as.numeric(data[[A]] == ap)
    `P(a*|W)` <- ps[, gl("P({as}|W)")]
    `P(a'|W)` <- ps[, gl("P({ap}|W)")]
    `P(Z,M|a',W)` <- pmz[, gl("P(M,Z|{ap},W)")]
    `P(Z,M|a*,W)` <- pmz[, gl("P(M,Z|{as},W)")]
    `E(Y|a*,Z,M,W)` <- or[, gl("E(Y|{as},Z,M,W)")]

    uc_If <- (`I(a*)` / `P(a*|W)`)*(`P(Z,M|a',W)` / `P(Z,M|a*,W)`)*(data[[Y]] - `E(Y|a*,Z,M,W)`) +
        (`I(a')` / `P(a'|W)`)*`E(Y|a*,Z,M,W)` - (`I(a')` / `P(a'|W)`)*Eh +
        Eh

    theta <- mean(uc_If)
    list(theta = theta,
         If = uc_If - theta)
}
