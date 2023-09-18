If_theta3 <- function(data, A, Y, ps, or, pz, pm2, Eh, Eh1, ap = 1, as = 0) {
    `I(a*)` <- as.numeric(data[[A]] == as)
    `I(a')` <- as.numeric(data[[A]] == ap)
    `P(a*|W)` <- ps[, gl("P({as}|W)")]
    `P(a'|W)` <- ps[, gl("P({ap}|W)")]
    `E(Y|a*,Z,M,W)` <- or[, gl("E(Y|{as},Z,M,W)")]
    `P(Z|a',W)` <- pz[, gl("P(Z|{ap},W)")]
    `P(Z|a*,W)` <- pz[, gl("P(Z|{as},W)")]
    `P(M|a*,Z,W)` <- pm2[, gl("P(M|{as},Z,W)")]
    `P(M|a',Z,W)` <- pm2[, gl("P(M|{ap},Z,W)")]

    uc_If <- (`I(a*)` / `P(a*|W)`)*(data[[Y]] - `E(Y|a*,Z,M,W)`)*(`P(M|a',Z,W)` / `P(M|a*,Z,W)`) +
        (`I(a*)` / `P(a*|W)`)*Eh1 - (`I(a*)` / `P(a*|W)`)*Eh +
        (`I(a')` / `P(a'|W)`)*`E(Y|a*,Z,M,W)`*(`P(Z|a*,W)` / `P(Z|a',W)`) -
            (`I(a')` / `P(a'|W)`)*(`P(Z|a*,W)` / `P(Z|a',W)`)*Eh1 +
        Eh

    theta <- mean(uc_If)
    list(theta = theta,
         If = uc_If - theta)
}
