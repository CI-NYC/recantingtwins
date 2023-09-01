If_thetap <- function(data, A, Y, ps, or, pz, pm1, pm2, Eh, Eh1, Eh2, ap = 1, as = 0, aj) {
    `I(a*)` <- as.numeric(data[[A]] == as)
    `I(a')` <- as.numeric(data[[A]] == ap)
    `I(a_j)` <- as.numeric(data[[A]] == aj)
    `P(a*|W)` <- ps[, gl("P({as}|W)")]
    `P(a'|W)` <- ps[, gl("P({ap}|W)")]
    `P(a_j|W)` <- ps[, gl("P({aj}|W)")]
    `E(Y|a*,Z,M,W)` <- or[, gl("E(Y|{as},Z,M,W)")]
    `P(Z|aj,W)` <- pz[, gl("P(Z|{aj},W)")]
    `P(Z|a*,W)` <- pz[, gl("P(Z|{as},W)")]
    `P(M|a',W)` <- pm1[, gl("P(M|{ap},W)")]
    `P(M|a*,Z,W)` <- pm2[, gl("P(M|{as},Z,W)")]

    uc_If <- (`I(a*)` / `P(a*|W)`)*(`P(Z|aj,W)` / `P(Z|a*,W)`)*(`P(M|a',W)` / `P(M|a*,Z,W)`)*(data[[Y]] - `E(Y|a*,Z,M,W)`) +
        (`I(a_j)` / `P(a_j|W)`)*Eh1 - (`I(a_j)` / `P(a_j|W)`)*Eh +
        (`I(a')` / `P(a'|W)`)*Eh2 - (`I(a')` / `P(a'|W)`)*Eh +
        Eh

    theta <- mean(uc_If)
    list(theta = theta,
         If = uc_If - theta)
}
