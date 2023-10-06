theta2p_integral <- function(data, A, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, c(Z, M), with = FALSE])

    tmp <- copy(data)
    tmp[, .recantingtwins_id := 1:.N]
    tmp <- tmp[rep(1:.N, p)]

    # Z' and M
    hx1 <- draw_H(hs, nrow(tmp))
    # Z
    hx2 <- draw_H(hs[Z], nrow(tmp))
    tmp[[Z]] <- hx1[[Z]]$draws
    tmp[[M]] <- hx1[[M]]$draws

    `f(M|a',Z',W)` <- predict(fit_pm2, assign_value(tmp, A, ap))
    `f(Z'|a*,W)` <- predict(fit_pz, assign_value(tmp, A, as))

    tmp[[Z]] <- hx2[[Z]]$draws
    `f(Z|a*,W)` <- predict(fit_pz, assign_value(tmp, A, as))

    `E(Y|a*,Z,M,W)` <- predict(fit_or, assign_value(tmp, A, as))
    `h(Z, Z', M)` <- Reduce(`*`, lapply(c(Z, M), function(x) hx1[[x]]$px)) * hx2[[Z]]$px

    r <- (`f(Z|a*,W)` * `f(M|a',Z',W)` * `f(Z'|a*,W)`) / `h(Z, Z', M)`
    sapply(split(`E(Y|a*,Z,M,W)` * r, tmp$.recantingtwins_id), mean)
}

theta2p_integral1 <- function(data, A, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, c(Z, M), with = FALSE])

    out <- matrix(nrow = nrow(data), ncol = 2)
    colnames(out) <- c("E_h1(m,z')", "E_h1(m,z)")

    tmp <- copy(data)
    tmp[, .recantingtwins_id := 1:.N]
    tmp <- tmp[rep(1:.N, p)]

    # Z' and M
    hx <- draw_H(hs, nrow(tmp))
    tmp[[M]] <- hx[[M]]$draws

    `E(Y|a*,Z=z,M,W)` <- predict(fit_or, assign_value(tmp, A, as))
    `f(M|a',Z=z,W)` <- predict(fit_pm2, assign_value(tmp, A, ap))

    tmp[[Z]] <- hx[[Z]]$draws

    `f(M|a',Z',W)` <- predict(fit_pm2, assign_value(tmp, A, ap))
    `f(Z'|a*,W)` <- predict(fit_pz, assign_value(tmp, A, as))
    `E(Y|a*,Z',M,W)` <- predict(fit_or, assign_value(tmp, A, as))
    `h_1(M, Z')` <- Reduce(`*`, lapply(c(Z, M), function(x) hx[[x]]$px))

    r <- (`f(M|a',Z',W)` * `f(Z'|a*,W)`) / `h_1(M, Z')`
    out[, "E_h1(m,z')"] <- sapply(split(`E(Y|a*,Z=z,M,W)` * r, tmp$.recantingtwins_id), mean)

    r <- (`f(M|a',Z=z,W)` * `f(Z'|a*,W)`) / `h_1(M, Z')`
    out[, "E_h1(m,z)"] <- sapply(split(`E(Y|a*,Z',M,W)` * r, tmp$.recantingtwins_id), mean)

    out
}

theta2p_integral2 <- function(data, A, Z, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, Z, with = FALSE])

    out <- matrix(nrow = nrow(data), ncol = 2)
    colnames(out) <- c("E_h2(z')", "E_h2(z)")

    tmp <- copy(data)
    tmp[, .recantingtwins_id := 1:.N]
    tmp <- tmp[rep(1:.N, p)]

    # Z'
    hx <- draw_H(hs, nrow(tmp))
    tmp[[Z]] <- hx[[Z]]$draws

    `f(M|a',Z',W)` <- predict(fit_pm2, assign_value(tmp, A, ap))
    `f(Z'|a*,W)` <- predict(fit_pz, assign_value(tmp, A, as))
    `E(Y|a*,Z',M,W)` <- predict(fit_or, assign_value(tmp, A, as))

    r <- `f(Z'|a*,W)` / hx[[Z]]$px

    out[, "E_h2(z')"] <- sapply(split(`f(M|a',Z',W)` * r, tmp$.recantingtwins_id), mean)
    out[, "E_h2(z)"] <- sapply(split(`E(Y|a*,Z',M,W)` * r, tmp$.recantingtwins_id), mean)
    out
}
