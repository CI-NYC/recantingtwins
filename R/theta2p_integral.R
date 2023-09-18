theta2p_integral <- function(data, A, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, c(Z, M), with = FALSE])

    Eh <- foreach(i = 1:nrow(data), .options.future = list(seed = TRUE), .combine = c) %dofuture% {
        s <- copy(data[i, ])
        s <- rbindlist(replicate(p, s, simplify = FALSE))
        # Z' and M
        hx1 <- draw_H(hs, p)
        # Z
        hx2 <- draw_H(hs[Z], p)
        s[[Z]] <- hx1[[Z]]$draws
        s[[M]] <- hx1[[M]]$draws

        `f(M|a',Z',W)` <- predict(fit_pm2, assign_value(s, A, ap))
        `f(Z'|a*,W)` <- predict(fit_pz, assign_value(s, A, as))

        s[[Z]] <- hx2[[Z]]$draws
        `f(Z|a*,W)` <- predict(fit_pz, assign_value(s, A, as))

        `E(Y|a*,Z,M,W)` <- predict(fit_or, assign_value(s, A, as))
        `h(Z, Z', M)` <- Reduce(`*`, lapply(c(Z, M), function(x) hx1[[x]]$px)) * hx2[[Z]]$px

        r <- (`f(Z|a*,W)` * `f(M|a',Z',W)` * `f(Z'|a*,W)`) / `h(Z, Z', M)`
        mean(`E(Y|a*,Z,M,W)` * r)
    }

    Eh
}

theta2p_integral1 <- function(data, A, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, c(Z, M), with = FALSE])

    Eh <- foreach(i = 1:nrow(data), .options.future = list(seed = TRUE), .combine = rbind) %dofuture% {
        out <- matrix(nrow = 1, ncol = 2)
        colnames(out) <- c("E_h1(m,z')", "E_h1(m,z)")

        s <- copy(data[i, ])
        s <- rbindlist(replicate(p, s, simplify = FALSE))

        # Z' and M
        hx <- draw_H(hs, p)
        s[[M]] <- hx[[M]]$draws

        `E(Y|a*,Z=z,M,W)` <- predict(fit_or, assign_value(s, A, as))
        `f(M|a',Z=z,W)` <- predict(fit_pm2, assign_value(s, A, ap))

        s[[Z]] <- hx[[Z]]$draws

        `f(M|a',Z',W)` <- predict(fit_pm2, assign_value(s, A, ap))
        `f(Z'|a*,W)` <- predict(fit_pz, assign_value(s, A, as))
        `E(Y|a*,Z',M,W)` <- predict(fit_or, assign_value(s, A, as))
        `h_1(M, Z')` <- Reduce(`*`, lapply(c(Z, M), function(x) hx[[x]]$px))

        r <- (`f(M|a',Z',W)` * `f(Z'|a*,W)`) / `h_1(M, Z')`
        out[, "E_h1(m,z')"] <- mean(`E(Y|a*,Z=z,M,W)` * r)

        r <- (`f(M|a',Z=z,W)` * `f(Z'|a*,W)`) / `h_1(M, Z')`
        out[, "E_h1(m,z)"] <- mean(`E(Y|a*,Z',M,W)` * r)
        out
    }

    Eh
}

theta2p_integral2 <- function(data, A, Z, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
    # p <- nrow(data) + 1
    p <- 10
    hs <- H_factory(data[, Z, with = FALSE])

    Eh <- foreach(i = 1:nrow(data), .options.future = list(seed = TRUE), .combine = rbind) %dofuture% {
        out <- matrix(nrow = 1, ncol = 2)
        colnames(out) <- c("E_h2(z')", "E_h2(z)")

        s <- copy(data[i, ])
        s <- rbindlist(replicate(p, s, simplify = FALSE))

        # Z'
        hx <- draw_H(hs, p)
        s[[Z]] <- hx[[Z]]$draws

        `f(M|a',Z',W)` <- predict(fit_pm2, assign_value(s, A, ap))
        `f(Z'|a*,W)` <- predict(fit_pz, assign_value(s, A, as))
        `E(Y|a*,Z',M,W)` <- predict(fit_or, assign_value(s, A, as))

        r <- `f(Z'|a*,W)` / hx[[Z]]$px

        out[, "E_h2(z')"] <- mean(`f(M|a',Z',W)` * r)
        out[, "E_h2(z)"] <- mean(`E(Y|a*,Z',M,W)` * r)
        out
    }

    Eh
}
