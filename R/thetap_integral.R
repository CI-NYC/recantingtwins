thetap_integral <- function(data, A, Z, M, fit_or, fit_pz, fit_pm1, ap = 1, as = 0, aj) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, c(Z, M), with = FALSE])

    Eh <- foreach(i = 1:nrow(data), .options.future = list(seed = TRUE), .combine = c) %dofuture% {
        s <- copy(data[i, ])
        s <- rbindlist(replicate(p, s, simplify = FALSE))
        hx <- draw_H(hs, p)
        s[[Z]] <- hx[[Z]]$draws
        s[[M]] <- hx[[M]]$draws

        r <- (predict(fit_pz, assign_value(s, A, aj))*
                  predict(fit_pm1, assign_value(s, A, ap))) /
            Reduce(`*`, lapply(c(Z, M), function(x) hx[[x]]$px))

        mean(predict(fit_or, assign_value(s, A, as)) * r)
    }

    Eh
}

thetap_integral1 <- function(data, A, M, W, fit_or, fit_pm1, ap = 1, as = 0) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, M, with = FALSE])

    Eh <- foreach(i = 1:nrow(data), .options.future = list(seed = TRUE), .combine = c) %dofuture% {
        s <- copy(data[i, ])
        s <- rbindlist(replicate(p, s, simplify = FALSE))
        hx <- draw_H(hs, p)
        s[[M]] <- hx[[M]]$draws

        r <- predict(fit_pm1, assign_value(s, A, ap)) / hx[[M]]$px
        mean(predict(fit_or, assign_value(s, A, as)) * r)
    }

    Eh
}

thetap_integral2 <- function(data, A, Z, W, fit_or, fit_pz, as = 0, aj) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, Z, with = FALSE])

    Eh <- foreach(i = 1:nrow(data), .options.future = list(seed = TRUE), .combine = c) %dofuture% {
        s <- copy(data[i, ])
        s <- rbindlist(replicate(p, s, simplify = FALSE))
        hx <- draw_H(hs, p)
        s[[Z]] <- hx[[Z]]$draws

        r <- predict(fit_pz, assign_value(s, A, aj)) / hx[[Z]]$px
        mean(predict(fit_or, assign_value(s, A, as)) * r)
    }

    Eh
}
