theta3_integral <- function(data, A, Z, M, fit_or, fit_pz, fit_pm2, ap = 1, as = 0) {
    # p <- nrow(data) + 1
    p <- 10
    hs <- H_factory(data[, c(Z, M), with = FALSE])

    Eh <- foreach(i = 1:nrow(data), .options.future = list(seed = TRUE), .combine = c) %dofuture% {
        s <- copy(data[i, ])
        s <- rbindlist(replicate(p, s, simplify = FALSE))
        hx <- draw_H(hs, p)
        s[[Z]] <- hx[[Z]]$draws
        s[[M]] <- hx[[M]]$draws

        r <- (predict(fit_pz, assign_value(s, A, as)) *
                  predict(fit_pm2, assign_value(s, A, ap))) /
            Reduce(`*`, lapply(c(Z, M), function(x) hx[[x]]$px))

        mean(predict(fit_or, assign_value(s, A, as)) * r)
    }

    Eh
}

theta3_integral1 <- function(data, A, M, fit_or, fit_pm2, ap = 1, as = 0) {
    # p <- nrow(data) + 1
    p <- 10
    hs <- H_factory(data[, M, with = FALSE])

    Eh <- foreach(i = 1:nrow(data), .options.future = list(seed = TRUE), .combine = c) %dofuture% {
        s <- copy(data[i, ])
        s <- rbindlist(replicate(p, s, simplify = FALSE))
        hx <- draw_H(hs, p)
        s[[M]] <- hx[[M]]$draws

        r <- predict(fit_pm2, assign_value(s, A, ap)) / hx[[M]]$px
        mean(predict(fit_or, assign_value(s, A, as)) * r)
    }

    Eh
}
