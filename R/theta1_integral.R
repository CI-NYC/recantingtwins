theta1_integral <- function(data, A, Z, M, fit_or, fit_pmz, ap = 1, as = 0) {
    p <- nrow(data) + 1

    hs <- H_factory(data[, c(Z, M), with = FALSE])

    Eh <- foreach(i = 1:nrow(data), .options.future = list(seed = TRUE), .combine = c) %dofuture% {
        s <- copy(data[i, ])
        s <- rbindlist(replicate(p, s, simplify = FALSE))
        hx <- draw_H(hs, p)
        s[[Z]] <- hx[[Z]]$draws
        s[[M]] <- hx[[M]]$draws

        r <- predict(fit_pmz$fit, assign_value(s, A, ap)) /
            Reduce(`*`, lapply(c(Z, M), function(x) hx[[x]]$px))

        mean(predict(fit_or$fit, assign_value(s, A, as)) * r)
    }

    Eh
}
