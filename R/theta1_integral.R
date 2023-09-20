theta1_integral <- function(data, A, Z, M, fit_or, fit_pmz, ap = 1, as = 0) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, c(Z, M), with = FALSE])

    tmp <- copy(data)
    tmp[, .recantingtwins_id := 1:.N]
    tmp <- tmp[rep(1:.N, p)]

    hx <- draw_H(hs, nrow(tmp))
    tmp[[Z]] <- hx[[Z]]$draws
    tmp[[M]] <- hx[[M]]$draws

    sapply(split(predict(fit_or, assign_value(tmp, A, as)) *
                     (predict(fit_pmz, assign_value(tmp, A, ap)) /
                          Reduce(`*`, lapply(c(Z, M), function(x) hx[[x]]$px))),
                 tmp$.recantingtwins_id), mean)
}
