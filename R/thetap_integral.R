thetap_integral <- function(data, A, Z, M, fit_or, fit_pz, fit_pm1, ap = 1, as = 0, aj) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, c(Z, M), with = FALSE])

    tmp <- copy(data)
    tmp[, .recantingtwins_id := 1:.N]
    tmp <- tmp[rep(1:.N, p)]

    hx <- draw_H(hs, p)
    tmp[[Z]] <- hx[[Z]]$draws
    tmp[[M]] <- hx[[M]]$draws

    sapply(split(predict(fit_or, assign_value(tmp, A, as)) *
                     ((predict(fit_pz, assign_value(tmp, A, aj)) *
                           predict(fit_pm1, assign_value(tmp, A, ap))) /
                          Reduce(`*`, lapply(c(Z, M), function(x) hx[[x]]$px))),
                 tmp$.recantingtwins_id), mean)
}

thetap_integral1 <- function(data, A, M, fit_or, fit_pm1, ap = 1, as = 0) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, M, with = FALSE])

    tmp <- copy(data)
    tmp[, .recantingtwins_id := 1:.N]
    tmp <- tmp[rep(1:.N, p)]

    hx <- draw_H(hs, p)
    tmp[[M]] <- hx[[M]]$draws

    sapply(split(predict(fit_or, assign_value(tmp, A, as)) *
                     (predict(fit_pm1, assign_value(tmp, A, ap)) / hx[[M]]$px),
                 tmp$.recantingtwins_id), mean)
}

thetap_integral2 <- function(data, A, Z, fit_or, fit_pz, as = 0, aj) {
    p <- nrow(data) + 1
    hs <- H_factory(data[, Z, with = FALSE])

    tmp <- copy(data)
    tmp[, .recantingtwins_id := 1:.N]
    tmp <- tmp[rep(1:.N, p)]

    hx <- draw_H(hs, p)
    tmp[[Z]] <- hx[[Z]]$draws

    sapply(split(predict(fit_or, assign_value(tmp, A, as)) *
                     (predict(fit_pz, assign_value(tmp, A, aj)) / hx[[Z]]$px),
                 tmp$.recantingtwins_id), mean)
}
