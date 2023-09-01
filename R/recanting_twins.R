recanting_twins <- function(data, W, A, Z, M, Y,
                            outcome_type = c("binomial", "continuous"),
                            .control = rt_control()) {
    data <- copy(data)
    setDT(data)

    outcome_type <- match.arg(outcome_type)

    # Fit the propensity score: P(A | W)
    fit_ps <- ps(data, W, A, .control)
    # Fit the outcome regression: E(Y|A, Z, M, W)
    fit_or <- or(data, W, A, Z, M, Y, outcome_type, .control)

    # theta 0 -----------------------------------------------------------------

    # theta 1 -----------------------------------------------------------------
    # Fit the joint density of M,Z: P(M,Z|a,W)
    fit_pmz <- pmz(data, A, W, M, Z, .control)
    # Estimate E_h(E(Y|a*, Z, M, W)*P(M,Z|a',W))
    Eh_theta1 <- theta1_integral(data, A, Z, M, fit_or, fit_pmz, ap = 1, as = 0)
    # Estimate the EIF for theta 1
    est_theta1 <- If_theta1(data, A, Y, fit_ps$pred, fit_or$pred, fit_pmz$pred, Eh_theta1, ap = 1, as = 0)

    # theta' 1 ----------------------------------------------------------------
    # Fit the density of Z: P(Z|a,W)
    fit_pz <- pz(data, A, W, Z, .control)
    # Fit the density of M conditional on A,W: P(M|a,W)
    fit_pm1 <- pm1(data, A, W, M, .control)
    # Fit the density of M conditional on A,Z,W: P(M|a,Z,W)
    fit_pm2 <- pm2(data, A, W, M, Z, .control)

}
