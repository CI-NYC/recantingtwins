#' Mediation Analysis With Intermediate Confounding Using Recanting Twins
#'
#' @param data \[\code{data.frame}\]\cr
#' @param W trt \[\code{character}\]\cr
#' @param A trt \[\code{character}(1)\]\cr
#' @param Z trt \[\code{character}\]\cr
#' @param M trt \[\code{character}\]\cr
#' @param Y trt \[\code{character(1)}\]\cr
#' @param outcome_type trt \[\code{character}\]\cr
#' @param .control trt \[\code{list}\]\cr
#'
#' @return
#' @export
#'
#' @examples
recanting_twins <- function(data, W, A, Z, M, Y,
                            outcome_type = c("binomial", "continuous"),
                            .control = .recanting_twins_control()) {
    data <- copy(data)
    setDT(data)

    outcome_type <- match.arg(outcome_type)

    # Fit the propensity score: P(A | W)
    fit_ps <- ps(data, W, A, .control)
    # Fit the outcome regression: E(Y|A, Z, M, W)
    fit_or <- or(data, W, A, Z, M, Y, outcome_type, .control)
    # Fit the outcome regression: E(Y|A, W)
    fit_or2 <- or2(data, W, A, Y, outcome_type, .control)

    # theta 0 -----------------------------------------------------------------
    est_theta0 <- aipw(data, A, Y, fit_ps$pred, fit_or2$pred, a = 1)

    # theta 1 -----------------------------------------------------------------
    # Fit the density of Z: P(Z|a,W)
    fit_pz <- pz(data, A, W, Z, .control)
    # Fit the density of M conditional on A,Z,W: P(M|a,Z,W)
    fit_pm2 <- pm2(data, A, W, M, Z, .control)
    # Fit the joint density of M,Z: P(M,Z|a,W)
    fit_pmz <- pmz(data, A, W, M, Z, fit_pz$pred, fit_pm2$pred, .control)
    # Estimate E_h(E(Y|a*, Z, M, W)*P(M,Z|a',W))
    Eh_theta1 <- theta1_integral(data, A, W, Z, M, fit_or$fit, fit_pz$fit, fit_pm2$fit, ap = 1, as = 0)
    # Estimate the EIF for theta 1
    est_theta1 <- If_theta1(data, A, Y, fit_ps$pred, fit_or$pred, fit_pmz$pred, Eh_theta1, ap = 1, as = 0)

    # theta' 1 ----------------------------------------------------------------
    # Fit the density of M conditional on A,W: P(M|a,W)
    fit_pm1 <- pm1(data, A, W, M, .control)
    # Estimate integrals: E_h, E_h1, E_h2
    Eh_thetap1 <- thetap_integral(data, A, W, Z, M, fit_or$fit, fit_pz$fit, fit_pm1$fit, ap = 1, as = 0, aj = 1)
    Eh1_thetap1 <- thetap_integral1(data, A, W, Z, M, fit_or$fit, fit_pm1$fit, ap = 1, as = 0)
    Eh2_thetap1 <- thetap_integral2(data, A, W, Z, M, fit_or$fit, fit_pz$fit, as = 0, aj = 1)

    est_theta_p1 <- If_thetap(data, A, Y, fit_ps$pred, fit_or$pred, fit_pz$pred, fit_pm1$pred, fit_pm2$pred,
                              Eh_thetap1, Eh1_thetap1, Eh2_thetap1, ap = 1, as = 0, aj = 1)

    # theta' 2 ----------------------------------------------------------------
    # Estimate integrals: E_h, E_h1, E_h2
    Eh_thetap2 <- thetap_integral(data, A, W, Z, M, fit_or$fit, fit_pz$fit, fit_pm1$fit, ap = 1, as = 0, aj = 0)
    Eh1_thetap2 <- Eh1_thetap1
    Eh2_thetap2 <- thetap_integral2(data, A, W, Z, M, fit_or$fit, fit_pz$fit, as = 0, aj = 0)

    est_theta_p2 <- If_thetap(data, A, Y, fit_ps$pred, fit_or$pred, fit_pz$pred, fit_pm1$pred, fit_pm2$pred,
                              Eh_thetap2, Eh1_thetap2, Eh2_thetap2, ap = 1, as = 0, aj = 0)

    # theta'' 3 ---------------------------------------------------------------
    Eh_theta3_2p <- theta2p_integral(data, A, W, Z, M, fit_or$fit, fit_pz$fit, fit_pm2$fit, ap = 1, as = 0)
    Eh1_theta3_2p <- theta2p_integral1(data, A, W, Z, M, fit_or$fit, fit_pz$fit, fit_pm2$fit, ap = 1, as = 0)
    Eh2_theta3_2p <- theta2p_integral2(data, A, W, Z, M, fit_or$fit, fit_pz$fit, fit_pm2$fit, ap = 1, as = 0)

    est_theta3_2p <- If_theta3_2p(data, A, Y, fit_ps$pred, fit_or$pred, fit_pz$pred, fit_pm2$pred,
                                  Eh_theta3_2p, Eh1_theta3_2p, Eh2_theta3_2p, ap = 1, as = 0)

    # theta 3 -----------------------------------------------------------------
    # Estimate integrals: E_h, E_h1
    Eh_theta3 <- theta3_integral(data, A, W, Z, M, fit_or$fit, fit_pz$fit, fit_pm2$fit, ap = 1, as = 0)
    Eh1_theta3 <- theta3_integral1(data, A, W, Z, M, fit_or$fit, fit_pm2$fit, ap = 1, as = 0)

    est_theta3 <- If_theta3(data, A, Y, fit_ps$pred, fit_or$pred, fit_pz$pred,
                            fit_pm2$pred, Eh_theta3, Eh1_theta3, ap = 1, as = 0)

    # theta 4 -----------------------------------------------------------------
    est_theta4 <- aipw(data, A, Y, fit_ps$pred, fit_or2$pred, a = 0)

    # estimates ---------------------------------------------------------------
                # A -> Y
    out <- list(p1 = est_theta0$theta - est_theta1$theta,
                eif_p1 = est_theta0$If - est_theta1$If,
                # A -> Z -> Y
                p2 = est_theta_p1$theta - est_theta_p2$theta,
                eif_p2 = est_theta_p1$If - est_theta_p2$If,
                # A -> Z -> M -> Y
                p3 = est_theta_p2$theta - est_theta3_2p$theta,
                eif_p3 = est_theta_p2$If - est_theta3_2p$If,
                # A -> M -> Y
                p4 = est_theta3$theta - est_theta4$theta,
                eif_p4 = est_theta3$If - est_theta4$If,
                # Intermediate confounding
                intermediate_confounding = est_theta1$theta - est_theta_p1$theta + est_theta_p2$theta -
                    est_theta_p2$theta + est_theta3_2p$theta - est_theta3$theta,
                eif_intermediate_confounding = est_theta1$If - est_theta_p1$If + est_theta_p2$If -
                    est_theta_p2$If + est_theta3_2p$If - est_theta3$If,
                ate = est_theta0$theta - est_theta4$theta,
                eif_ate = est_theta0$If - est_theta4$If
                )

    class(out) <- "recantingtwins"
    out
}
