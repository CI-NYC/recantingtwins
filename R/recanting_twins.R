recanting_twins <- function(data, W, A, Z, M, Y,
                            outcome_type = c("binomial", "continuous"),
                            .control = rt_options()) {
    data <- copy(data)
    setDT(data)

    outcome_type <- match.arg(outcome_type)

    # Fit the propensity score: P(A | W)
    ps(data, W, W, .control)
    # Fit the outcome regression: E(Y|A, Z, M, W)
    or(data, W, A, Z, M, Y, outcome_type, .control)
}
