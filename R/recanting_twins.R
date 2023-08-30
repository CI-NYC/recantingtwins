recanting_twins <- function(data, W, A, Z, M, Y, .control = rt_options()) {
    data <- copy(data)
    setDT(data)

    # Fit the propensity score: P(A | W)
    ps(data, W, W, .control)
}
