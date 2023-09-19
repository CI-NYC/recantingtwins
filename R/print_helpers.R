calc_stderror <- function(eif) {
    sqrt(var(eif) / length(eif))
}

calc_ci <- function(x, eif) {
    se <- calc_stderror(eif)
    x + c(-1, 1)*se*qnorm(0.975)
}
