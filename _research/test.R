g <- function(a, w) {
    pscore <- plogis(w - 0.5)
    a * pscore + (1 - a) * (1 - pscore)
}

pZ <- function(z, a, w) {
    prob1 <- plogis(-0.9 + a + 0.8*w)
    z * prob1 + (1 - z) * (1 - prob1)
}

pM <- function(m, z, a, w) {
    gamma1 <- 0.5
    # gamma1 <- 0
    gamma2 <- 1.5
    prob1 <- plogis(-1 + gamma1*z + gamma2*a + 0.5*w)
    m * prob1 + (1 - m) * (1 - prob1)
}

my <- function(m, z, a, w) {
    # beta1 <- 1.5
    beta1 <- 0
    beta2 <- 1
    plogis(0.5*m + beta1*z + beta2*a - 0.6*w - 1.2)
}

gendata <- function(N) {
    w <- rbinom(N, 1, .5)
    a <- rbinom(N, 1, g(1, w))
    z <- rbinom(N, 1, pZ(1, a, w))
    m <- rbinom(N, 1, pM(1, z, a, w))
    y <- rbinom(N, 1, my(m, z, a, w))
    data.frame(W = w, A = a, Z = z, M = m, Y = y)
}

tmp <- gendata(1000)

library(recantingtwins)
library(mlr3extralearners)

params <- .recanting_twins_control(.pmz_learners = c("xgboost"),
                                   .pz_learners = c("xgboost"),
                                   .pm1_learners = c("xgboost"),
                                   .pm2_learners = c("xgboost"))

recanting_twins(tmp, "W", "A", "Z", "M", "Y", "binomial", .control = params)

