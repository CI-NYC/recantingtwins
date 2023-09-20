g <- function(a) {
    pscore <- .5
    a * pscore + (1 - a) * (1 - pscore)
}

pZ <- function(z, a, w) {
    prob1 <- plogis(-log(2) + (log(10) * a) - log(2) * w[, "W1"])
    z * prob1 + (1 - z) * (1 - prob1)
}

pM <- function(m, z, a, w) {
    prob1 <- plogis(-log(2) + log(12) * z - log(1.4) * w[, "W1"])
    m * prob1 + (1 - m) * (1 - prob1)
}

my <- function(m, z, a, w) {
    plogis(-log(5) + log(8) * z + log(10) * m -
               log(1.2) * w[, "W1"] + log(1.2) * w[, "W1"] * z)
}

gendata <- function(N) {
    w1 <- rbinom(N, 1, .4)
    w <- data.frame(W1 = w1)
    a <- rbinom(N, 1, g(1))
    z <- rbinom(N, 1, pZ(1, a, w))
    m <- rbinom(N, 1, pM(1, z, a, w))
    y <- rbinom(N, 1, my(m, z, a, w))
    data.frame(W1 = w1, A = a, Z = z, M = m, Y = y)
}

# Create DGMs where there is no effect through each of the paths and
# then checking that certain components are 0

tmp <- gendata(1000)

# library(recantingtwins)

params <- .recanting_twins_control(.pmz_learners = c("nnet", "xgboost"),
                                   .pz_learners = c("nnet", "xgboost"),
                                   .pm1_learners = c("nnet", "xgboost"),
                                   .pm2_learners = c("nnet", "xgboost"))

test <- recanting_twins(tmp, "W1", "A", "Z", "M", "Y", "binomial", .control = params)
print.recantingtwins(test)

