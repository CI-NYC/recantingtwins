
<!-- README.md is generated from README.Rmd. Please edit that file -->

# recantingtwins

> Mediation Analysis With Intermediate Confounding Using Recanting Twins

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of recantingtwins from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CI-NYC/recantingtwins")
```

## Example

``` r
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

set.seed(636456)
tmp <- gendata(1000)
```

``` r
library(recantingtwins)
library(mlr3extralearners)

params <- .recanting_twins_control(.pmz_learners = c("nnet", "xgboost"),
                                   .pz_learners = c("nnet", "xgboost"),
                                   .pm1_learners = c("nnet", "xgboost"),
                                   .pm2_learners = c("nnet", "xgboost"))

recanting_twins(tmp, "W1", "A", "Z", "M", "Y", "binomial", .control = params)
#> ══ Results `recantings_twins()` ═══════════════════════════════════════════════════
#> 
#> ── Average Treatment Effect 
#>       Estimate: 0.2918
#>     Std. error: 0.0292
#>         95% CI: (0.2346, 0.3491)
#> 
#> ── Path: A -> Y 
#>       Estimate: 0.0181
#>     Std. error: 0.0262
#>         95% CI: (-0.0334, 0.0695)
#> 
#> ── Path: A -> Z -> Y 
#>       Estimate: 0.1892
#>     Std. error: 0.03
#>         95% CI: (0.1304, 0.248)
#> 
#> ── Path: A -> Z -> M -> Y 
#>       Estimate: 0.1189
#>     Std. error: 0.0223
#>         95% CI: (0.0752, 0.1626)
#> 
#> ── Path: A -> M -> Y 
#>       Estimate: -0.0232
#>     Std. error: 0.0188
#>         95% CI: (-0.06, 0.0136)
#> 
#> ── Intermediate Confounding 
#>       Estimate: -0.0111
#>     Std. error: 0.0086
#>         95% CI: (-0.028, 0.0058)
```
