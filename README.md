
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
devtools::install_github("CI-NYC/recantingtwins@categoricalZM2")
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

tmp = data.frame(apply(tmp, 2, as.numeric))
recanting_twins(tmp, "W1", "A", "Z", "M", "Y", "binomial")
#> ══ Results `recantings_twins()` ═══════════════════════════════════════════════════
#> 
#> ── Average Treatment Effect 
#>       Estimate: 0.2919
#>     Std. error: 0.0292
#>         95% CI: (0.2346, 0.3491)
#> 
#> ── Path: A -> Y 
#>       Estimate: 0.0149
#>     Std. error: 0.0258
#>         95% CI: (-0.0356, 0.0655)
#> 
#> ── Path: A -> Z -> Y 
#>       Estimate: 0.1823
#>     Std. error: 0.0257
#>         95% CI: (0.132, 0.2327)
#> 
#> ── Path: A -> Z -> M -> Y 
#>       Estimate: 0.1213
#>     Std. error: 0.0178
#>         95% CI: (0.0864, 0.1561)
#> 
#> ── Path: A -> M -> Y 
#>       Estimate: -0.0153
#>     Std. error: 0.0156
#>         95% CI: (-0.0458, 0.0152)
#> 
#> ── Intermediate Confounding 
#>       Estimate: -0.0114
#>     Std. error: 0.0049
#>         95% CI: (-0.0209, -0.0018)
```
