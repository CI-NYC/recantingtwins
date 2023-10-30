# No effect of M on Y

library(foreach)

Ey <- function(m, z, a) {
    gamma_2 <- 0.5
    gamma_3 <- 0.4
    plogis(0.5*m + gamma_2*z + gamma_3*a)
}

Ey2 <- function(a) {
    # m = 1, z = 1
    Ey(1, 1, a)*prob_M_zax(1, 1, a)*prob_Z_ax(1, a) +
        # m = 0, z = 1
        Ey(0, 1, a)*prob_M_zax(0, 1, a)*prob_Z_ax(1, a) +
        # m = 1, z = 0
        Ey(1, 0, a)*prob_M_zax(1, 0, a)*prob_Z_ax(0, a) +
        # m = 0, z = 0
        Ey(0, 0, a)*prob_M_zax(0, 0, a)*prob_Z_ax(0, a)
}

prob_Z_ax <- function(z, a) {
    prob <- plogis(0.3 + 0.7*a)
    z*prob + (1 - z)*(1 - prob)
}

prob_M_zax <- function(m, z, a) {
    prob <- plogis(0.3 + 0*z + 0.7*a)
    m*prob + (1 - m)*(1 - prob)
}

prob_M_ax <- function(m, a) {
    prob_M_zax(m, 1, a)*prob_Z_ax(1, a) +
        prob_M_zax(m, 0, a)*prob_Z_ax(0, a)
}

prob_ZM_ax <- function(z, m, a) {
    prob_Z_ax(z, a)*prob_M_zax(m, z, a)
}

sim_data = function(n = 500) {
    X = rbinom(n, 1, 0.5)
    A = rbinom(n, 1, 0.5)
    Z = rbinom(n, 1, prob_Z_ax(1, A))
    M = rbinom(n, 1, prob_M_zax(1, Z, A))
    Y = rbinom(n, 1, Ey(M, Z, A))
    data.frame(X = X, A = A, Z = Z, M = M, y = Y)
}

`E(Y_s4|W)` <- Ey2(0)
`E(Y_s0|W)` <- Ey2(1)

ap <- 1
as <- 0
vals <- expand.grid(z = c(1, 0), m = c(1, 0))

`E(Y_s1|W)` <- foreach(z = vals$z, m = vals$m, .combine = "sum") %do% {
    Ey(m, z, as)*prob_ZM_ax(z, m, ap)
}

`E(Y'_s1|W)` <- foreach(z = vals$z, m = vals$m, .combine = "sum") %do% {
    Ey(m, z, as)*prob_Z_ax(z, ap)*prob_M_ax(m, ap)
}

`E(Y'_s2|W)` <- foreach(z = vals$z, m = vals$m, .combine = "sum") %do% {
    Ey(m, z, as)*prob_Z_ax(z, as)*prob_M_ax(m, ap)
}

`E(Y''_s2|W)` <- `E(Y'_s2|W)`

`E(Y_s3|W)` <- foreach(z = vals$z, m = vals$m, .combine = "sum") %do% {
    Ey(m, z, as)*prob_Z_ax(z, as)*prob_M_zax(m, ap, z)
}

vals <- expand.grid(z = c(1, 0), zp = c(1, 0), m = c(1, 0))

`E(Y''_s3|W)` <- foreach(z = vals$z, m = vals$m, zp = vals$zp, .combine = "sum") %do% {
    Ey(m, z, as)*prob_Z_ax(z, as)*prob_M_zax(m, ap, zp)*prob_Z_ax(zp, as)
}

p1 <- `E(Y_s0|W)` - `E(Y_s1|W)`
p2 <- `E(Y'_s1|W)` - `E(Y'_s2|W)`
p3 <- `E(Y''_s2|W)` - `E(Y''_s3|W)`
p4 <- `E(Y_s3|W)` - `E(Y_s4|W)`
ate <- `E(Y_s0|W)` - `E(Y_s4|W)`

p1 + p2 + p3 + p4 +
    (`E(Y_s1|W)` - `E(Y'_s1|W)` + `E(Y'_s2|W)` - `E(Y''_s2|W)` +
         `E(Y''_s3|W)` - `E(Y_s3|W)`)

library(devtools)
library(mlr3extralearners)

load_all()
attr_models <- c("glm", "nnet", "lightgbm", "ranger")

params <- .recanting_twins_control(
    .g_learners = attr_models,
    .m_learners = attr_models,
    .pmz_learners = attr_models,
    .pz_learners = attr_models,
    .pm1_learners = attr_models,
    .pm2_learners = attr_models
)

data = sim_data(1000)

res = recanting_twins(data, "X", "A", "Z", "M", "y", "binomial", .control = params)
print(res)
