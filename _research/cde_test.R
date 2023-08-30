library(mlr3superlearner)

n <- 1000

w <- runif(n)
x <- rnorm(n, 0.5 + w)
dx <- dnorm(x, 0.5 + w)

dat <- data.frame(w = w, x = x)
delta <- rep(c(0, 1), each = n)

minx <- min(x)
maxx <- max(x)
R <- runif(n, minx, maxx)
# dR <- dunif(R, minx, maxx)

dat2 <- rbind(dat, dat)
dat2$delta <- delta
dat2$x <- ifelse(delta == 0, R, x)

fit <- mlr3superlearner(dat2, "delta", c("nnet", "earth", "glm"),
                        outcome_type = "binomial",
                        folds = 3)

dx_est <- (predict(fit, dat) / (1 - predict(fit, dat))) * dunif(x, minx, maxx)

plot(dx, dx_est)
abline(0, 1)

head((predict(fit, dat) / (1 - predict(fit, dat))))
head(dx / dunif(x, minx, maxx))
