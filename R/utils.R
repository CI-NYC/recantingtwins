assign_value <- function(data, x, val) {
    out <- copy(data)
    out[, (x) := lapply(.SD, function(y) val), .SDcols = x][]
}

H_factory <- function(x) {
    apply(x, 2, function(j) {
        is_continuous <- any(schoolmath::is.decimal(j))
        if (is_continuous) {
            return(H_factory_continuous(j))
        }
        H_factory_discrete(j)
    }, simplify = FALSE)
}

H_factory_continuous <- function(x) {
    minx <- min(x)
    maxx <- max(x)
    list(rh = function(n) runif(n, minx, maxx),
         dh = function(x) dunif(x, minx, maxx))
}

H_factory_discrete <- function(x) {
    vals <- unique(x)
    list(rh = function(n) sample(vals, size = n, replace = TRUE),
         dh = function(x) 1 / length(vals))
}

draw_H <- function(hs, p) {
    .f <- function(h) {
        draws <- h$rh(p)
        list(draws = draws,
             px = h$dh(draws))
    }

    lapply(hs, .f)
}

gl <- glue::glue
