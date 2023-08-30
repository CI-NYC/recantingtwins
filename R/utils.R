assign_value <- function(data, x, val) {
    out <- copy(data)
    out[, (x) := lapply(.SD, function(y) val), .SDcols = x][]
}
