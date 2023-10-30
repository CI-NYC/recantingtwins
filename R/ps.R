ps_disc <- function(data, W, A, .control) {
    out <- matrix(nrow = nrow(data), ncol = 2)
    colnames(out) <- c("P(0|W)", "P(1|W)")
    fit <- mlr3superlearner(data = data[, c(A, W), with = FALSE],
                            target = A,
                            library = .control$.g_learners,
                            outcome_type = "binomial",
                            folds = .control$.g_folds)
    
    out[, "P(1|W)"] <- predict(fit, data[,c(W), with = F], discrete = F)
    out[, "P(0|W)"] <- 1 - out[, "P(1|W)"]
    
    #browser()
    
    list(pred = out,
        fit = fit)
}