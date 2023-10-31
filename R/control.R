#' Change Recanting Twins Estimator Hyperparameters
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
.recanting_twins_control <- function(...) {
    change <- list(...)
    params <- list(.g_learners = list("mean", list("nnet", trace = F), "ranger", "lightgbm"),
                   .g_folds = NULL,
                   .m_learners = list("mean", list("nnet", trace = F), "ranger", "lightgbm"),
                   .m_folds = NULL,
                   .pmz_learners = list("mean", list("nnet", trace = F), "ranger", "lightgbm"),
                   .pmz_folds = NULL,
                   .pz_learners = list("mean", list("nnet", trace = F), "ranger", "lightgbm"),
                   .pz_folds = NULL,
                   .pm1_learners = list("mean", list("nnet", trace = F), "ranger", "lightgbm"),
                   .pm1_folds = NULL,
                   .pm2_learners = list("mean", list("nnet", trace = F), "ranger", "lightgbm"),
                   .pm2_folds = NULL)

    if (length(change) == 0) return(params)
    change <- change[names(change) %in% names(params)]
    params[names(change)] <- change
    params
}
