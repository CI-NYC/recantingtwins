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
    params <- list(.g_learners = c("mean", "glm", "earth"),
                   .g_folds = 10,
                   .m_learners = c("mean", "glm", "earth"),
                   .m_folds = 10,
                   .pmz_learners = c("mean", "glm", "earth"),
                   .pmz_folds = 10,
                   .pz_learners = c("mean", "glm", "earth"),
                   .pz_folds = 10,
                   .pm1_learners = c("mean", "glm", "earth"),
                   .pm1_folds = 10,
                   .pm2_learners = c("mean", "glm", "earth"),
                   .pm2_folds = 10)

    if (length(change) == 0) return(params)
    change <- change[names(change) %in% names(params)]
    params[names(change)] <- change
    params
}
