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
                   .g_folds = NULL,
                   .m_learners = c("mean", "glm", "earth"),
                   .m_folds = NULL,
                   .pmz_learners = c("mean", "glm", "earth"),
                   .pmz_folds = NULL,
                   .pz_learners = c("mean", "glm", "earth"),
                   .pz_folds = NULL,
                   .pm1_learners = c("mean", "glm", "earth"),
                   .pm1_folds = NULL,
                   .pm2_learners = c("mean", "glm", "earth"),
                   .pm2_folds = NULL)

    if (length(change) == 0) return(params)
    change <- change[names(change) %in% names(params)]
    params[names(change)] <- change
    params
}
