rt_control <- function() {
    list(.g_learners = c("mean", "glm"),
         .g_folds = 10,
         .m_learners = c("mean", "glm"),
         .m_folds = 10,
         .pmz_learners = c("mean", "glm"),
         .pmz_folds = 10)
}
