rt_control <- function() {
    list(.g_learners = c("mean", "glm", "earth"),
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
}
