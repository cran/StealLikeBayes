# test for reproducibility 
set.seed(1)
run_no1 <- rtmvnorm(mean = matrix(c(0, 0), nrow = 1), sigma = diag(2), 
                    blc = diag(2), lower = matrix(c(-Inf, -Inf), nrow = 1), upper = matrix(c(1, 1), nrow = 1), init = matrix(c(0, 0), nrow = 1), burn = 10)
set.seed(1)
run_no2 <- rtmvnorm(mean = matrix(c(0, 0), nrow = 1), sigma = diag(2), 
                    blc = diag(2), lower = matrix(c(-Inf, -Inf), nrow = 1), upper = matrix(c(1, 1), nrow = 1), init = matrix(c(0, 0), nrow = 1), burn = 10)
expect_identical(
  run_no1[1],
  run_no2[1],
  info = "rtmvnorm: the draws are not reproducible."
)
expect_true(
  is.matrix(run_no1) && nrow(run_no1)==1 && ncol(run_no1)==2,
  info = "rtmvnorm: the returned output is not a 1x2 matrix."
)

# test validation
expect_error(
  rtmvnorm(mean = c(0, 0), sigma = diag(2), 
           blc = diag(2), lower = matrix(c(-Inf, -Inf), nrow = 1), upper = matrix(c(1, 1), nrow = 1), init = matrix(c(0, 0), nrow = 1), burn = 10),
  info = "rtmvnorm: incorrect first argument."
)

# test bad argument
expect_error(
  rtmvnorm(mean = matrix(c(0, 0), nrow = 1), sigma = -diag(2), 
           blc = diag(2), lower = matrix(c(-Inf, -Inf), nrow = 1), upper = matrix(c(1, 1), nrow = 1), init = matrix(c(0, 0), nrow = 1), burn = 10),
  info = "rtmvnorm: incorrect second argument."
)

