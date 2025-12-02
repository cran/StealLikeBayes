# Replication check
set.seed(123)
res1 <- rtmvnorm_hmc(n = 1, mean = c(0, 0), cov = diag(2), initial = c(1, 1), Fmat = diag(2), g = c(0, 0), burn = 10)

set.seed(123)
res2 <- rtmvnorm_hmc(n = 1, mean = c(0, 0), cov = diag(2), initial = c(1, 1), Fmat = diag(2), g = c(0, 0), burn = 10)

expect_equal(res1, res2, info = "rtmvnorm_hmc: Replication with same seed")

# Error handling checks
# Covariance must be square
expect_error(rtmvnorm_hmc(n = 1, mean = c(0, 0), cov = matrix(1:6, 2, 3), initial = c(1, 1), Fmat = diag(2), g = c(0, 0)),
    info = "rtmvnorm_hmc: Non-square covariance"
)

# Dimensions mismatch (mean vs cov)
expect_error(rtmvnorm_hmc(n = 1, mean = c(0, 0, 0), cov = diag(2), initial = c(1, 1), Fmat = diag(2), g = c(0, 0)),
    info = "rtmvnorm_hmc: Dimension mismatch: mean vs cov"
)

# Burn-in negative
expect_error(rtmvnorm_hmc(n = 1, mean = c(0, 0), cov = diag(2), initial = c(1, 1), Fmat = diag(2), g = c(0, 0), burn = -1),
    info = "rtmvnorm_hmc: Negative burn-in"
)
