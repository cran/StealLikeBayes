#' @title Sample Random Draws From the Truncated Multivariate Normal Using the Algorithm Proposed by Yifang Li and Sujit K. Ghosh (2015)
#' 
#' @description Samples random numbers from a truncated multivariate normal distribution
#' with parameters mean vector, covariance matrix, and linear inequality constraints 
#' of the form \eqn{l \leq Bx \leq u}, where \eqn{B} is a constraint matrix and 
#' \eqn{l} and \eqn{u} are lower and upper bounds. The function uses a Gibbs sampling 
#' algorithm to generate draws from the constrained distribution.
#' 
#' The truncated multivariate normal is important for research in Bayesian statistics, 
#' econometrics, and any field requiring parameter estimation subject to inequality 
#' constraints. Common applications include censored regression models, portfolio 
#' optimization with constraints, and prior distributions with bounded support.
#' 
#' @details This function is based on C++ code from the R package \pkg{tmvtnsim}
#' by Lu (2025) and is using 
#' objects and commands from the \pkg{armadillo} library by Sanderson & Curtin (2025)
#' thanks to the \pkg{RcppArmadillo} package by Eddelbuettel, Francois, Bates, 
#' Ni, & Sanderson (2025).
#' 
#' @param mean an \eqn{n\times p} matrix of means. n is the number of draws to be sampled. p is the dimension of the draws.
#' \strong{C++}: an \code{arma::mat} object.
#' @param sigma a \eqn{p\times p} covariance matrix for the draws. 
#' \strong{C++}: an \code{arma::mat} object.
#' @param blc an \eqn{m\times p} matrix of coefficients for linear inequality constraints.
#' \strong{C++}: an \code{arma::mat} object.
#' @param lower an \eqn{n\times m} matrix of lower truncation bounds. 
#' \strong{C++}: an \code{arma::mat} object.
#' @param upper an \eqn{n\times m} matrix of upper truncation bounds. 
#' \strong{C++}: an \code{arma::mat} object.
#' @param init an \eqn{n\times p} matrix of initial values for the algorithm.
#' \strong{C++}: an \code{arma::mat} object.
#' @param burn number of iterations used as burn-in. Defaults is 10.
#' \strong{C++}: an \code{arma::uword} object.
#' 
#' @return An \eqn{n\times p} matrix of draws from the specified truncated multivariate normal. \strong{C++}: an \code{arma::mat} object.
#' 
#' @author Filip Reierson \email{filip.reierson@gmail.com}
#' 
#' @references 
#' 
#' Eddelbuettel D., Francois R., Bates D., Ni B., Sanderson C. (2025). 
#' RcppArmadillo: 'Rcpp' Integration for the 'Armadillo' Templated Linear 
#' Algebra Library. R package version 15.0.2-2. <doi:10.32614/CRAN.package.RcppArmadillo>
#' 
#' Sanderson C., Curtin R. (2025). Armadillo: An Efficient Framework for 
#' Numerical Linear Algebra. International Conference on Computer and Automation 
#' Engineering, 303-307, <doi:10.1109/ICCAE64891.2025.10980539>
#' 
#' Li, Y., Ghosh, S.K. Efficient sampling methods for truncated multivariate 
#' normal and student-t distributions subject to linear inequality constraints. 
#' J Stat Theory Pract 9, 712–732 (2015). <doi:10.1080/15598608.2014.996690>
#' 
#' Lu K. (2025). tmvtnsim: Truncated Multivariate Normal and t Distribution
#' Simulation. R package version 0.1.4, <doi:10.32614/CRAN.package.tmvtnsim>
#' 
#' @examples
#' rtmvnorm(mean = matrix(c(0, 0), nrow = 1), sigma = diag(2), 
#'          blc = diag(2), lower = matrix(c(-Inf, -Inf), nrow = 1), 
#'          upper = matrix(c(1, 1), nrow = 1), init = matrix(c(0, 0), 
#'          nrow = 1), burn = 10)
#' 
#' @export
rtmvnorm <- function(mean, sigma, blc, lower, upper, init, burn=10) {
  stopifnot(
    "The argument mean must be a matrix" =
      is.matrix(mean)
  )
  n <- nrow(mean)
  p <- ncol(mean)
  stopifnot(
    "The argument sigma must be a p by p matrix" =
      is.matrix(sigma) && nrow(sigma)==p && ncol(sigma)==p
  )
  stopifnot(
    "The argument blc must be an m by p matrix" =
      is.matrix(blc) && ncol(blc)==p
  )
  m <- nrow(blc)
  stopifnot(
    "The argument lower must be an n by m matrix"=
      is.matrix(lower) && nrow(lower)==n && ncol(lower) == m
  )
  stopifnot(
    "The argument upper must be an n by m matrix"=
      is.matrix(upper) && nrow(upper)==n && ncol(upper) == m
  )
  stopifnot(
    "The argument init must be an n by p matrix"=
      is.matrix(init) && nrow(init)==n && ncol(init) == p
  )
  stopifnot(
    "The argument burn must be a scalar"=
      is.numeric(burn) && length(burn)==1
  )
  out = .Call(`_StealLikeBayes_rtmvnorm`, mean, sigma, blc, lower, upper, init, burn)
  return(out)
}