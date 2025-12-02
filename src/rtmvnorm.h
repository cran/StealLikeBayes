#ifndef RTMVNORM
#define RTMVNORM

#include <RcppArmadillo.h>

double norm_rej(const double a, const double b);

double unif_rej(const double a, const double b);

double halfnorm_rej(const double a, const double b);

double exp_rej(const double a, const double b);

arma::vec rtnormcpp(
    const arma::vec& mean, 
    const double sd, 
    const arma::vec& lower, 
    const arma::vec& upper
);

arma::mat rtmvnorm(const arma::mat& mean, 
                    const arma::mat& sigma, 
                    const arma::mat& blc,
                    const arma::mat& lower, 
                    const arma::mat& upper,
                    const arma::mat& init,
                    const arma::uword burn = 10);
  

#endif
