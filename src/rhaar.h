
#ifndef _RHAAR_H_
#define _RHAAR_H_

#include <RcppArmadillo.h>

arma::mat qr_sign_cpp(const arma::mat &A);

arma::mat rhaar1(const int &n);

#endif // _RHAAR_H_
