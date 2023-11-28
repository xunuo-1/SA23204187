#include <Rcpp.h>
using namespace Rcpp;

//' @title The wilks statistics of Pareto and Stretched-Exponential 
//' @description The wilks statistics of Pareto and Stretched-Exponential 
//' @param x the vector of samples
//' @param u the threshold of domain
//' @return the result of the hypothesis \code{n}
//' @examples
//' \dontrun{
//' wilkstat <- wilks(c(2,3,4,5),1)
//' wilkstat
//' }
//' @export
// [[Rcpp::export]]
double wilks(
    NumericVector x, int u) {
  double k1 = 0.0,k2 = 0.0,b1 = 0.0,chat = 0.0,wsta = 0.0;
  int n = x.size();
  for(int i = 0; i < n; ++i) {
    b1 += log(x[i] / u );
    k1 += pow(log(x[i] / u),2);
    k2 += pow(log(x[i] / u),3);
  }
  chat = -(k1/2-pow(b1,2))/(b1*k1/2-k2/3);
  wsta = 2 * n * pow(chat,2) * (k2/(6*b1) - k1/2 + pow(k1/b1,2)/8);
  return wsta;
}


