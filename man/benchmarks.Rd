% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SA23204187R.R
\name{benchmarks}
\alias{benchmarks}
\title{Benchmark R and Rcpp functions.}
\description{
Use R package \code{microbenchmark} to compare the performance of C functions (\code{gibbsR} and \code{vaccR}) and Cpp functions (\code{gibbsC} and \code{vaccC}).
}
\examples{
\dontrun{
tm1 <- microbenchmark::microbenchmark(
  rnR = hytest(c(10,12,23,2,4,5,6,5,4,7,8,6),1,0.05),
  rnC = wilks(c(10,12,23,2,4,5,6,5,4,7,8,6),1)
)
print(summary(tm1)[,c(1,3,5,6)])
}
}
