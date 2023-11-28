#' @title the mle of Pareto distribution
#' @description the mle of Pareto distribution
#' @param x the vector of samples
#' @param u the threshold of domain
#' @return the mle of Pareto distribution \code{n}
#' @examples
#' \dontrun{
#'   pmle <- Paretomle(c(2,3,4,5),1)
#'   pmle
#' }
#' @export
Paretomle <- function(x,u){
  N <- length(x)
  beta <- 0
  for (i in 1:N){
    beta <- beta + log(x[i]/u)
  }
  betahat <- N/beta
  supL0 <- -N*(1+log(u)+(1/betahat)-log(betahat))
  return(supL0)
}

#' @title the approximate mle of Stretched-Exponential distribution
#' @description the approximate mle of Stretched-Exponential distribution
#' @param x the vector of samples
#' @param u the threshold of domain
#' @return the approximate mle of SE distribution \code{n}
#' @examples
#' \dontrun{
#'   semle <- SEmle(c(2,3,4,5),1)
#'   semle
#' }
#' @export
SEmle <- function(x,u){
  N <- length(x)
  k1 <- k2 <- b <- b1 <- 0
  for (i in 1:N){
    b1 <- b1 + log(x[i]/u)
    k1 <- k1 + (log(x[i]/u))^2
    k2 <- k2 + (log(x[i]/u))^3
  }
  chat <- (k1/2 - b1^2)/(b1*k1/2 - k2/3) 
  for (j in 1:N){
    b <- b + (x[i]/u)^chat
  }
  bhat <- chat * (N/(b-N))
  supL1 <- -N*(1+log(u)-(chat-1)*b1/N-log(bhat))
  return(supL1)
}

#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description Use R package \code{microbenchmark} to compare the performance of C functions (\code{gibbsR} and \code{vaccR}) and Cpp functions (\code{gibbsC} and \code{vaccC}).
#' @examples
#' \dontrun{
#' tm1 <- microbenchmark::microbenchmark(
#'   rnR = hytest(c(10,12,23,2,4,5,6,5,4,7,8,6),1,0.05),
#'   rnC = wilks(c(10,12,23,2,4,5,6,5,4,7,8,6),1)
#' )
#' print(summary(tm1)[,c(1,3,5,6)])
#' }
#' @import microbenchmark
#' @importFrom Rcpp evalCpp
#' @importFrom stats qchisq
#' @useDynLib SA23204187
NULL

#' @title Testing Pareto versus Stretched-Exponential by wilks statistics
#' @description Testing Pareto versus Stretched-Exponential by wilks statistics
#' @param x the vector of samples
#' @param u the threshold of domain
#' @param alpha the significant level
#' @return the result of the hypothesis \code{n}
#' @examples
#' \dontrun{
#'   wtest <- hytest(c(2,3,4,5),1,0.05)
#'   wtest
#' }
#' @export
hytest <- function(x,u,alpha){
  N <- length(x)
  k1 <- k2 <- b1 <- 0
  for (i in 1:N){
    b1 <- b1 + log(x[i]/u)
    k1 <- k1 + (log(x[i]/u))^2
    k2 <- k2 + (log(x[i]/u))^3
  }
  chat <- (k1/2 - b1^2)/(b1*k1/2 - k2/3) 
  wsta <- 2 * N * (chat)^2 * (k2/(6*b1) - k1/2 + (k1/b1)^2/8)
  ifelse(wsta > qchisq(1-alpha,1),"reject the null hypothesis","do not reject the null hypothesis")
  
}

