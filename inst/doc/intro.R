## ----eval=FALSE---------------------------------------------------------------
#  Paretomle <- function(x,u){
#    N <- length(x)
#    beta <- 0
#    for (i in 1:N){
#      beta <- beta + log(x[i]/u)
#    }
#    betahat <- N/beta
#    supL0 <- -N*(1+log(u)+(1/betahat)-log(betahat))
#    return(supL0)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  SEmle <- function(x,u){
#    N <- length(x)
#    k1 <- k2 <- b <- b1 <- 0
#    for (i in 1:N){
#      b1 <- b1 + log(x[i]/u)
#      k1 <- k1 + (log(x[i]/u))^2
#      k2 <- k2 + (log(x[i]/u))^3
#    }
#    chat <- (k1/2 - b1^2)/(b1*k1/2 - k2/3)
#    for (j in 1:N){
#      b <- b + (x[i]/u)^chat
#    }
#    bhat <- chat * (N/(b-N))
#    supL1 <- -N*(1+log(u)-(chat-1)*b1/N-log(bhat))
#    return(supL1)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  function(x,u,alpha){
#    N <- length(x)
#    k1 <- k2 <- b1 <- 0
#    for (i in 1:N){
#      b1 <- b1 + log(x[i]/u)
#      k1 <- k1 + (log(x[i]/u))^2
#      k2 <- k2 + (log(x[i]/u))^3
#    }
#    chat <- (k1/2 - b1^2)/(b1*k1/2 - k2/3)
#    wsta <- 2 * N * (chat)^2 * (k2/(6*b1) - k1/2 + (k1/b1)^2/8)
#    ifelse(wsta > qchisq(1-alpha,1),"reject the null hypothesis","do not reject the null hypothesis")
#  
#  }

## ----eval=FALSE---------------------------------------------------------------
#  double wilks(
#      NumericVector x, int u) {
#    double k1 = 0.0,k2 = 0.0,b1 = 0.0,chat = 0.0,wsta = 0.0;
#    int n = x.size();
#    for(int i = 0; i < n; ++i) {
#      b1 += log(x[i] / u );
#      k1 += pow(log(x[i] / u),2);
#      k2 += pow(log(x[i] / u),3);
#    }
#    chat = -(k1/2-pow(b1,2))/(b1*k1/2-k2/3);
#    wsta = 2 * n * pow(chat,2) * (k2/(6*b1) - k1/2 + pow(k1/b1,2)/8);
#    return wsta;
#  }

## ----eval=TRUE----------------------------------------------------------------
library(SA23204187)
library(microbenchmark)
tm1 <- microbenchmark::microbenchmark(
   rnR = hytest(c(10,12,23,2,4,5,6,5,4,7,8,6),1,0.05),
   rnC = wilks(c(10,12,23,2,4,5,6,5,4,7,8,6),1)
)
print(summary(tm1)[,c(1,3,5,6)])

