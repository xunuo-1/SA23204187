## -----------------------------------------------------------------------------
library(ggplot2)
library(bootstrap)
library(boot)
library(R2OpenBUGS)
library(DAAG)
library(Rcpp)

## ----eval=FALSE---------------------------------------------------------------
#  
#  #Number of male and female students in the master's class
#  x = c(13,32)
#  #Enter labels
#  ns = c("women","men")
#  #Enter colors
#  cs = c("blue","lightgray")
#  #Draw a 3D pie chart
#  pie3D(x,labels = ns,explode = 0.1,col = cs,main = "examples 1.1")
#  #Draw a flat pie chart
#  pie(x,labels = ns,col = cs,main = "examples 1.2")

## ----warning=FALSE------------------------------------------------------------
x <- 1:10
y <- x^3
lmr <- lm(y~x)
co <- summary(lmr)$coefficients
title <- expression(y == x^3)
result <- knitr::kable(co,digits = 2,align = 'c')
plot(x,y)
abline(lmr,col="red")
result

## ----warning=FALSE------------------------------------------------------------

dat <- c(10,22,30,40,55,56,80,94)
fun_table <- function(x){x <- sort(x) 
n <- length(x) 
tab <- unname(c(table(x)))
pct = tab / n 
d <- data.frame(x = sort(unique(x)),freq = tab,pct = pct,cumfreq = cumsum(tab),cumpct = cumsum(pct)) 
}
knitr::kable(fun_table(dat),align = 'c')
d <- data.frame(dat)
datafre <- fun_table(d$dat)
pic <- ggplot(data = datafre,mapping = aes(x=dat,y=cumpct))
pic +stat_ecdf(geom = "step") +
scale_y_continuous( 
limits = c(-.01, 1.01), 
expand = c(0, 0), 
name = " cumulative percent") 

## ----warning=FALSE------------------------------------------------------------
#Take the same parameters as the sample function
newsample <- function(x,size,replace = FALSE,prob = NULL){
  if (replace == TRUE){         #sample with replacement
    A <- runif(size,1,length(x))#sampling from uniform distribution
    B <- round(A)               #round random numbers to  indexs
    y <- x[B]
  }
  else if (size <= length(x) & replace == FALSE){  #sample without replacement 
    n <- size;
    k <- 0;                     #sampled numbers
    y <- c(1:size);             #The while loop replaces elements in y with elements in x
    Results <- 1:size
    while (k<n){
      D <- runif(1,1,length(x))
      E <- round(D)
      #removes the taken element from x
      if (E %in% Results){
        k <- k + 1
        Results <- Results[-which(Results==E)]
        y[k] <- x[E] 
      }
    }
  }
  else {              #Except for the above two methods, the rest will report errors
    y <- "input error"
  }
  return(y)
}
#Compare the two functions sample and newsample
newsample(c(1,3,2,4,5),5) #Sampling without replacement
newsample(1:2,5,replace = TRUE) #Sampling with replacement
sample(c(1,3,2,4,5),5)
sample(1:2,5,replace=TRUE)

## ----warning=FALSE------------------------------------------------------------
set.seed(1234)
n <- 1000
result <- vector()   #generate numbers
u <- runif(n)
k <- 0
#Divide the generated u into two situations
while (k<n){
   k <- k+1
   if (0<u[k] & u[k]<=0.5){
     result[k] <- log(2*u[k]) 
   }
   else {
     result[k] <- -log(2*(1-u[k]))
   }
}
hist(result,prob = TRUE)
y <- seq(-5,5,0.1)
z <- exp(-abs(y))/2
lines(y,z)

## ----warning=FALSE------------------------------------------------------------

# creat the function
betasample <- function(n,a,b){
  samplenum <- n  
  k <- 0
  result <- vector()
  while (k < samplenum){
    u <- runif(1) #define U
    x <- runif(1) #define g(x)
    if (x^(a-1)*(1-x)^(b-1) > u){ #define c = gamma(a+b)/(gamma(a)+gamma(b))
      k <- k + 1
      result[k] <- x
    }
  }
  return(result)
}

#generate 1000 samples from the Beta distribution and draw the histogram with theoretical Beta(3,2) density
n <- 1000
data <- betasample(n,3,2)
hist(data,prob = TRUE)
x <- seq(0,1,0.1)
curve(dbeta(x,3,2),add = TRUE)

## ----warning=FALSE------------------------------------------------------------
Ekernel <- function(n){
  u1 <- runif(n,-1,1)
  u2 <- runif(n,-1,1)
  u3 <- runif(n,-1,1)
  result <- vector()
  k <- 0
  while (k < n){
    k <- k + 1
    if (abs(u3[k])>=abs(u2[k]) & abs(u3[k])>=abs(u1[k])){
      result[k] <- u2[k]
      
    }
    else {
      result[k] <- u3[k]
    }
  }
  return(result)
}
x <- Ekernel(1000)
hist(x,prob = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  ![Homework ](3.10.jpg)

## ----warning=FALSE------------------------------------------------------------
niddle.var <- function(l){
  d <- 1
  m <- 1e6
  x <- runif(m,0,d/2)
  y <- runif(m,0,pi/2)
  pihat <- 2 * l / d  /mean(l/2*sin(y)>x)
  pihat
}
values.min <- numeric(100)
values1 <- numeric(100)
values2 <- numeric(100)

for (i in 1:100){
  values.min[i] <- niddle.var(1)
  values1[i] <- niddle.var(0.5)
  values2[i] <- niddle.var(0.8)
}
data = matrix(c(var(values.min),var(values1),var(values2)),ncol=3)
colnames(data) = c("1","0.5","0.8")
rownames(data) = c("var")
as.table(data)

## ----warning=FALSE------------------------------------------------------------
#the result is
1-4*(10*exp(1)-5-3*exp(2))/(exp(2)-1)

## ----warning=FALSE------------------------------------------------------------
#simple mC
MC.SIMPLE <- function(n){
  x <- runif(n)
  theta.hat <- mean(exp(x))
  theta.hat
}

#Antithetic Variables
MC.ANTI <- function(m){
  u <- runif(m/2)
  v <- 1-u
  rvalues <- (exp(u) + exp(1-u))/2
  theta.hat <- mean(rvalues) 
  theta.hat
}
#compute the variance reduction
m <- 1000
MC1 <- numeric(m)
MC2 <- numeric(m)
for (i in 1:m){
  MC1[i] <- MC.SIMPLE(1000)
  MC2[i] <- MC.ANTI(1000)
}
print((var(MC1)-var(MC2))/var(MC1))

## ----warning=FALSE------------------------------------------------------------
m <- 10000
theta.hat <- se <-numeric(2)
g <- function(x){
  (x^2)*exp(-x^2/2)/(sqrt(2*pi))*(x>1)
}
f1 <-function(x){
  exp(-x/2)/2
}
f2 <-function(x){
  (x^2)*exp(-x/2)/16
}
x <- rexp(m,0.5)  #using f1
y <- rgamma(m,shape = 3,rate = 0.5)
#By the graphs,we compare the f1 with f2.
x1 <- seq(1,10,length = 1000)
plot(x1,g(x1),col = "black")
lines(x1,f1(x1),col = "blue")
lines(x1,f2(x1),col = "yellow")
#the variance of f1 and f2
gf1 <- g(x)/f1(x)
gf2 <- g(y)/f2(y)
theta.hat[1] <- mean(gf1)
se[1] <- sd(gf1)
theta.hat[2] <- mean(gf2)
se[2] <- sd(gf2)
rbind(theta.hat,se)

## ----warning=FALSE------------------------------------------------------------
g <- function(x){
  (x^2)*exp(-x^2/2)/(sqrt(2*pi))*(x>1)
}
f1 <-function(x){
  exp(-x/2)/2
}
m <- 10000
x <- rexp(m,0.5)
gf <- g(x)/f1(x)
theta.hat <- mean(gf)
theta.hat  #estimator
integrate(g,1,Inf) #real values

## ----warning=FALSE------------------------------------------------------------
M <- 10000
k <- 5    #stratified numbers
estimate <- numeric(k) #estimates
variance <- numeric(k) 
g <- function(x){
  exp(-x - log(1+x^2)) * (x>0) * (x<1)
}
f <- function(x){
  exp(-x)/(1-exp(-1)) * (x>0) * (x<1)
}
for (i in 1:k){
  x <- runif(M/k,(i-1)/5,i/5)
  rate <- g(x)/f(x)
  variance[i] <- var(rate)
  estimate[i] <- mean(rate)
}
theta.hat <- mean(estimate)
sd <- sum(variance)/(M/k)
theta.hat
sd

## ----warning=FALSE------------------------------------------------------------
n <- 20
alpha <- 0.05
k <- 1000
# 6.5 confidence interval
calci <- function(n,alpha){
  confidence.int <- numeric(2)
  x <- rchisq(n,df = 2)
  theta.hat <- mean(x)
  d <- var(x)
  UCL <- theta.hat + qt(1-alpha/2,df = n-1)*sqrt(d/n) #upper confidence limit
  LCL <- theta.hat - qt(1-alpha/2,df = n-1)*sqrt(d/n) #lower confidence limit
  confidence.int[1] <- LCL
  confidence.int[2] <- UCL
  return(confidence.int)
}
counts <- numeric(k)
for (i in 1:k){
  cfint <- calci(n,alpha)
  if (cfint[1]<2 & cfint[2]>2){
    counts[i] <- 1
  }
}
# Example 6.4
results <- replicate(k,expr = {
  x <- rnorm(n,mean = 0,sd = 2)
  (n-1) * var(x)/qchisq(alpha,df = n-1)
})

mean(counts)
mean(results > 4)

## ----warning=FALSE------------------------------------------------------------

n <- 20
alpha <- 0.05
mu1 <- mu2 <- mu3 <-1

m <- 10000
p1<- p2 <- p3 <- numeric(m)
for (i in 1:m){
  x1 <- rchisq(n,df = 1)
  x2 <- runif(n,0,2)
  x3 <- rexp(n,1)
  x1test <- t.test(x1,alternative = "greater",mu = mu1) #(i)chi-square
  x2test <- t.test(x2,alternative = "greater",mu = mu2) #Uniform(0,2)
  x3test <- t.test(x3,alternative = "greater",mu = mu3) #exp(1)
  p1[i] <- x1test$p.value
  p2[i] <- x2test$p.value
  p3[i] <- x3test$p.value
}
p1.hat <- mean(p1 < alpha)
se1.hat<- sqrt(p1.hat * (1-p1.hat) / m)
p2.hat <- mean(p2 < alpha)
se2.hat<- sqrt(p2.hat * (1-p2.hat) / m)
p3.hat <- mean(p3 < alpha)
se3.hat<- sqrt(p3.hat * (1-p3.hat) / m)
print(c(p1.hat,se1.hat))
print(c(p2.hat,se2.hat))
print(c(p3.hat,se3.hat))

## ----warning=FALSE------------------------------------------------------------
m <- 1000
alpha <- 0.1
#generate p-value
fwer1 <- fdr1 <- tpr1 <- numeric(m)
fwer2 <- fdr2 <- tpr2 <- numeric(m)
orignalp <- function(m){
  p0 <- runif(m*0.95)
  p1 <- rbeta(m*0.05,0.1,1)
  return(c(p0,p1))
}
#compute FWER
fwer <- function(alpha,x){
  k <- 0
  for (i in length(x)){
    if (x[i]<alpha){
      k <- k + 1
    }
  } 
  1-(1-alpha/length(x))^(length(x)-k)
}
#compute FDR
fdr <- function(origvalues,adjust.val){
  FN <- 0
  for (i in 1:length(origvalues)){
    if (origvalues[i] < 0.1 & adjust.val[i] > 0.1){
      FN <- FN + 1
    }
  }
  FN / sum(adjust.val > 0.1)
}
#compute TPR
tpr <- function(origvalues,adjust.val){
  TP <- 0
  for (i in length(origvalues)){
    if (origvalues[i] < 0.1 & adjust.val[i] < 0.1){
      TP <- TP + 1
    }
  }
  TP / sum(origvalues < 0.1)
}
for (i in 1:m){
  pvalues <- orignalp(1000)
  #generate adjust-pvalues
  sortpvalues <- sort(pvalues)
  bonvalues <- p.adjust(pvalues,method = "bonferroni")
  bhvalues <- p.adjust(sortpvalues,method = "BH")
  fwer1[i] <- fwer(alpha,bonvalues)
  fwer2[i] <- fwer(alpha,bhvalues)
  fdr1[i] <- fdr(pvalues,bonvalues)
  fdr2[i] <- fdr(sortpvalues,bhvalues)
  tpr1[i] <- tpr(pvalues,bonvalues)
  tpr2[i] <- tpr(sortpvalues,bhvalues)
}
df <- data.frame(
  FWER = c(mean(fwer1),mean(fwer2)),
  FDR = c(mean(fdr1),mean(fdr2)),
  TPR = c(mean(tpr1),mean(tpr2)),
  row.names = c("Bonferroni","B-H"),
  stringsAsFactors=FALSE
)
print(df)

## ----warning=FALSE------------------------------------------------------------
# theoretical bias and se
theoretical <- function(n,lambda){
  bias <- lambda/(n-1)
  se <- lambda *n/((n-1)*sqrt(n-2))
  return(c(bias,se))
}
# bias and se by the bootstrap
boots <- function(x,n,lambda,B){
  thetastar <- numeric(B)
  for (i in 1:B){
    xstar <- sample(x,replace=TRUE)
    thetastar[i] <- 1/mean(xstar)
  }
  round(c(bias=mean(thetastar)-1/mean(x),se.boot = sd(thetastar)))
}
#repeat the sumulations for 1000 times
result <- function(n){
  bias <- se <- numeric(1000)
  lambda <- 2
  B <- 1000
  for (i in 1:1000){
    x <- rexp(n,lambda)
    bias[i] <- boots(x,n,lambda,B)[1]
    se[i] <- boots(x,n,lambda,B)[2]
  }
  c(mean(bias),mean(se))
}
bias <- data.frame(
  bootstrap = c(result(5)[1],result(10)[1],result(20)[1]),
  theor = c(theoretical(5,2)[1],theoretical(10,2)[1],theoretical(20,2)[1]),
  row.names = c(5,10,20)
)
se <- data.frame(
  bootstrap = c(result(5)[2],result(10)[2],result(20)[2]),
  theor = c(theoretical(5,2)[2],theoretical(10,2)[2],theoretical(20,2)[2]),
  row.names = c(5,10,20)
)
print(bias)
print(se)

## ----warning=FALSE------------------------------------------------------------

data(law,package = "bootstrap")
boot.t.ci <-
function(x, B = 500, R = 100, level = .95, statistic){
  #compute the bootstrap t CI
  x <- as.matrix(x); n <- nrow(x)
  stat <- numeric(B); se <- numeric(B)
  boot.se <- function(x, R, f) {
    #local function to compute the bootstrap
    #estimate of standard error for statistic f(x)
    x <- as.matrix(x); m <- nrow(x)
    th <- replicate(R, expr = {
      i <- sample(1:m, size = m, replace = TRUE)
      f(x[i, ])
      })
    return(sd(th))
  }
  for (b in 1:B) {
    j <- sample(1:n, size = n, replace = TRUE)
    y <- x[j, ]
    stat[b] <- statistic(y)
    se[b] <- boot.se(y, R = R, f = statistic)
  }
  stat0 <- statistic(x)
  t.stats <- (stat - stat0) / se
  se0 <- sd(stat)
  alpha <- 1 - level
  Qt <- quantile(t.stats, c(alpha/2, 1-alpha/2), type = 1)
  names(Qt) <- rev(names(Qt))
  CI <- rev(stat0 - Qt * se0)
}
stat <- function(x,i){cor(x[i,1],x[i,2])}
ci <- boot.t.ci(law,statistic = stat,B=2000,R=200)
print(ci)

## ----warning=FALSE------------------------------------------------------------


n <- 10;
m <- 1000
obser <- c(3,5,7,18,43,85,91,98,100,130,230,487)
set.seed(12345)
boot.mean <- function(x,i) mean(x[i])
ci.norm <- ci.basic <- ci.perc <- ci.bca <- matrix(NA,1,2)
de <- boot(data=obser,statistic = boot.mean,R=2000)
ci <- boot.ci(de,type = c("norm","basic","perc","bca"))
print(ci)
#compute the  mean lengths of the intervals
norm.len <- basic.len <- perc.len <- bca.len <- numeric(m)
for (i in 1:m){
  ci <- boot.ci(de,type = c("norm","basic","perc","bca"))
  norm.len[i] <- ci$norm[3]-ci$norm[2]
  basic.len[i] <- ci$basic[5]-ci$basic[4]
  perc.len[i] <- ci$perc[5]-ci$perc[4]
  bca.len[i] <- ci$bca[5]-ci$bca[4]
}
print(c(mean(norm.len),mean(basic.len),mean(perc.len),mean(bca.len)))


## ----warning=FALSE------------------------------------------------------------
attach(scor)
data <- as.matrix(scor)
n <- length(data[,1])
#the function of theta
scores.fun <- function(x,i){
  eigen(cov(x))$values[1]/sum(eigen(cov(x))$values)
}
theta.hat <- eigen(cov(data))$values[1]/sum(eigen(cov(data))$values)
theta.jack <- numeric(n)
for (i in 1:n){
  theta.jack[i] <- scores.fun(data[-i,])
}
bias.jack <- (n-1)*mean((theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(original=theta.hat,bias.jack=bias.jack,se.jack=se.jack),3)

## ----warning=FALSE------------------------------------------------------------

attach(ironslag)
set.seed(1234)
n <- length(magnetic)
e1 <- e2 <- e3 <-e4 <- matrix(NA,n,n-1)
# for n-fold cross validation
# fit models on leave-two-out samples
for (k in 1:n){
  y <- magnetic[-k]
  x <- chemical[-k]
  for (j in 1:(n-1)){
    z <- y[-j]
    w <- x[-j]
    
    J1 <- lm( z~ w)
    yhat1 <- J1$coef[1] + J1$coef[2] * x[j]
    e1[k,j] <- y[j] - yhat1

    L2 <- lm( z ~ w + I(w^2) )
    yhat2 <- L2$coef[1] + L2$coef[2] * x[j] +
             L2$coef[3] * x[j]^2
    e2[k,j] <- y[j] - yhat2

    L3 <- lm( log(z) ~ w )
    logyhat3 <- L3$coef[1] + L3$coef[2] * x[j]
    yhat3 <- exp( logyhat3 )
    e3[k,j] <- y[j] - yhat3

    L4 <- lm( log(z) ~ log(w) )
    logyhat4 <- L4$coef[1] + L4$coef[2] * log(x[j])
    yhat4 <- exp( logyhat4 )
    e4[k,j] <- y[j] - yhat4
  }
}
c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2))

## ----eval=FALSE---------------------------------------------------------------
#  
#  plot(L2$fit, L2$res)#residuals vs fitted values
#  abline(0, 0)        #reference line
#  qqnorm(L2$res)      #normal probability plot
#  qqline(L2$res)      #reference line
#  

## ----warning=FALSE------------------------------------------------------------
set.seed(123)
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
z <- c(x,y)
detach(chickwts)
r<-999
dos <- numeric(r)
#the cramer-von Mises Statistic
w2 <- function(n1,m1){
  sd1 <- numeric(n)
  sd2 <- numeric(m)
  n2 <- sort(n1)
  m2 <- sort(m1)
  for (j in 1:n){
      k1 <- 0
      for (k in 1:m){
        if (n2[j] <= m2[k]){
          k1 <- k1 + 1
        }
      }
      sd1[j] <- ((j/n)-(k1/m))^2
  }
  for (j1 in 1:m){
      k2 <- 0
      for (k3 in 1:n){
        if (m2[j1] <= n2[k3]){
          k2 <- k2 + 1
        }
      }
      sd2[j1] <- ((j1/m)-(k2/n))^2
    }
  m*n*(sum(sd1)+sum(sd2))/(m+n)^2  
}
n <- length(x)
m <- length(y)
l <- 1:length(z)
for (i in 1:r){
  sam <- sample(l,n,replace = FALSE)
  x1 <- z[sam]
  x2 <- z[-sam]
  dos[i] <- w2(x1,x2)
}
do <-w2(x,y)
p <- mean(c(do,dos) >= do)
p

## ----warning=FALSE------------------------------------------------------------
set.seed(1234)
c5t <- function(x,y){
  n <- length(x)
  m <- length(y)
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(c(outx,outy))
}
nu1 <- 20
nu2 <- 30
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
r <- 999
D <- matrix(NA,r,2)
x <- rnorm(nu1, mu1, sigma1)
y <- rnorm(nu2, mu2, sigma2)
N <- c(x,y)
l <- 1:length(N)
DO <- c5t(x,y)
for (i in 1:r){
  n1 <- sample(l,length(x),replace = FALSE)
  x1 <- N[n1]
  y1 <- N[-n1]
  pt <- c5t(x1,y1)
  D[i,1] <- pt[1]
  D[i,2] <- pt[2]
}
K1 <- 1
for (i in 1:r){
  if (D[i,1]>=DO[1] & D[i,2] >= DO[2]){
    K1 <- K1 + 1
  }
}
p <- K1/1000
p

## ----eval=FALSE---------------------------------------------------------------
#  
#  data(schools) # a real dataset
#  
#  N <- 1e4
#  J <- nrow(schools)
#  y <- schools$estimate
#  sigma.y <- schools$sd
#  data <- list ("N", "J", "y", "sigma.y")
#  
#  inits <- function(){
#    list(theta = rnorm(J, 0, 100), mu.theta = rnorm(1, 0, 100),
#      sigma.theta = runif(1, 0, 100))
#  }
#  
#  schools.sim <- bugs(data, inits, model.file = "schools.txt",
#                      parameters = c("theta", "mu.theta", "sigma.theta"),
#                      n.chains = 3, n.iter = N, codaPkg=TRUE)

## ----warning=FALSE------------------------------------------------------------
set.seed(12345)
ln <- function(N,b1,b2,b3,f0){
  x1 <- rpois(N,1)
  x2 <- rexp(N,1)
  x3 <- sample(0:1,N,replace = TRUE)
  p <- function(alpha){
  tmp <- exp(-alpha-b1*x1-b2*x2-b3*x3)
    pval <- 1/(1+tmp)
    mean(pval) - f0
  }
  solution <- uniroot(p,c(-20,0)) #let p value < 0.1
  solution$root
}
N <- 10e6
b1 <- 0;b2 <- 1;b3 <- -1
f0 <- 0.1
a1 <- ln(N,b1,b2,b3,f0)
a2 <- ln(N,b1,b2,b3,f0/10)
a3 <- ln(N,b1,b2,b3,f0/100)
a4 <- ln(N,b1,b2,b3,f0/1000)
x <- c(a1,a2,a3,a4)
y <- c(-log(f0),-log(f0/10),-log(f0/100),-log(f0/1000))
plot(x,y)

## ----warning=FALSE------------------------------------------------------------
set.seed(12345)
f <- function(x) (exp(-abs(x)))/2
set.seed(12345)
rw.Metropolis <- function(sigma, x0, N) {
    x <- numeric(N)
    x[1] <- x0
    u <- runif(N)
    k <- 0
    for (i in 2:N) {
      y <- rnorm(1, x[i-1], sigma)
        if (u[i] <= (exp(-abs(y))/exp(-abs(x[i-1]))))
        x[i] <- y  else {
            x[i] <- x[i-1]
            k <- k + 1
        }
      }
    return(list(x=x, k=N-k))
    }
N <- 2000
sigma <- c(0.05,1,4,16) #four different sigma

x0 <- 5
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)

#number of candidate points accepted
acceptance <- data.frame(sigma=sigma,acceptance=c(rw1$k, rw2$k, rw3$k, rw4$k))
knitr::kable(acceptance)

## ----warning=FALSE------------------------------------------------------------


rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
for (j in 1:4) {
    plot(rw[,j], type="l",
          xlab=bquote(sigma == .(round(sigma[j],3))),
          ylab="X", ylim=range(rw[,j]))
}

    

## ----warning=FALSE------------------------------------------------------------
#initialize constants and parameters
N <- 5000 #length of chain
burn <- 1000 #burn-in length
X <- matrix(0, N, 2) #the chain, a bivariate sample
rho <- 0.9 #correlation
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2
# generate the chain
X[1, ] <- c(mu1, mu2) #initialize
for (i in 2:N) {
  x2 <- X[i-1, 2]
  m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
  X[i, 1] <- rnorm(1, m1, s1)
  x1 <- X[i, 1]
  m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
  X[i, 2] <- rnorm(1, m2, s2)
}
b <- burn + 1
x <- X[b:N, ]
plot(x, main="", cex=.5, xlab=bquote(X[1]),ylab=bquote(X[2]), ylim=range(x[,2]))

## ----warning=FALSE------------------------------------------------------------
Y <- x[,2]
X <- x[,1]
dat <- data.frame(x)
lreg <- lm(Y~X,data=dat)
#normality test
res <- residuals(lreg)
shapiro.test(res) # p value > 0.05,we do not reject the null hypothesis
#plot(density(res))

plot(lreg)

## ----warning=FALSE------------------------------------------------------------
f <- function(x, sigma) {
  if (any(x < 0)) return (0)
  stopifnot(sigma > 0)
  return((x / sigma^2) * exp(-x^2 / (2*sigma^2)))
}
Gelman.Rubin <- function(psi) {
    # psi[i,j] is the statistic psi(X[i,1:j])
    # for chain in i-th row of X
    psi <- as.matrix(psi)
    n <- ncol(psi)
    k <- nrow(psi)

    psi.means <- rowMeans(psi)     #row means
    B <- n * var(psi.means)        #between variance est.
    psi.w <- apply(psi, 1, "var")  #within variances
    W <- mean(psi.w)               #within est.
    v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
    r.hat <- v.hat / W             #G-R statistic
    return(r.hat)
}

chisq.chain <- function(sigma, N){
    #with chisq(df=x) proposal distribution
    #and starting value X1
    x <- rep(0, N)
    x[1] <- rchisq(1,df=1)
    u <- runif(N)

    for (i in 2:N) {
      xt <- x[i-1]
      y <- rchisq(1,df = xt)     #candidate point
      r1 <- f(y,sigma) * dchisq(xt,df = y)
      r2 <- f(xt,sigma) * dchisq(y,df = xt)
      if (u[i] <= r1/r2) x[i] <- y else
        x[i] <- xt
    }
    return(x)
}

    sigma <- 4    #parameter of proposal distribution
    k <- 4          #number of chains to generate
    n <- 20000     #length of chains
    b <- 1000       #burn-in length

    #generate the chains
    X <- matrix(0, nrow=k, ncol=n)
    for (i in 1:k)
        X[i, ] <- chisq.chain(sigma, n)

    #compute diagnostic statistics
    psi <- t(apply(X, 1, cumsum))
    for (i in 1:nrow(psi))
        psi[i,] <- psi[i,] / (1:ncol(psi))
#plot psi for the four chains
for (i in 1:k)
  if(i==1){
    plot((b+1):n,psi[i, (b+1):n],ylim=c(4.5,5.5), type="l",
        xlab='Index', ylab=bquote(phi))
  }else{
  lines(psi[i, (b+1):n], col=i)
  }


## ----warning=FALSE------------------------------------------------------------
    
    #plot the sequence of R-hat statistics
    rhat <- rep(0, n)
    for (j in (b+1):n)
        rhat[j] <- Gelman.Rubin(psi[,1:j])
    plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
    abline(h=1.2, lty=2)

## ----warning=FALSE------------------------------------------------------------
#mle generates the numerical solution
#compute log likelihood function root
obj.lambda <- function(left,right,lambda){
  k <- 0
  for (i in 1:length(left)){
    k <- k + (left[i] - right[i])*exp(lambda*(left[i] - right[i]))/(1-exp(lambda*(left[i] - right[i])))
  }
  sum(left) + k
}
left <- c(11,8,27,13,16,0,23,10,24,2)
right <- c(12,9,28,14,17,1,24,11,25,3)
u <- uniroot(obj.lambda,lower = 0.001,upper = 10e5,left = left,right = right)
result <- u$root
result

## ----warning=FALSE------------------------------------------------------------
set.seed(1234)
N <- 10000 #number of iterations
L <- 1  #set initial lambda 
ilow <- c(11,8,27,13,16,0,23,10,24,2)
iupper <- c(12,9,28,14,17,1,24,11,25,3)
for (i in 1:N){
  k <- 0
  for (j in 1:length(ilow)){
    m <- ilow[j]*exp(-L*ilow[j])-iupper[j]*exp(-L*iupper[j])
    o <- exp(-L*ilow[j])-exp(-L*iupper[j])
    k <- k + m/o + 1/L
  }
  L <- length(ilow)/k
}
L

## ----warning=FALSE------------------------------------------------------------
solve.game <- function(A) {
#let elements in A are positive
smallA <- min(A)
A <- A - smallA 
largeA <- max(A)
A <- A / max(A)
m <- nrow(A)
n <- ncol(A)
it <- n^3
#set the objective function
a <- c(rep(0, m), 1) 
A1 <- -cbind(t(A), rep(-1, n)) 
b1 <- rep(0, n)
A3 <- t(as.matrix(c(rep(1, m), 0)))
b3 <- 1
sx <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
maxi=TRUE, n.iter=it)

a <- c(rep(0, n), 1) 
A1 <- cbind(A, rep(-1, m)) 
b1 <- rep(0, m)
A3 <- t(as.matrix(c(rep(1, n), 0)))
b3 <- 1
sy <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
maxi=FALSE, n.iter=it)
soln <- list("A" = A * largeA + smallA,
"x" = sx$soln[1:m],
"y" = sy$soln[1:n],
"v" = sx$soln[m+1] * largeA + smallA)
soln
}
A <- matrix(c( 0,-2,-2,3,0,0,4,0,0,
2,0,0,0,-3,-3,4,0,0,
2,0,0,3,0,0,0,-4,-4,
-3,0,-3,0,4,0,0,5,0,
0,3,0,-4,0,-4,0,5,0,
0,3,0,0,4,0,-5,0,-5,
-4,-4,0,0,0,5,0,0,6,
0,0,4,-5,-5,0,0,0,6,
0,0,4,0,0,5,-6,-6,0), 9, 9)
library(boot) 
s1 <- solve.game(A)
s2 <- solve.game(A + 2)
round(cbind(s2$x, s2$y), 7)
A.va1 <- t(matrix(s1$x,9,1))%*% A
ava1 <- A.va1 %*% matrix(s1$y,9,1)
A.va2 <- t(matrix(s2$x,9,1))%*% (A+2)
ava2 <- A.va2 %*% matrix(s2$y,9,1)
ava1
ava2

## ----warning=FALSE------------------------------------------------------------
x <- list(1,"we")
y <- unlist(x)
z <- as.vector(x)
print(c(typeof(y[1]),typeof(y[2])))
print(c(typeof(z[1]),typeof(z[2])))

## -----------------------------------------------------------------------------
x <- c(1,2,3,4)
dim(x)

## -----------------------------------------------------------------------------
x <- matrix(c(1,2,3,4),2,2)
is.matrix(x)
is.array(x)

## -----------------------------------------------------------------------------
cy <- list("a","ah")
df <- data.frame(x=1:2,y=I(cy))
str(df)
k <- as.matrix(df)
k
typeof(k[2,2])

## -----------------------------------------------------------------------------
df <- data.frame(x=1,y=1:10)
d0 <- df[FALSE,]
d1 <- df[,FALSE]
d0

## ----warning=FALSE------------------------------------------------------------
scale01 <- function(x) {
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
}
df1 <- data.frame(x=c(1,1.3,1.5,1.6,2),y=3:7,z=c(TRUE,FALSE,FALSE,FALSE,FALSE))
l1 <- length(df1[1,])
l2 <- length(df1[,1])
res1 <- matrix(NA,l2,l1)
#Apply it to every column of a data frame
for (i in 1:l1){
  cla <- df1[1,i]
  if (is.character(cla)=="FALSE"){
      res1[,i] <- scale01(df1[,i])
  }
}
res1
res2 <- matrix(NA,l2,l1)
#apply it to every numeric column in a data frame
for (j in 1:l1){
  cla <- df1[1,j]
  if (is.numeric(cla)=="TRUE"){
      res2[,j] <- scale01(df1[,j])
  }
}
res2

## ----eval=FALSE---------------------------------------------------------------
#  df <- data.frame(x=1:6,y=c(2,4,6,1,9,8))
#  sd1 <- function(x){
#    funs <- c(sd)
#    sapply(funs, function(f) f(x, na.rm = TRUE))
#  }
#  vapply(df,sd1,FUN.VALUE=c(sd=0))
#  

## ----eval=FALSE---------------------------------------------------------------
#  df <- data.frame(x=1:6,y=c(2,4,6,1,9,8),z=c("a","b","c","d","e","f"))
#  
#  sd1 <- function(x){
#    funs <- c(sd)
#    sapply(funs, function(f) f(x, na.rm = TRUE))
#  }
#  vapply(df,sd1,FUN.VALUE=c(sd=0))
#  

## ----warning=FALSE------------------------------------------------------------
#set initial value
gr <-function(N,M){
x <- matrix(0,M,2)
n <- 5
a <- 2
b <- 3
x1 <-1
x2<-0.1
for (j in 1:M){
  for (i in 1:N){
    x1 <- rbinom(1,n,x2)
    x2 <- rbeta(1,x1+a,n-x1+b)
  }
  x[j,1] <- x1
  x[j,2] <- x2
}
  return(x)
}

## ----eval=FALSE---------------------------------------------------------------
#  #include <Rcpp.h>
#  using namespace Rcpp;
#  // [[Rcpp::export]]
#  NumericMatrix gibbsC(int N, int M) {
#    NumericMatrix mat(N, 2);
#    double x = 1, y = 0.1;
#    for(int i = 0; i < M; i++) {
#      for(int j = 0; j < N; j++) {
#        x = rbinom(1, 5, y)[0];
#        y = rbeta(1,x+2, 8-x)[0];
#      }
#      mat(i, 0) = x;
#      mat(i, 1) = y;
#    }
#    return(mat);
#  }

## ----eval=FALSE---------------------------------------------------------------
#  
#  g_cpp <- "../work/"
#  sourceCpp(paste0(g_cpp,"gibbs.cpp"))
#  library(microbenchmark)
#  N <- 1000
#  M <- 10
#  ts <- microbenchmark(gr=gr(N,M),cppgr=gibbsC(N,M))
#  summary(ts)[,c(1,3,5,6)]

