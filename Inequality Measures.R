##______________________Joshua Winter____________Statistical Computing Final________________Inequality Measures____________________________


####______________________________________________________________________________________________________________________________________
##_____________________________Initial Setup and Dataset Creation__________________________________________________________________________



## this is used to allow for the plotting of 4 charts at once

par(mfrow=c(2,2)) 


## small vectors of 10 randomly picked observations meant to represent incomes of individuals in different countries 

US<-c(50000, 50000, 50000, 50000, 50000, 45000, 45000, 40000, 55000, 55000)

UK<-c(70000, 80000, 75000, 20000, 20000, 20000, 80000, 15000, 15000, 15000)

SK<-c(20000, 20000, 25000, 20000, 25000, 30000, 30000, 30000, 27500, 22500)

NK<-c(1000000, 100000, 1000, 1000, 250000, 5000, 5000, 2000, 2000, 2000)

SA<-c(6000,5000,5000,5000,4750,4500,4500,5500,5250,5000)

CA<-c(1,10,30,2500,50000,4500,100000,15000,500,65000)

## historgrams of each country's incomes are used to show that the US and South Korea have relative equal distributions compared to North Korea and the UK

hist(US, main = "US Income Distribution")

hist(UK, main="UK Income Distribution")

hist(SK, main="SK Income Distribution")


hist(NK, main="NK Income Distribution")

layout(matrix(c(1,2),2,1))

hist(SA, main="Saudi Arabia Income Distribution")

hist(CA, main="Canada Income Distribution")


####_______________________________Atkinson_________________________________________________

## .5 is normally used as the default parameter for this function

Atkinson<-function (x, parameter = 0.5, na.rm = TRUE) 
  
{
  if (!na.rm && any(is.na(x))) 
    return(NA_real_)
  x <- as.numeric(na.omit(x))
  if (is.null(parameter)) 
    parameter <- 0.5
  if (parameter == 1) 
    A <- 1 - (exp(mean(log(x)))/mean(x))
  else {
    x <- (x/mean(x))^(1 - parameter)
    A <- 1 - mean(x)^(1/(1 - parameter))
  }
  A
}

Atkinson(US,parameter = .5, na.rm = TRUE)
Atkinson(UK,parameter = .5, na.rm = TRUE)
Atkinson(SK,parameter = .5, na.rm = TRUE)
Atkinson(NK,parameter = .5, na.rm = TRUE)
Atkinson(SA,parameter = .5, na.rm = TRUE)


####_____________________________Theil Index__________________________________________________

Theil<-function (x, parameter = 0, na.rm = TRUE) 
{
  if (!na.rm && any(is.na(x))) 
    return(NA_real_)
  x <- as.numeric(na.omit(x))
  if (is.null(parameter)) 
    parameter <- 0
  if (parameter == 0) {
    x <- x[!(x == 0)]
    Th <- x/mean(x)
    Th <- sum(x * log(Th))
    Th <- Th/sum(x)
  }
  else {
    Th <- exp(mean(log(x)))/mean(x)
    Th <- -log(Th)
  }
  Th
}

Theil(US)
Theil(UK)
Theil(SK)
Theil(NK)
Theil(SA)
Theil(CA)

####___________________________Gini Coefficient________________________________

Gini<-function (x, n = rep(1, length(x)), unbiased = TRUE, conf.level = NA, 
                R = 1000, type = "bca", na.rm = FALSE) 

  {
  x <- rep(x, n)
  if (na.rm) 
    x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) 
    return(NA_real_)
  i.gini <- function(x, unbiased = TRUE) {
    n <- length(x)
    x <- sort(x)
    res <- 2 * sum(x * 1:n)/(n * sum(x)) - 1 - (1/n)
    if (unbiased) 
      res <- n/(n - 1) * res
    return(pmax(0, res))
  }
  if (is.na(conf.level)) {
    res <- i.gini(x, unbiased = unbiased)
  }
  else {
    boot.gini <- boot(x, function(x, d) i.gini(x[d], unbiased = unbiased), R = R)
    ci <- boot.ci(boot.gini, conf = conf.level, type = type)
    res <- c(gini = boot.gini$t0, lwr.ci = ci[[4]][4], upr.ci = ci[[4]][5])
  }
  return(res)
}

Gini(US,n=rep(1,length(US)),unbiased=TRUE,conf.level = NA,R=1000,type = "bca",na.rm = FALSE)
Gini(UK,n=rep(1,length(UK)),unbiased=TRUE,conf.level = NA,R=1000,type = "bca",na.rm = FALSE)
Gini(SK,n=rep(1,length(SK)),unbiased=TRUE,conf.level = NA,R=1000,type = "bca",na.rm = FALSE)
Gini(NK,n=rep(1,length(NK)),unbiased=TRUE,conf.level = NA,R=1000,type = "bca",na.rm = FALSE)
Gini(SA,n=rep(1,length(SA)),unbiased=TRUE,conf.level = NA,R=1000,type = "bca",na.rm = FALSE)
Gini(CA,n=rep(1,length(CA)),unbiased=TRUE,conf.level = NA,R=1000,type = "bca",na.rm = FALSE)


####___________________________________Lorenz Curve_________________________________________

Lc<-function (x, n = rep(1, length(x)), plot = FALSE) 
{
  ina <- !is.na(x)
  n <- n[ina]
  x <- as.numeric(x)[ina]
  k <- length(x)
  o <- order(x)
  x <- x[o]
  n <- n[o]
  x <- n * x
  p <- cumsum(n)/sum(n)
  L <- cumsum(x)/sum(x)
  p <- c(0, p)
  L <- c(0, L)
  L2 <- L * mean(x)/mean(n)
  Lc <- list(p, L, L2)
  names(Lc) <- c("p", "L", "L.general")
  class(Lc) <- "Lc"
  if (plot) 
    plot(Lc)
  Lc
}


## Creation of Lorenz Curves for Each Country

## United States

Lc(US)

US<-Lc(US, n = rep(1,length(US)), plot =F)

plot(US$p,US$L,
     col="blue",
     type="b",      # it should be "b"
     lty=5,
     lwd=2,
     main="United States Lorenz Curve"     
)

## United Kingdom

Lc(UK)

UK<-Lc(UK, n = rep(1,length(UK)), plot =F)

plot(UK$p,UK$L,
     col="red",
     type="b",     
     lty=5,
     lwd=2,
     main="United Kindom Lorenz Curve"     
)

## South Korea

Lc(SK)

SK<-Lc(SK, n = rep(1,length(SK)), plot =F)

plot(SK$p,SK$L,
     col="cyan",
     type="b",     
     lty=5,
     lwd=2,
     main="South Korea Lorenz Curve"     
)

## North Korea

Lc(NK)

NK<-Lc(NK, n = rep(1,length(NK)), plot =F)

plot(NK$p,NK$L,
     col="darkorchid",
     type="b",     
     lty=5,
     lwd=2,
     main="North Korea Lorenz Curve"     
)

## Saudi Arabia

Lc(SA)

SA<-Lc(SA, n = rep(1,length(SA)), plot =F)

plot(SA$p,SA$L,
     col="dodgerblue2",
     type="b",     
     lty=5,
     lwd=2,
     main="Saudi Arabia Lorenz Curve"     
)

## Canada

CA<-c(1,10,30,2500,50000,4500,100000,15000,500,65000)

Lc(CA)

CA<-Lc(CA, n = rep(1,length(CA)), plot =F)

plot(CA$p,CA$L,
     col="firebrick",
     type="b",     
     lty=5,
     lwd=2,
     main="Canada Lorenz Curve"     
)

## China

CN=c(26.1,16.1,15.5,15.4,14.8,14.7,13.7,12.1,11.7,11.6,11,10.8,10.8,7.5)

Lc(CN)

CN<-Lc(CN, n = rep(1,length(CN)), plot =F)

plot(CN$p,CN$L,
     col="gold",
     type="b",     
     lty=5,
     lwd=2,
     main="China Lorenz Curve"     
)


## __________________Additional Exercises/Material Expansion Using Random Number Generators


## Sample of 100

x<-runif(100, min=0, max=1)

hist(x)

Atkinson(x)
Theil(x)

Gini(x,n=rep(1,length(x)),unbiased=TRUE,conf.level = NA,R=1000,type = "bca",na.rm = FALSE)

Lc(x)

x<-Lc(x, n = rep(1,length(x)), plot =F)

plot(x$p,x$L,
     col="gold",
     type="b",     
     lty=5,
     lwd=2,
     main="x"     
)

## Sample of 10000

y<-runif(1000, min=1000, max=100000)

hist(y)

Atkinson(y)

Theil(y)

Gini(y,n=rep(1,length(y)),unbiased=TRUE,conf.level = NA,R=1000,type = "bca",na.rm = FALSE)

Lc(y)

y<-Lc(y, n = rep(1,length(y)), plot =F)

plot(y$p,y$L,
     col="firebrick",
     type="b",     
     lty=5,
     lwd=2,
     main="y"     
)

## Sample of 10000

w<-runif(10000, min=500, max=1000)

hist(w)

Atkinson(w)

Theil(w)

Gini(w,n=rep(1,length(w)),unbiased=TRUE,conf.level = NA,R=1000,type = "bca",na.rm = FALSE)

Lc(w)

w<-Lc(w, n = rep(1,length(w)), plot =F)

plot(w$p,w$L,
     col="red",
     type="b",     
     lty=5,
     lwd=2,
     main="y"     
)

## Sample of 1000000

z<-runif(1000000, min=0, max=1)

hist(z)

Atkinson(z)

Theil(z)

Gini(z,n=rep(1,length(z)),unbiased=TRUE,conf.level = NA,R=1000,type = "bca",na.rm = FALSE)

Lc(z)

z<-Lc(z, n = rep(1,length(z)), plot =F)

plot(z$p,z$L,
     col="blue",
     type="b",     
     lty=5,
     lwd=2,
     main="z"     
)


## You could also look at calculating equality measures for other sequences, distributions, etc. 
