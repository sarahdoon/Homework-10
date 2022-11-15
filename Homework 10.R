# 1) The code in survival_simulations.R simulates a realistic dataset,
# with censoring of data etc. How does it work?  Just sketch out the
# logic of the code, in a few words/diagram,  not a detailed explanation. 

##R code

# baseline hazard: Weibull
# N = sample size
# lambda = scale parameter in h0()
# rho = shape parameter in h0()
# beta = fixed effect parameter
# rateC = rate parameter of the exponential distribution of C
simulWeib <- function(N, lambda, rho, beta, rateC)
{
  # covariate --> N Bernoulli trials
  x <- sample(x=c(0, 1), size=N, replace=TRUE, prob=c(0.5, 0.5))
  
  # Weibull latent event times
  v <- runif(n=N)
  Tlat <- (- log(v) / (lambda * exp(x * beta)))^(1 / rho)
  
  # censoring times
  C <- rexp(n=N, rate=rateC)
  
  # follow-up times and event indicators
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  
  # data set
  data.frame(id=1:N,
             time=time,
             status=status,
             x=x)
}
# name a function to variable
# inside function argument, give variables N, lambda, rho, beta, and rateC
# inside code, name variable x
?sample
## sample takes sample from specified elements of x, with replacing set to true
## c(0,1) refers to the categories
## variable c is given prob .5 and .5 because there are two possible events, censoring or death
?runif
# inside code, name variable v
## if number of observations is equal to N (sample size)
# inside code, name variable Tlat and put equation for weibull use
?rexp
# inside code, name variable C
## use rexp function with n=N and rate=rateC
?pmin
# inside code, name variable time
# use Tlat and C variables are arguments in in pmin function
?as.numeric
# inside code, name variable status, make Tlat and C numeric with as.numeric
?data.frame()
# create the dataset with data.frame
# N goes in id col name, time goes in time col name, numeric of weibull goes into status col name, sample does into x col name
## generate a dataset to simulate a set of events
# assign arguments
dat <- simulWeib(N=100, lambda=0.01, rho=1, beta=-0.6, rateC=0.001)
dat
# x can be 0 or 1, censor or death. there are more 1's possibly influenced by the rate being so small, more death, less censoring

#Test of simulations
#Here is some quick simulation with beta=−0.6:
library(survival) # import survival library
?set.seed()
# random number generator
set.seed(1234)
?rep
betaHat <- rep(NA, 1e3) # replace in NA, 1e3 times
for(k in 1:1e3) # for variables from 1 until 1e3,
{
  dat <- simulWeib(N=100, lambda=0.01, rho=1, beta=-0.6, rateC=0.001) # set arguments to function and result to variable
  fit <- coxph(Surv(time, status) ~ x, data=dat) # the data is dat, in here, the surv time and status columens with relation to x are fit for cox analysis
  #time: for right censored data, this is the follow up time.  For
  # event: The status indicator, normally 0=alive, 1=dead.  Other
  # choices are ‘TRUE’/‘FALSE’ (‘TRUE’ = death) or 1/2 (2=death).
  # For interval censored data, the status indicator is 0=right censored, 1=event at ‘time’,
  
  betaHat[k] <- fit$coef # the coefficient in the fit will be applied to the betahat variable
}
mean(betaHat) #this tells you average coefficient

#> mean(betaHat)
#[1] -0.6085473

# Problem 2
# Which condition dominates in determining engine life?
library(R.utils)
gunzip("/Users/sarahferaidoon/Desktop/turbofan_degradation.tar.gz", remove=FALSE)
turbo <- read.tar("/Users/sarahferaidoon/Desktop/turbofan_degradation.tar")
turbo
library(archive)
library(readr)
read_csv(archive_read("/Users/sarahferaidoon/Desktop/turbofan_degradation.tar", file = 3), col_types = cols())
# I can't figure out how to open this
# what I would do if I could access the data set is take KM curve, compare, see which one seems to be the most helpful