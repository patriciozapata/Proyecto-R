###########################################################
#
# Copyright (C) 2012-2020 by Chi Yau
# All rights reserved
#
# http://www.r-tutor.com
#


################################
# c22-s01

y <- as.numeric(discoveries)

a <- 1+sum(y); a
b <- 0+length(y); b

mu <- a/b; mu
sigma <- sqrt(a)/b; sigma

qgamma(0.5, a, b)


# Ex
a <- 250 + sum(y); a
b <- 120 + length(y); b

mu <- a/b; mu
sigma <- sqrt(a)/b; sigma

qgamma(0.5, a, b)



################################
# c22-s02

y <- as.numeric(discoveries)

mu <- 2.15
sigma <- 0.15

b <- mu/(sigma*sigma); b
a <- mu*b; a

a1 <- a+sum(y); a1
b1 <- b+length(y); b1

mu1 <- a1/b1; mu1
sigma1 <- sqrt(a1)/b1; sigma1

qgamma(0.5, a1, b1)


# Ex
mu <- 3.82
sigma <- 0.17

b <- mu/(sigma*sigma); b
a <- mu*b; a

a1 <- a+sum(y); a1
b1 <- b+length(y); b1

mu1 <- a1/b1; mu1
sigma1 <- sqrt(a1)/b1; sigma1

qgamma(0.5, a1, b1)



################################
# c22-s03

y <- as.numeric(discoveries)
N <- length(y); N

model_code = 
"
data {
  int<lower=1> N;       // trials
  int<lower=0> y[N];    // occurrences
}

parameters {
  // mean occurrences per interval
  real<lower=0> lambda;         
}

model {
  lambda ~ uniform(0, 1e10);  // prior
  y ~ poisson(lambda);        // likelihood
}
"

library(rstan)
fit <- stan(
  model_code=model_code, 
  data=list(y=y, N=N), 
  iter=5000, 
  cores=2 
)

fs <- summary(fit)$summary
all(fs[,"Rhat"] < 1.1)

fs["lambda", c("mean", "50%", "sd")]


# Ex
model_code = 
"
data {
  // number of time periods
  int<lower=1> N;
  
  // occurrences in each time period
  int<lower=0> y[N];
}

parameters {
  // mean occurrences per time period]
  real<lower=0> lambda;         
}

model {
  lambda ~ gamma(250, 120); // prior
  y ~ poisson(lambda);      // likelihood
}
"

library(rstan)
fit <- stan(
  model_code=model_code, 
  data=list(y=y, N=N), 
  iter=5000, 
  cores=2 
)

fs <- summary(fit)$summary
all(fs[,"Rhat"] < 1.1)

fs["lambda", c("mean", "50%", "sd")]


