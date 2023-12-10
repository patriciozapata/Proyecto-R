###########################################################
#
# Copyright (C) 2012-2020 by Chi Yau
# All rights reserved
#
# http://www.r-tutor.com
#

################################
# c23-s01

library(MASS)
y <- na.omit(survey$Height)
n <- length(y)

mean(y)
10/sqrt(n)

# Ex
library(MASS)
y <- na.omit(survey$Pulse)
n <- length(y)

mean(y)
12/sqrt(n)



################################
# c23-s02

library(MASS)
y <- na.omit(survey$Height)
n <- length(y)

tau <- 1/(10*10)
t.prior <- 1/(1.2*1.2)

t.post <- t.prior + n * tau
s.post <- sqrt(1/t.post); s.post

m.prior <- 160
m.post <- (1/t.post) *
    (m.prior * t.prior + sum(y) * tau)
m.post


# Ex
y <- na.omit(survey$Pulse)
n <- length(y)

tau <- 1/(12*12)
t.prior <- 1/(1.4*1.4)

t.post <- t.prior + n * tau
s.post <- sqrt(1/t.post); s.post

m.prior <- 68
m.post <- (1/t.post) *
    (m.prior * t.prior + sum(y) * tau)
m.post



##############################################
# c23-s03

library(MASS)
y <- na.omit(survey$Height)
N <- length(y); N

model_code = 
"
data {
  // data size
  int<lower=1> N; 
  
  // heights
  real<lower=0> y[N];   
}

parameters {
  // mean
  real<lower=0> mu;     
}

model {
  // prior of mu
  mu ~ normal(160, 1.2);
    
  // likelihood of mu
  y ~ normal(mu, 10);
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

fs["mu", c("mean", "sd")]


# Ex
library(MASS)

y <- na.omit(survey$Pulse)
N <- length(y); N

model_code = 
"
data {
  // data size
  int<lower=1> N; 
  
  // pulse
  real<lower=0> y[N];   
}

parameters {
  // mean
  real<lower=0> mu;     
}

model {
  // prior of mu
  mu ~ normal(68, 1.4);
    
  // likelihood of mu
  y ~ normal(mu, 12);
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

fs["mu", c("mean", "sd")]



##############################################
# c23-s04

library(MASS)
y <- na.omit(survey$Height)
N <- length(y); N

model_code = 
"
data {
  // data size
  int<lower=1> N; 
  
  // pulse
  real<lower=0> y[N];   
}

parameters {
  // mean
  real<lower=0> mu;
       
  // standard deviation
  real<lower=0> sigma;
}

model {
  // priors
  mu ~ normal(160, 1.2);
  sigma ~ exponential(1e-02);
  
  // likelihood
  y ~ normal(mu, sigma);
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

fs[c("mu", "sigma"), "mean"]


# Ex
library(MASS)

y <- na.omit(survey$Pulse)
N <- length(y); N

model_code = 
"
data {
  // data size
  int<lower=1> N; 
  
  // pulse
  real<lower=0> y[N];   
}

parameters {
  // mean
  real<lower=0> mu;
       
  // standard deviation
  real<lower=0> sigma;
}

model {
  // priors
  mu ~ normal(68, 1.4);
  sigma ~ exponential(1e-02);
  
  // likelihood
  y ~ normal(mu, sigma);
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

fs[c("mu", "sigma"), "mean"]



