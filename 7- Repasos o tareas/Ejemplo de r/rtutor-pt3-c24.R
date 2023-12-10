###########################################################
#
# Copyright (C) 2012-2020 by Chi Yau
# All rights reserved
#
# http://www.r-tutor.com
#

################################
# c24-s01

library(MASS)
y1 <- immer$Y1
y2 <- immer$Y2

y <- y1 - y2
N <- length(y); N

sd(y)


model_code = 
"
data {
  // data size
  int<lower=1> N;
  
  // difference
  real y[N];
}

parameters {
  // mean
  real mu;
  
  // standard deviation
  real<lower=0> sigma;
}

model {
  // priors
  mu ~ normal(0, 260);
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

fs["mu", c("mean", "2.5%", "97.5%")]


# posterior 99% interval
alpha <- 0.01
I <- c(alpha/2, 1-alpha/2)

mu_draws = extract(fit)$mu
quantile(mu_draws, probs=I)




################################
# c24-s02

OJ <- ToothGrowth$supp == "OJ"
growth <- ToothGrowth$len

y1 <- growth[OJ]
y2 <- growth[!OJ]

N1 <- length(y1)
N2 <- length(y2)

mean(growth); sd(growth)


model_code = 
"
data {
  int<lower=1> N1;
  int<lower=1> N2;      
  
  real<lower=0> y1[N1]; 
  real<lower=0> y2[N2]; 
}

parameters {
  real<lower=0> mu1;
  real<lower=0> mu2;
  
  real<lower=0> sigma;
}

transformed parameters {
  real delta;
  delta = mu1 - mu2;
}

model {
  mu1 ~ normal(19, 100);
  mu2 ~ normal(19, 100);
  
  sigma ~ exponential(1e-02);
  
  y1 ~ normal(mu1, sigma);
  y2 ~ normal(mu2, sigma);
}
"

library(rstan)
fit <- stan(
  model_code=model_code, 
  data=list(y1=y1, y2=y2, N1=N1, N2=N2), 
  iter=5000, 
  cores=2 
  )

fs <- summary(fit)$summary
all(fs[,"Rhat"] < 1.1)

print(fs[
  c("mu1", "mu2", "delta"), 
  c("mean", "2.5%", "97.5%")
  ], digits=2)
  



################################
# c24-s03

y1 <- beaver1$temp
y2 <- beaver2$temp

N1 <- length(y1)
N2 <- length(y2)

y <- c(y1, y2)
mean(y); sd(y)

model_code = 
"
data {
  int<lower=1> N1;
  int<lower=1> N2;      
  
  real<lower=0> y1[N1]; 
  real<lower=0> y2[N2]; 
}

parameters {
  real<lower=0> mu1;
  real<lower=0> mu2;
  
  real<lower=0> sigma1;
  real<lower=0> sigma2;
}

transformed parameters {
  real delta;
  delta = mu1 - mu2;
}

model {
  mu1 ~ normal(37, 10);
  mu2 ~ normal(37, 10);
  
  sigma1 ~ exponential(1e-02);
  sigma2 ~ exponential(1e-02);
  
  y1 ~ normal(mu1, sigma1);
  y2 ~ normal(mu2, sigma2);
}
"

library(rstan)
fit <- stan(
  model_code=model_code, 
  data=list(y1=y1, y2=y2, N1=N1, N2=N2), 
  iter=5000, 
  cores=2 
  )

fs <- summary(fit)$summary
all(fs[,"Rhat"] < 1.1)

print(fs[
  c("mu1", "mu2", "delta"), 
  c("mean", "2.5%", "97.5%")
  ], digits=2)



