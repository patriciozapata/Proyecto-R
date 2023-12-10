###########################################################
#
# Copyright (C) 2012-2020 by Chi Yau
# All rights reserved
#
# http://www.r-tutor.com
#


################################
# c21-s01
library(MASS)
tbl <- table(survey$Smoke); tbl

N <- as.numeric(sum(tbl)); N
y <- N - as.numeric(tbl["Never"]); y

a <- 1+y; a
b <- 1+(N-y); b

c <- a+b
mu <- a/c; mu
sigma <- sqrt(a*b/(c*c*(c+1))); sigma

# Ex
a <- 82+y; a
b <- 120+(N-y); b

c <- a+b
mu <- a/c; mu
sigma <- sqrt(a*b/(c*c*(c+1))); sigma



################################
# c21-s02

library(MASS)
tbl <- table(survey$Smoke); tbl

N <- as.numeric(sum(tbl)); N
y <- N - as.numeric(tbl["Never"]); y

mu <- 0.35
sigma <- 0.025

c <- mu*(1-mu)/(sigma*sigma) - 1
a <- mu*c; a
b <- (1-mu)*c; b

a1 <- a+y; a1
b1 <- b+(N-y); b1

c1 <- a1+b1
mu1 <- a1/c1; mu1
sigma1 <- sqrt(a1*b1/(c1*c1*(c1+1))); sigma1


# Ex
mu <- 0.15
sigma <- 0.03

c <- mu*(1-mu)/(sigma*sigma) - 1
a <- mu*c; a
b <- (1-mu)*c; b

a1 <- a+y; a1
b1 <- b+(N-y); b1

c1 <- a1+b1
mu1 <- a1/c1; mu1
sigma1 <- sqrt(a1*b1/(c1*c1*(c1+1))); sigma1




################################
# c21-s03

library(MASS)
tbl <- table(survey$Smoke); tbl

N <- as.numeric(sum(tbl)); N
y <- N - as.numeric(tbl["Never"]); y


# stan model
model_code = 
"
data {
  int<lower=1> N;   // number of trials
  int<lower=0> y;   // number of successes
}

parameters {
  // chance of success
  real<lower=0, upper=1> p;     
}

model {
  p ~ beta(1, 1);       // prior
  y ~ binomial(N, p);   // likelihood
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
colnames(fs)

all(fs[, "Rhat"] < 1.1)
all(fs[, "n_eff"] > 20)

fs["p", c("mean", "sd")]

# figures
png(file="bayesian-rstan0x.png", width=450, height=450)
traceplot(fit)
dev.off()

# extracting the posterior draws
p_draws = extract(fit)$p

# posterior intervals
quantile(p_draws, probs=c(0.10, 0.90))

# figures
png(file="bayesian-rstan1x.png", width=450, height=450)
plot(density(p_draws))
dev.off()

library(shinystan)
launch_shinystan(fit)


# Ex
# stan model
model_code = 
"
data {
  int<lower=1> N;   // number of trials
  int<lower=0> y;   // number of successes
}

parameters {
  // chance of success
  real<lower=0, upper=1> p;     
}

model {
  p ~ beta(85, 120);    // prior
  y ~ binomial(N, p);   // likelihood
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

fs["p", c("mean", "sd")]

