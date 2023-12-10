###########################################################
#
# Copyright (C) 2012-2020 by Chi Yau
# All rights reserved
#
# http://www.r-tutor.com
#

################################
# c27-s01

# rstanarm
library("rstanarm")
options(mc.cores = parallel::detectCores())

my_prior <- normal(
    location = 0, 
    scale = 100, 
    autoscale = FALSE
    ) 
fit <- stan_glm(
    am ~ hp + wt, data = mtcars,
    family = binomial(link = "logit"),
    prior = my_prior,
    prior_intercept = my_prior
    )

fs <- summary(
    fit, 
    probs = c(0.025, 0.975)
    )
all(fs[,"Rhat"] < 1.1)

print(
    fs[, c("mean", "2.5%", "97.5%")], 
    digits=2
    )
prior_summary(fit)


# predict
newdata <- data.frame(
    hp = 120,
    wt = 2.8
    )
res <- posterior_predict(
    fit, 
    newdata=newdata
    )

mean(res)

quantile(res, c(.025, .975))


# frequentist
am.glm <- glm(formula=am ~ hp + wt, 
    data=mtcars, family=binomial)
newdata = data.frame(hp=120, wt=2.8)
predict(am.glm, newdata, type="response")



##############################################
# c27-s02

x <- c(-1.2198, -0.5371, -0.3064, 
         0.1578, 0.7695)
y <- c(11, 8, 3, 2, 1)
n <- c(15, 20, 12, 18, 16)

N <- max(n); N
r <- round((y * N) / n); r
s <- N - r; s
df <- data.frame(x, r, s); df


# rstanarm
library("rstanarm")
options(mc.cores = parallel::detectCores())

my_prior <- normal(
    location = 0, 
    scale = 100, 
    autoscale = FALSE
    ) 
fit <- stan_glm(
    cbind(r, s) ~ x, data = df,
    family = binomial(link = "logit"),
    prior = my_prior,
    prior_intercept = my_prior
    )

fs <- summary(
    fit, 
    probs = c(0.025, 0.975)
    )
all(fs[,"Rhat"] < 1.1)

res <- posterior_predict(
    fit, 
    newdata=data.frame(x=-0.4)
    )

mean(res)/N

quantile(res, c(.025, .975))/N
