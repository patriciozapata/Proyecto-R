###########################################################
#
# Copyright (C) 2012-2020 by Chi Yau
# All rights reserved
#
# http://www.r-tutor.com
#

################################
# c25-s01

# rstanarm
library("rstanarm")
options(mc.cores = parallel::detectCores())

fit <- stan_glm(
    eruptions ~ waiting, 
    data = faithful
    )
print(fit, digits=2)

fs <- summary(
    fit, 
    probs = c(0.025, 0.975)
    )
all(fs[,"Rhat"] < 1.1)

print(fs, digits=4)
prior_summary(fit)


library(shinystan)
launch_shinystan(fit)


# frequentist
faithful.lm = lm(
    eruptions ~ waiting, 
    data=faithful)
cbind(coefficients(faithful.lm))



################################
# c25-s02

res <- posterior_predict(
    fit, 
    newdata=data.frame(waiting=80)
    )
dim(res)

mean(res)

quantile(res, c(.025, .975))


# frequentist
faithful.lm = lm(
    eruptions ~ waiting, 
    data=faithful)
predict(faithful.lm, 
    newdata = data.frame(waiting=80), 
    interval="predict")


################################
# c25-s03

alpha <- fs["(Intercept)", "mean"]; alpha
beta  <- fs["waiting", "mean"]; beta
sigma <- fs["sigma", "mean"]; sigma

yhat  <- alpha + beta * faithful$waiting
stdres <- (faithful$eruptions - yhat)/sigma


png(file="simple-regression7x.png", width=450, height=450)
plot(faithful$waiting, stdres, 
     ylab="Standardized Residuals", 
     xlab="Waiting Time", 
     main="Old Faithful Eruptions") 
abline(0, 0)    # the horizon
dev.off()


png(file="simple-regression8x.png", width=450, height=450)
qqnorm(stdres, 
     ylab="Standardized Residuals", 
     xlab="Normal Scores", 
     main="Old Faithful Eruptions") 
qqline(stdres) 
dev.off()

