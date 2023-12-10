###########################################################
#
# Copyright (C) 2012-2020 by Chi Yau
# All rights reserved
#
# http://www.r-tutor.com
#

################################
# c26-s01

df <- stackloss
str(df)

colnames(df) <- c(
    "airflow", "watertemp", "acidconc",
    "stackloss"
    )
str(df)


# rstanarm
library("rstanarm")
options(mc.cores = parallel::detectCores())

fit <- stan_glm(
    stackloss ~ 
        airflow + watertemp + acidconc, 
    data = df 
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


# frequentist
stackloss.lm <- lm(
    stackloss ~ 
        airflow + watertemp + acidconc, 
    data = df)
cbind(coefficients(stackloss.lm))



################################
# c26-s02

newdata <- data.frame(
    airflow = 72,
    watertemp = 20,
    acidconc = 85
    )
res <- posterior_predict(
    fit, 
    newdata=newdata
    )
    
dim(res)

mean(res)

quantile(res, c(.025, .975))


# frequentist prediction
stackloss.lm <- lm(
    stackloss ~ 
        airflow + watertemp + acidconc, 
    data = df)
predict(stackloss.lm, newdata,
    interval="predict")




