###########################################################
#
# Copyright (C) 2012-2020 by Chi Yau
# All rights reserved
#
# http://www.r-tutor.com
#

################################
# c28-s01

tbl <- read.table("fastfood-1.txt", 
    header=TRUE)
tbl

ngroup <- ncol(tbl); ngroup
nsample <- nrow(tbl); nsample
N <- ngroup*nsample; N

tm <- gl(ngroup, nsample, N); tm
y <- as.numeric(as.matrix(tbl)); y

df <- data.frame(tm=tm, y=y); df


library("rstanarm")
options(mc.cores = parallel::detectCores())

my_prior <- normal(
    location = 0, 
    scale = 100, 
    autoscale = FALSE
    ) 
fit <- stan_lmer(
    y ~ (1 | tm), 
    data = df, 
    prior = my_prior,
    prior_intercept = my_prior
)
prior_summary(fit)
print(fit, digits=2)

fs <- summary(fit) 
all(fs[,"Rhat"] < 1.1)

# extract the posterior draws of all parameters
sims <- as.matrix(fit)
dim(sims)
colnames(sims)


# extract posterior draws of slope parameters
s <- "b\\[\\(Intercept\\) tm\\:"
s_sims <- as.matrix(
    fit, 
    regex_pars = s
    )
dim(s_sims)
colnames(s_sims)


# difference between menu items
I <- c(0.025, 0.975)

diff_2_1 <- s_sims[, 2] - s_sims[, 1]
quantile(diff_2_1, I)

diff_3_1 <- s_sims[, 3] - s_sims[, 1]
quantile(diff_3_1, I)

diff_3_2 <- s_sims[, 3] - s_sims[, 2]
quantile(diff_3_2, I)


# https://mc-stan.org/rstanarm/reference/prior_summary.stanreg.html
prior_summary(fit)


################################
# c28-s02

tbl <- read.table("fastfood-2.txt", 
    header=TRUE)
tbl

ngroup <- ncol(tbl); ngroup
nsample <- nrow(tbl); nsample
N <- ngroup*nsample; N

tm <- gl(ngroup, nsample, N); tm
blk <- gl(nsample, 1, N); blk
y <- as.numeric(as.matrix(tbl)); y

df <- data.frame(tm=tm, blk=blk, y=y); df

library("rstanarm")
options(mc.cores = parallel::detectCores())

my_prior <- normal(
    location = 0, 
    scale = 100, 
    autoscale = FALSE
    ) 
fit <- stan_lmer(
    y ~ tm + (1 | blk), 
    data = df, 
    prior = my_prior,
    prior_intercept = my_prior
)
print(fit, digits=2)

fs <- summary(fit) 
all(fs[,"Rhat"] < 1.1)


# extract the posterior draws of all parameters
sims <- as.matrix(fit)
dim(sims)

colnames(sims)

# difference between menu items
I <- c(0.025, 0.975)
quantile(sims[, 2], I)
quantile(sims[, 3], I)
quantile(sims[, 3] - sims[, 2], I)


################################
# c28-s03

tbl <- read.csv("fastfood-3.csv")
tbl

f1 <- c("Item1", "Item2", "Item3")
f2 <- c("East", "West")
n1 <- length(f1) 
n2 <- length(f2)  

nsample <- 4
N <- n1*n2*nsample; N

tm <- gl(n1, nsample*n2, N); tm
fc <- gl(n2, nsample, N); fc
y <- as.numeric(as.matrix(tbl))

df <- data.frame(tm=tm, fc=fc, y=y); df

library("rstanarm")
options(mc.cores = parallel::detectCores())

my_prior <- normal(
    location = 0, 
    scale = 100, 
    autoscale = FALSE
    ) 
fit <- stan_lmer(
    y ~ tm + (tm | fc), 
    data = df, 
    prior = my_prior,
    prior_intercept = my_prior
)
print(fit, digits=2)

fs <- summary(fit) 
all(fs[,"Rhat"] < 1.1)


# extract the posterior draws of all parameters
sims <- as.matrix(fit)
dim(sims)

colnames(sims)

tm2_fc1 <- sims[,4]+sims[,5]
tm2_fc2 <- sims[,7]+sims[,8]
tm2_fc2_fc1 <- tm2_fc2 - tm2_fc1 

I <- c(0.025, 0.975)
quantile(tm2_fc2_fc1, I)

