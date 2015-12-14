# quick check after running
out1$name
names(out1$model)
out1$model$psrf
length(which(out1$model$psrf$psrf[,1]>1.05))
out1$model$psrf$psrf[which(out1$model$psrf$psrf[,1]>1.05),1]
as.matrix(out1$model$psrf$psrf[which(out1$model$psrf$psrf[,1]>1.05),1])

out1$model$autocorr
out1$model$thin
out1$model$dic

# for the bet model
rm(list = ls())
source("_scripts/run_sit_models.r")
out8 <- simulate_model("RevLearn_RLcoh_2lr_bet")

#### after running ####
library(runjags)
library(rjags)
library(lattice)
library(coda)
library(modeest)

load("_outputs/bet_output.rdata")
load("_outputs/hmoutput.rdata")

jagsfit8 <- out8$model

names(jagsfit8)
length(which(jagsfit8$psrf$psrf[,1]>1.05))
jagsfit8$autocorr
jagsfit8$psrf$psrf[which(jagsfit8$psrf$psrf[,1]>1.05),1]
as.matrix(jagsfit8$psrf$psrf[which(jagsfit8$psrf$psrf[,1]>1.05),1])

# find whose psrf > 1.05
names <- colnames(as.matrix(jagsfit8$mcmc))
ind <- which(jagsfit8$psrf$psrf[,1]>1.05)
# ind1 <- which(names=="lr.mu[1]")
# ind2 <- which(names=="lr.mu[2]")

# plot (visualize)
densityplot(jagsfit8$mcmc[,ind[2]])
traceplot(jagsfit8$mcmc[,ind[2]])

# extract fitting parameters
## mean
head(jagsfit8$summary$statistics)
jagsfit8$summary$statistics[,1]
as.matrix(jagsfit8$summary$statistics[,1])

## median
head(jagsfit8$summary$quantiles)
jagsfit8$summary$quantiles[,3]
as.matrix(jagsfit8$summary$quantiles[,3])

## mean, median, and mode, by running combine.mcmc()
## 
mcmc8 <- combine.mcmc(jagsfit8$mcmc)
mean(as.matrix(mcmc8[,ind[2]]))         # mean
median(as.matrix(mcmc8[,ind[2]]))       # median
tmp <- mlv(as.matrix(mcmc8[,ind[2]]), method="density")
names(tmp)
tmp$M                                   # mode


## 
# a bit more
find which chain is not good (write a loop for plotting)
extand.jags(jagsfit8,drop.chain = )

badfits <- which(jagsfit8$psrf$psrf[,1]>1.05)
for(j in 1:length(badfits)) {
  densityplot(jagsfit8$mcmc[,badfits[j]])
}


# check the coda package for more details
# diff lags, 0 5 10 25 50 etc.
atcorr <- autocorr(jagsfit8$mcmc)
save(atcorr8,  file = 'atcorr8.rdata')





# NOTE: compute WAIC



## check deviance
out1$model$deviance.sum









































