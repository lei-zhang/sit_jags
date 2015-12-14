names(lst1)
lst1$name
names(lst1$model)
lst1$model$dic
lst1$model$psrf
lst1$model$psrf$psrf[which(lst1$model$psrf$psrf[,1]>1.05),1]
as.matrix(lst1$model$psrf$psrf[which(lst1$model$psrf$psrf[,1]>1.05),1])

names <- colnames(as.matrix(lst1$model$mcmc))
which(names=="lr.mu[1]")
which(names=="lr.mu[2]")
which(names=="lr.kappa[1]")
which(names=="lr.kappa[2]")
library(lattice)
densityplot(lst1$model$mcmc[,268])
library(coda)
traceplot(lst1$model$mcmc[,268])

lst1$model$autocorr
lst1$model$thin
as.matrix(lst1$model$psrf$psrf[which(lst1$model$psrf$psrf[,1]>1.05),1])
lst1$model$dic
densityplot(lst1$model$mcmc[,268])
lst1$model$summary$statistics
lst1$model$summary$statistics[,1]
lst1$model$summary$quantiles

mcmc <- combine.mcmc(lst1$model$mcmc)
mean(as.matrix(mcmc[,268]))
median(as.matrix(mcmc[,268]))
library(modeest)
tmp <- mlv(as.matrix(mcmc[,268]), method="density")
names(tmp)
tmp$M





