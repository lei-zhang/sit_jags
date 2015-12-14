simulate_model <- function(modelString) {

  library(runjags)
  
  load("sit_reversal_betnoshow.rdata")
  
  n.chains <- 3
  n.samples <- 20000
  n.burnin <- 1000
  n.thin <- 5
  
  nt <- as.vector(c(100,100,100,100,100,90,90,90,90,90))
  
  #  create dataList for JAGS
  if (modelString == "RevLearn_RL" || modelString == "RevLearn_RLnc" || modelString == "RevLearn_RLnc_cfa" ||
        modelString == "RevLearn_RLnc_2lr" || modelString == "RevLearn_RLnc_2lr_cfa") {
    dataList <- list()
    sz <- dim(data)
    ns <- sz[3]
    
    dataList$ns <- ns
    dataList$nt <- nt
    
    choice <- array(0,dim = c(ns,100))
    reward <- array(0,dim = c(ns,100))

    for (s in 1:ns) {
      choice[s,] <- data[,9,s]
      reward[s,] <- data[,14,s]
    }
    
    dataList$choice <- choice
    dataList$reward <- reward
    
  }
  if (modelString == "RevLearn_RLcoh" || modelString == "RevLearn_RLcoh_cfa" || 
      modelString == "RevLearn_RLcoh_2lr" || modelString == "RevLearn_RLcoh_2lr_cfa") {
    dataList <- list()
    sz <- dim(data)
    ns <- sz[3]
  
    dataList$ns <- ns
    dataList$nt <- nt
  
    choice1 <- array(0,dim = c(ns,100))
    choice2 <- array(0,dim = c(ns,100))
    reward <- array(0,dim = c(ns,100))
    with <- array(0,dim = c(ns,100))
    against <- array(0,dim = c(ns,100))
    my1 <- 0
    other1 <- c(0,0,0,0)
  
    for (s in 1:ns) {
      choice1[s,] <- data[,4,s]
      choice2[s,] <- data[,9,s]
      reward[s,] <- data[,14,s]
      for (t in 1:nt[s]) {
        my1 <- data[t,4,s]
        other1 <- data[t,5:8,s]
        with[s,t] <- length(which(other1==my1))
        against[s,t] <- length(which(other1!=my1))
      }
    }
    
    dataList$choice1 <- choice1
    dataList$choice2 <- choice2
    dataList$reward <- reward
    dataList$with   <- with
    dataList$against <- against
    
  }
  
  # inits
  initList <- get_inits(modelString,n.chains)
  
  # monitors
  monitorList <- get_monitors(modelString)
  
  # compute samples/chain
  nPerChain <-  ceiling( n.samples  / n.chains )
    
  # configure runjags and run simulation
  model <- run.jags(model=paste(modelString,".txt",sep=""),
                    monitor  = monitorList,
                    data     = dataList,
                    n.chains = n.chains,
                    inits    = initList,
                    burnin   = n.burnin,
                    sample   = nPerChain,
                    thin     = n.thin,
                    plots    = TRUE,
                    modules  = c("dic"),
                    summarise= TRUE,
                    silent.jags = FALSE,
                    keep.jags.files = FALSE,
                    method = "interruptible")

  return(list(model=model,name=modelString))
  
}

get_inits <- function(modelString,n.chains) {
  
  inits <- list()
  
  beta.prior.a <- 1
  beta.prior.b <- 1
  norm.prior.mu <- 0
  norm.prior.sigma <- 0.1
  unif.prior.lower <- 0
  unif.prior.upper <- 10
  
  for (c in 1:n.chains) {
    
    L <- list()
    
    if (modelString == "RevLearn_RL" || modelString == "RevLearn_RLnc") {      
      L$lr.mu            <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
    }
    if (modelString == "RevLearn_RLnc_2lr") {
      L$lr.mu            <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
    }
    if (modelString == "RevLearn_RLnc_cfa") {      
      L$lr.mu            <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.prime.kappa  <- sample_from_beta(1,beta.prior.a,beta.prior.b)
    }   
    if (modelString == "RevLearn_RLnc_2lr_cfa") { 
      L$lr.mu            <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.prime.kappa  <- sample_from_beta(1,beta.prior.a,beta.prior.b)
    }
    if (modelString == "RevLearn_RLcoh") {      
      L$lr.mu            <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$coha.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$coha.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.priorupper)      
      L$cohw.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cohw.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.priorupper)      
    }
    if (modelString == "RevLearn_RLcoh_2lr") {      
      L$lr.mu            <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$coha.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$coha.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.priorupper)      
      L$cohw.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cohw.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.priorupper)      
    }
    if (modelString == "RevLearn_RLcoh_2lr_cfa") {
      L$lr.mu            <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$coha.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$coha.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.priorupper)      
      L$cohw.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cohw.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.priorupper)
      L$cfa.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.prime.kappa  <- sample_from_beta(1,beta.prior.a,beta.prior.b)
    }
    if (modelString == "RevLearn_RLcoh_cfa") {
      L$lr.mu            <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.prime.kappa  <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$coha.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$coha.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.priorupper)      
      L$cohw.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cohw.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.priorupper)      
    }
    inits[[c]] <- L
  }
  return(inits)
}

get_monitors <- function(modelString) {  
  if (modelString == "RevLearn_RL" || modelString == "RevLearn_RLnc" || modelString == "RevLearn_RLnc_2lr") {
    monitorList <- c("deviance","dic","pd",
                     "lr.mu","lr.kappa","lr.a","lr.b",
                     "temp.mu","temp.kappa","temp.a","temp.b",
                     "lr","temp")
  }
  if (modelString == "RevLearn_RLnc_cfa" || modelString == "RevLearn_RLnc_2lr_cfa") {
    monitorList <- c("deviance","dic","pd",
                     "lr.mu","lr.kappa","lr.a","lr.b",
                     "temp.mu","temp.kappa","temp.a","temp.b",
                     "cfa.mu","cfa.kappa","cfa.a","cfa.b",
                     "lr","temp","cfa")
  }
  if (modelString == "RevLearn_RLcoh" || modelString == "RevLearn_RLcoh_2lr") {
    monitorList <- c("deviance","dic","pd",
                     "lr.mu","lr.kappa","lr.a","lr.b",
                     "temp.mu","temp.kappa","temp.a","temp.b",
                     "coha.mu","coha.sigma",
                     "cohw.mu","cohw.sigma",
                     "lr","temp","coha","cohw")
  }
  if (modelString == "RevLearn_RLcoh_cfa" || modelString == "RevLearn_RLcoh_2lr_cfa") {
    monitorList <- c("deviance","dic","pd",
                     "lr.mu","lr.kappa","lr.a","lr.b",
                     "temp.mu","temp.kappa","temp.a","temp.b",
                     "coha.mu","coha.sigma",
                     "cohw.mu","cohw.sigma",
                     "cfa.mu","cfa.kappa","cfa.a","cfa.b",
                     "lr","temp","coha","cohw","cfa")
  }
  return(monitorList)
}

sample_from_beta <- function(n,a,b) {
  out <- rbeta(n,a,b)
  for (i in 1:n) {
    if (out[i] < 0.001) out[i] <- 0.001
    if (out[i] > 0.999) out[i] <- 0.999
  }
  return(out)
}

sample_from_normal <- function(n,mu,sigma) {
  out <- sample_from_normal(n,mu,sigma)
  for (i in 1:n) {
    if (out[i] < -1) out[i] <- -1
    if (out[i] >  1) out[i] <- 1
  }
  return(out)
}

sample_from_uniform <- function(n,lower,upper) {
  return(out<-runif(n,lower,upper))
}