simulate_model <- function(modelString, sys) {

  if ( sys == 0 ){
    setwd("F:/projects/SocialInflu/sit_jags")        # WINDOWS  sys = 0
  } else {
    setwd("/projects/crunchie/zhang/sit/sit_jags")   # LINUX    sys = 1
  }
  
  library(runjags)
  library(rjags)
  
  # path  <- 'F:/projects/SocialInflu/data_MR/data3.mat'
  # data <- readMat(path)
  # data <- data$data3
  # save(data, file = 'sit_reversal_betnoshow.rdata')
  
  load("_data/sit_reversal_betnoshow.rdata")
  
  n.chains  <- 3
  n.samples <- 12000  # 30000  # 20000
  n.burnin  <- 2000  #5000   # 2000
  n.thin    <- 5
  
  nt <- 100  # nTrials
  
  #  create dataList for JAGS #
  ## model only the 2nd choice ####
  if (modelString == "RevLearn_RL" || modelString == "RevLearn_RLnc" || modelString == "RevLearn_RLnc_cfa" || 
        modelString == "RevLearn_RLnc_2lr" || modelString == "RevLearn_RLnc_2lr_cfa") {
    dataList <- list()
    sz <- dim(data) # size
    ns <- sz[3]     # No. of subjects
    
    dataList$ns <- ns
    dataList$nt <- nt
    
    choice <- array(0,dim = c(ns,nt))
    reward <- array(0,dim = c(ns,nt))

    for (s in 1:ns) {
      choice[s,] <- data[,10,s]
      reward[s,] <- data[,14,s] # 1 OR -1
    }
    
    dataList$choice <- choice
    dataList$reward <- reward
    
  }
  
  ## RLcoh, model both 1st and 2nd choice ##
  if (modelString == "RevLearn_RLcoh" || modelString == "RevLearn_RLcoh_cfa" || 
      modelString == "RevLearn_RLcoh_2lr" || modelString == "RevLearn_RLcoh_2lr_cfa" ||
      modelString == "RevLearn_RLcoh_2lr_bet") {
    dataList <- list()
    sz <- dim(data) # size
    ns <- sz[3]     # No. of subjects
  
    dataList$ns <- ns
    dataList$nt <- nt
  
    choice1 <- array(0,dim = c(ns,nt))
    choice2 <- array(0,dim = c(ns,nt))
    reward  <- array(0,dim = c(ns,nt))
    with    <- array(0,dim = c(ns,nt))
    against <- array(0,dim = c(ns,nt))
    my1     <- 0
    other1  <- c(0,0,0,0)

    if (modelString == "RevLearn_RLcoh_2lr_bet") {
      bet1    <- array(0,dim = c(ns,nt )) 
      bet2    <- array(0,dim = c(ns,nt )) 
    }
      
    for (s in 1:ns) {
      choice1[s,] <- data[, 3,s]
      choice2[s,] <- data[,10,s]
      reward[s,]  <- data[,14,s]
      
      if (modelString == "RevLearn_RLcoh_2lr_bet") {
        bet1[s,]    <- data[,13,s]
        bet2[s,]    <- data[,19,s]
      }
      
      for (tr in 1:nt) {
        my1    <- data[tr,3,s]
        other1 <- data[tr,6:9,s]
        with[s,tr]    <- length(which(other1==my1)) /4  # count of with, either 1, 2, 3, or 4, divided by 4
        against[s,tr] <- length(which(other1!=my1)) /4  # count of against, either 1, 2, 3, or 4, divided by4 
      }
    }
    
    dataList$choice1 <- choice1
    dataList$choice2 <- choice2
    dataList$reward  <- reward
    dataList$with    <- with
    dataList$against <- against
    
    if (modelString == "RevLearn_RLcoh_2lr_bet") {
      dataList$bet1    <- bet1
      dataList$bet2    <- bet2
    }
    
  }
  
  
  ## RLcumrew, model both 1st and 2nd choice ##
  if (modelString == "RevLearn_RLcumrew" || modelString == "RevLearn_RLcumrew_cfa" || 
        modelString == "RevLearn_RLcumrew_2lr" || modelString == "RevLearn_RLcumrew_2lr_cfa") {
    dataList <- list()
    sz <- dim(data) # size
    ns <- sz[3]     # No. of subjects
    
    dataList$ns <- ns
    dataList$nt <- nt
    
    choice1     <- array(0,dim = c(ns,nt))
    choice2     <- array(0,dim = c(ns,nt))
    reward      <- array(0,dim = c(ns,nt))
    otherChoice <- array(0,dim = c(ns,nt,4))  # others' choices
    otherReward <- array(0,dim = c(ns,nt,4))  # others' reward                  
    
    for (s in 1:ns) {
      choice1[s,]      <- data[, 3,s]
      choice2[s,]      <- data[,10,s]
      reward[s,]       <- data[,14,s]
      otherChoice[s,,] <- data[,6:9,s]
      otherReward[s,,] <- data[,24:27,s]
    }
    
    dataList$choice1     <- choice1
    dataList$choice2     <- choice2
    dataList$reward      <- reward
    dataList$otherChoice <- otherChoice
    dataList$otherReward <- otherReward
    
  }
  
  
  # inits
  initList <- get_inits(modelString, n.chains)    # function defined below
  
  # monitors
  monitorList <- get_monitors(modelString)       # function defined below
  
  # compute samples per chain
  nPerChain <- ceiling( n.samples  / n.chains )
    
  # CORE: configure runjags and run simulation
  model <- run.jags(model=paste("_scripts/", modelString,".R",sep=""),
                    monitor   = monitorList,
                    data      = dataList,
                    n.chains  = n.chains,
                    inits     = initList,
                    burnin    = n.burnin,
                    sample    = nPerChain,
                    thin      = n.thin,
                    plots     = TRUE,
                    method    = "interruptible", # "interruptible" OR "parallel" (if so, without 'dic' and 'pd')
                    modules   = c("dic"),        # c("dic") OR c("glm")
                    summarise = TRUE,
                    silent.jags = FALSE,
                    keep.jags.files = paste0("_temp/", modelString, "/tmp_files"), # T OR F OR a directory 
                    tempdir   = TRUE)

  # return the output of the main function
  return(list(model=model,name=modelString))
  
}


# nested functions #
## get initial parameters for each model per chain ##
get_inits <- function(modelString, n.chains) {
  
  inits <- list()
  
  beta.prior.a     <- 1
  beta.prior.b     <- 1
  norm.prior.mu    <- 0
  norm.prior.sigma <- 0.1 # is it too small?
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
#       L$coha.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$coha.prime.kappa <-  sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cohw.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cohw.prime.kappa <-  sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$coha.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$coha.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)      
      L$cohw.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cohw.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)      
    }
    if (modelString == "RevLearn_RLcoh_2lr") {      
      L$lr.mu            <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$coha.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$coha.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cohw.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cohw.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)   
      L$coha.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$coha.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)      
      L$cohw.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cohw.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)  
    }
    if (modelString == "RevLearn_RLcoh_2lr_cfa") {
      L$lr.mu            <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$coha.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$coha.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)      
      L$cohw.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cohw.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)  
#       L$coha.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$coha.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cohw.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cohw.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
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
      L$coha.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)      
      L$cohw.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cohw.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)  
#       L$coha.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$coha.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cohw.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cohw.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)   
    }
    if (modelString == "RevLearn_RLcoh_2lr_bet") {
      L$lr.mu            <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$coha.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$coha.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)      
      L$cohw.mu          <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cohw.sigma       <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)  
#       L$coha.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$coha.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cohw.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cohw.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$tau0.mu          <- sample_from_normal(2,norm.prior.mu,norm.prior.sigma)
      L$tau.sigma        <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)
    }
    if (modelString == "RevLearn_RLcumrew") {      
      L$lr.mu            <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cra.mu           <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cra.sigma        <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)      
      L$crw.mu           <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$crw.sigma        <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)   
#       L$cra.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cra.prime.kappa  <-  sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$crw.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$crw.prime.kappa  <-  sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$disc.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$disc.prime.kappa <-  sample_from_beta(1,beta.prior.a,beta.prior.b)      
    }
    if (modelString == "RevLearn_RLcumrew_2lr") {      
      L$lr.mu            <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cra.mu           <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cra.sigma        <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)      
      L$crw.mu           <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$crw.sigma        <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)  
#       L$cra.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cra.prime.kappa  <-  sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$crw.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$crw.prime.kappa  <-  sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$disc.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$disc.prime.kappa <-  sample_from_beta(1,beta.prior.a,beta.prior.b)      
    }
    if (modelString == "RevLearn_RLcumrew_cfa") {      
      L$lr.mu            <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.prime.kappa  <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cra.mu           <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cra.sigma        <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)      
      L$crw.mu           <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$crw.sigma        <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)  
#       L$cra.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cra.prime.kappa  <-  sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$crw.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$crw.prime.kappa  <-  sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$disc.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$disc.prime.kappa <-  sample_from_beta(1,beta.prior.a,beta.prior.b)      
    }
    if (modelString == "RevLearn_RLcumrew_2lr_cfa") {      
      L$lr.mu            <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$lr.prime.kappa   <- sample_from_beta(2,beta.prior.a,beta.prior.b)
      L$temp.prime.mu    <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$temp.prime.kappa <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cfa.prime.kappa  <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$cra.mu           <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$cra.sigma        <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)      
      L$crw.mu           <- sample_from_normal(1,norm.prior.mu,norm.prior.sigma)
      L$crw.sigma        <- sample_from_uniform(1,unif.prior.lower,unif.prior.upper)  
#       L$cra.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$cra.prime.kappa  <-  sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$crw.mu           <- sample_from_beta(1,beta.prior.a,beta.prior.b)
#       L$crw.prime.kappa  <-  sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$disc.mu          <- sample_from_beta(1,beta.prior.a,beta.prior.b)
      L$disc.prime.kappa <-  sample_from_beta(1,beta.prior.a,beta.prior.b)      
    }
    
    inits[[c]] <- L
  }
  return(inits)
}


get_monitors <- function(modelString) {  
  
  monitorList <- list()
  
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
#                      "coha.mu","coha.kappa","coha.a","coha.b",
#                      "cohw.mu","cohw.kappa","cohw.a","cohw.b",
                     "coha.mu","coha.sigma",
                     "cohw.mu","cohw.sigma",
                     "lr","temp","coha","cohw")
  }
  if (modelString == "RevLearn_RLcoh_cfa" || modelString == "RevLearn_RLcoh_2lr_cfa") {
    monitorList <- c("deviance","dic","pd",
                     "lr.mu","lr.kappa","lr.a","lr.b",
                     "temp.mu","temp.kappa","temp.a","temp.b",
#                      "coha.mu","coha.kappa","coha.a","coha.b",
#                      "cohw.mu","cohw.kappa","cohw.a","cohw.b",
                     "coha.mu","coha.sigma",
                     "cohw.mu","cohw.sigma",
                     "cfa.mu","cfa.kappa","cfa.a","cfa.b",
                     "lr","temp","coha","cohw","cfa")
  }
  if (modelString == "RevLearn_RLcoh_2lr_bet") {
    monitorList <- c("deviance", "dic","pd",
                     "lr.mu","lr.kappa","lr.a","lr.b",
                     "temp.mu","temp.kappa","temp.a","temp.b",
#                      "coha.mu","coha.kappa","coha.a","coha.b",
#                      "cohw.mu","cohw.kappa","cohw.a","cohw.b",
                     "coha.mu","coha.sigma",
                     "cohw.mu","cohw.sigma",
                     "tau.mu","tau.sigma",
                     "lr","temp","coha","cohw","tau")
  }
  if (modelString == "RevLearn_RLcumrew" || modelString == "RevLearn_RLcumrew_2lr") {
    monitorList <- c("deviance","dic","pd",
                     "lr.mu","lr.kappa","lr.a","lr.b",
                     "temp.mu","temp.kappa","temp.a","temp.b",
#                      "cra.mu","cra.kappa","cra.a","cra.b",
#                      "crw.mu","crw.kappa","crw.a","crw.b",
                     "cra.mu","cra.sigma",
                     "crw.mu","crw.sigma",
                     "disc.mu","disc.kappa","disc.a","disc.b",
                     "lr","temp","cra","crw","disc")
  }
  if (modelString == "RevLearn_RLcumrew_cfa" || modelString == "RevLearn_RLcumrew_2lr_cfa") {
    monitorList <- c("deviance","dic","pd",
                     "lr.mu","lr.kappa","lr.a","lr.b",
                     "temp.mu","temp.kappa","temp.a","temp.b",
#                      "cra.mu","cra.kappa","cra.a","cra.b",
#                      "crw.mu","crw.kappa","crw.a","crw.b",
                     "cra.mu","cra.sigma",
                     "crw.mu","crw.sigma",
                     "disc.mu","disc.kappa","disc.a","disc.b",
                     "cfa.mu","cfa.kappa","cfa.a","cfa.b",
                     "lr","temp","cra","crw","disc","cfa")
  }
  
  return(monitorList)
}


# nested sample functions #
sample_from_beta <- function(n,a,b) {
  out <- rbeta(n,a,b)
  for (i in 1:n) { # retain between 0.001 and 0.999
    if (out[i] < 0.001) out[i] <- 0.001
    if (out[i] > 0.999) out[i] <- 0.999
  }
  return(out)
}

sample_from_normal <- function(n,mu,sigma) {
  out <- rnorm(n,mu,sigma) 
  for (i in 1:n) { # retain between -1 and 1
    if (out[i] < -1) out[i] <- -1 
    if (out[i] >  1) out[i] <-  1
  }
  return(out)
}

sample_from_uniform <- function(n,lower,upper) {
  return(out<-runif(n,lower,upper))
}