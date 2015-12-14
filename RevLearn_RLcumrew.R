var v[ns,nt,2], pa[ns,nt,2], pe[ns,nt], penc[ns,nt], vtmp[ns,nt+1,2], wgtrew[ns,nt,4,nt], cumwgtrew[ns,nt,4], cumrew[ns,nt,4], cumrew_ini[ns,1], sum_with_each[ns,nt,4], sum_against_each[ns,nt,4];
model {
  
  for (s in 1:ns) # subject loop
  {
    lr[s]   ~ dbeta(lr.a, lr.b) T(0.001,0.999)
#     cra[s]  ~ dbeta(cra.a,cra.b) T(0.001,0.999)
#     crw[s]  ~ dbeta(crw.a,crw.b) T(0.001,0.999)
    cra[s]  ~ dnorm(cra.mu,cra.lambda) T(-2,2)
    crw[s]  ~ dnorm(crw.mu,crw.lambda) T(-2,5) #T(-2,2)
    disc[s] ~ dbeta(disc.a,disc.b) T(0.001,0.999)
    temp.prime[s] ~ dbeta(temp.a, temp.b) T(0.001,0.999)
    temp[s] <- 20.0 * temp.prime[s]
    
    cumrew_ini[s,1] <- 0.25
    vtmp[s,1,1]     <- 0 # value for the 1st trial 
    vtmp[s,1,2]     <- 0 # value for the 2nd trial
    
    for (t in 1:nt)  # trial loop 
    {
      # compute sum of weighted cumulative reward
      for (o1 in 1:4) {       # o: index of the 'other' 4 players  
        wgtrew[s,t,o1,1]  <- 0
        
        for (ct in 2:t ) {   # ct: index of the current trial
          # wgtrew : weighted reward
          wgtrew[s,t,o1,ct]      <- (disc[s] ^(t-ct+1)) * otherReward[s,ct-1,o1]
        }
        cumwgtrew[s,t,o1]        <- sum(wgtrew[s,t,o1,1:t] )
      }

      for (o2 in 1:4) {
        cumrew[s,t,o2]           <- (t<=2)*(cumrew_ini[s,1]) + (t>2)* (exp(cumwgtrew[s,t,o2]) /
                                             (exp(cumwgtrew[s,t,1])+exp(cumwgtrew[s,t,2])+exp(cumwgtrew[s,t,3])+exp(cumwgtrew[s,t,4])+ 1.0E-10) ) # normalise
        sum_with_each[s,t,o2]    <- otherChoice[s,t,o2] == choice1[s,t]
        sum_against_each[s,t,o2] <- otherChoice[s,t,o2] != choice1[s,t]
        cumrew_with[s,t,o2]      <- sum_with_each[s,t,o2]    * cumrew[s,t,o2]
        cumrew_against[s,t,o2]   <- sum_against_each[s,t,o2] * cumrew[s,t,o2]
      }
      
      
#       sum_with[s,t]    <- sum(sum_with_each[s,t,])
#       sum_against[s,t] <- sum(sum_against_each[s,t,])
      
      # modify values for current trial according to coherence with or against the group
      v[s,t,3-choice1[s,t]]  <- vtmp[s,t,3-choice1[s,t]] + cra[s] * sum(cumrew_against[s,t,])
      v[s,t,choice1[s,t]]    <- vtmp[s,t,choice1[s,t]]   + crw[s] * sum(cumrew_with[s,t,])

#       v[s,t,3-choice1[s,t]]  <- (sum_against >sum_with) * (vtmp[s,t,3-choice1[s,t]] + cra[s] * sum(cumrew_against[s,t,])) +
#                                 (sum_against<=sum_with) * (vtmp[s,t,3-choice1[s,t]] - cra[s] * sum(cumrew_against[s,t,]))
#       v[s,t,choice1[s,t]]    <- (sum_with>=sum_against) * (vtmp[s,t,choice1[s,t]] + crw[s]   * sum(cumrew_with[s,t,])) + 
#                                 (sum_with< sum_against) * (vtmp[s,t,choice1[s,t]] - crw[s]   * sum(cumrew_with[s,t,]))
       
      # compute action probability from v[s,t,]
      pa[s,t,1] <- exp(temp[s] * v[s,t,1]) / (exp(temp[s] * v[s,t,1]) + exp(temp[s] * v[s,t,2]) + 1.0E-10)
      pa[s,t,2] <- exp(temp[s] * v[s,t,2]) / (exp(temp[s] * v[s,t,1]) + exp(temp[s] * v[s,t,2]) + 1.0E-10)
      
      # prediction error
      pe[s,t]   <-  reward[s,t]  - v[s,t,choice2[s,t]]
      penc[s,t] <- -reward[s,t]  - v[s,t,3-choice2[s,t]]
      
      # learning (value update)
      vtmp[s,t+1,3-choice2[s,t]] <- v[s,t,3-choice2[s,t]] + lr[s] * penc[s,t]
      vtmp[s,t+1,  choice2[s,t]] <- v[s,t,  choice2[s,t]] + lr[s] * pe[s,t]
      
      # relate data to model using a categorical distribution
      choice2[s,t] ~ dcat(pa[s,t,])
      
    }
    
  }
  
  # HYPERPARAMETERS
  lr.mu ~ dbeta(1,1) T(0.001,0.999)
  lr.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
  lr.kappa <- 3 / (lr.prime.kappa * lr.prime.kappa) - 1
  lr.a <- lr.mu * lr.kappa
  lr.b <- (1-lr.mu) * lr.kappa
  
  # for Beta distribution
#   cra.mu ~ dbeta(1,1) T(0.001,0.999)
#   cra.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
#   cra.kappa <- 3 / (cra.prime.kappa * cra.prime.kappa) - 1
#   cra.a <- cra.mu * cra.kappa
#   cra.b <- (1-cra.mu) * cra.kappa
#   crw.mu ~ dbeta(1,1) T(0.001,0.999)
#   crw.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
#   crw.kappa <- 3 / (crw.prime.kappa * crw.prime.kappa) - 1
#   crw.a <- crw.mu * crw.kappa
#   crw.b <- (1-crw.mu) * crw.kappa

  # for Normal distribution
  cra.mu ~ dnorm(0,0.01) T(-2,2)
  cra.sigma ~ dunif(0,10)
  cra.lambda <- 1/pow(cra.sigma,2)
  crw.mu ~ dnorm(0,0.01) T(-2,5) #T(-2,2)
  crw.sigma ~ dunif(0,10)
  crw.lambda <- 1/pow(crw.sigma,2)

  
  disc.mu ~ dbeta(1,1) T(0.001,0.999)
  disc.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
  disc.kappa <- 3 / (disc.prime.kappa * disc.prime.kappa) - 1
  disc.a <- disc.mu * disc.kappa
  disc.b <- (1-disc.mu) * disc.kappa
  
  temp.prime.mu ~ dbeta(1,1) T(0.001,0.999)
  temp.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
  temp.kappa <- 3 / (temp.prime.kappa * temp.prime.kappa) - 1
  temp.a <- temp.prime.mu * temp.kappa
  temp.b <- (1-temp.prime.mu) * temp.kappa
  temp.mu <- 20 * temp.prime.mu
  
}