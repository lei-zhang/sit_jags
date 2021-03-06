var v1[ns,nt+1,2], v2[ns,nt+1,2], valdiff[ns,nt,2], pa1[ns,nt,2], pa2[ns,nt,2], pbet1[ns,nt,3], pbet2[ns,nt,3], tmp1[ns,nt,2], tmp2[ns,nt,2], pe[ns,nt], penc[ns,nt];
model {
  
  for (s in 1:ns) # subject loop
    {
      for (i in 1:2) {
        lr[s,i] ~ dbeta(lr.a[i], lr.b[i]) T(0.001,0.999)
        tau[s,i] ~ dnorm(tau.mu[i], tau.lambda) T(-10,10)
      }
      #coha[s] ~ dbeta(coha.a,coha.b) T(0.001,0.999)
      #cohw[s] ~ dbeta(cohw.a,cohw.b) T(0.001,0.999)
      coha[s] ~ dnorm(coha.mu,coha.lambda) T(-2,2)
      cohw[s] ~ dnorm(cohw.mu,cohw.lambda) T(-2,2)

      temp.prime[s] ~ dbeta(temp.a, temp.b) T(0.001,0.999)
      temp[s] <- 20.0 * temp.prime[s]

      v1[s,1,1] <- 0
      v1[s,1,2] <- 0

	    for (t in 1:nt)  # trial loop 
	    { 
        # 1st choice
        pa1[s,t,1] <- exp(temp[s] * v1[s,t,1]) / (exp(temp[s] * v1[s,t,1]) + exp(temp[s] * v1[s,t,2]) + 1.0E-10)
		    pa1[s,t,2] <- exp(temp[s] * v1[s,t,2]) / (exp(temp[s] * v1[s,t,1]) + exp(temp[s] * v1[s,t,2]) + 1.0E-10)
        
        # 1st value difference: chosen value - nonchosen value
        valdiff[s,t,1] <- abs(v1[s,t,1] - v1[s,t,2])
        
        # softmax transform for the bet threshold: 1st bet
        tmp1[s,t,1] <- 1 / (1+exp(-(tau[s,1] - valdiff[s,t,1])) + 1.0E-10)
        tmp1[s,t,2] <- 1 / (1+exp(-(tau[s,2] - valdiff[s,t,1])) + 1.0E-10)

        pbet1[s,t,1] <- tmp1[s,t,1]
        pbet1[s,t,2] <- tmp1[s,t,2] - tmp1[s,t,1]
        pbet1[s,t,3] <- 1 - tmp1[s,t,2]

        # modify values for current trial according to coherence with or against the group
        v2[s,t,3-choice1[s,t]] <- v1[s,t,3-choice1[s,t]] + coha[s] * against[s,t]
        v2[s,t,choice1[s,t]]   <- v1[s,t,choice1[s,t]]   + cohw[s] * with[s,t]

		    # compute action probability from v2[s,t,]
		    pa2[s,t,1] <- exp(temp[s] * v2[s,t,1]) / (exp(temp[s] * v2[s,t,1]) + exp(temp[s] * v2[s,t,2]) + 1.0E-10)
		    pa2[s,t,2] <- exp(temp[s] * v2[s,t,2]) / (exp(temp[s] * v2[s,t,1]) + exp(temp[s] * v2[s,t,2]) + 1.0E-10)
        
        # 2nd value difference: chosen value - nonchosen value
        valdiff[s,t,2] <- abs(v2[s,t,1] - v2[s,t,2])

        # softmax transform for the bet threshold: 2nd bet
        tmp2[s,t,1] <- 1 / (1+exp(-(tau[s,1] - valdiff[s,t,2])) + 1.0E-10)
        tmp2[s,t,2] <- 1 / (1+exp(-(tau[s,2] - valdiff[s,t,2])) + 1.0E-10)

        pbet2[s,t,1] <- tmp2[s,t,1]
        pbet2[s,t,2] <- tmp2[s,t,2] - tmp2[s,t,1]
        pbet2[s,t,3] <- 1 - tmp2[s,t,2]

		    # prediction error
		    pe[s,t]   <- reward[s,t]  - v2[s,t,choice2[s,t]]
        penc[s,t] <- -reward[s,t] - v2[s,t,3-choice2[s,t]]

		    # learning (value update)
  	    v1[s,t+1,  choice2[s,t]] <- v2[s,t,  choice2[s,t]] + lr[s,1] * pe[s,t]
        v1[s,t+1,3-choice2[s,t]] <- v2[s,t,3-choice2[s,t]] + lr[s,2] * penc[s,t]


		    # relate data to model using a categorical distribution
  	    choice1[s,t] ~ dcat(pa1[s,t,])
		    choice2[s,t] ~ dcat(pa2[s,t,])
        bet1[s,t]    ~ dcat(pbet1[s,t,])
        bet2[s,t]    ~ dcat(pbet2[s,t,])

      }

    }
  
    # HYPERPARAMETERS
    for (i in 1:2) {
      lr.mu[i] ~ dbeta(1,1) T(0.001,0.999)
      lr.prime.kappa[i] ~ dbeta(1,1) T(0.001,0.999)
      lr.kappa[i] <- 3 / (lr.prime.kappa[i] * lr.prime.kappa[i]) - 1
      lr.a[i] <- lr.mu[i] * lr.kappa[i]
      lr.b[i] <- (1-lr.mu[i]) * lr.kappa[i]
    }

    # for Beta distribution
#     coha.mu ~ dbeta(1,1) T(0.001,0.999)
#     coha.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
#     coha.kappa <- 3 / (coha.prime.kappa * coha.prime.kappa) - 1
#     coha.a <- coha.mu * coha.kappa
#     coha.b <- (1-coha.mu) * coha.kappa
#     
#     cohw.mu ~ dbeta(1,1) T(0.001,0.999)
#     cohw.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
#     cohw.kappa <- 3 / (cohw.prime.kappa * cohw.prime.kappa) - 1
#     cohw.a <- cohw.mu * cohw.kappa
#     cohw.b <- (1-cohw.mu) * cohw.kappa

    # for Normal distribution
    coha.mu ~ dnorm(0,0.01) T(-2,2)
    coha.sigma ~ dunif(0,10)
    coha.lambda <- 1/pow(coha.sigma,2)
    cohw.mu ~ dnorm(0,0.01) T(-2,2)
    cohw.sigma ~ dunif(0,10)
    cohw.lambda <- 1/pow(cohw.sigma,2)

  
    # tau, for normal distribution
    for (i in 1:2) {
      tau0.mu[i] ~ dnorm(0,0.01) T(-10,10)
    }
    tau.mu <- sort(tau0.mu)
    tau.sigma ~ dunif(0,10)
    tau.lambda <- 1/pow(tau.sigma,2)
    
    # temp
    temp.prime.mu ~ dbeta(1,1) T(0.001,0.999)
    temp.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
    temp.kappa <- 3 / (temp.prime.kappa * temp.prime.kappa) - 1
    temp.a <- temp.prime.mu * temp.kappa
    temp.b <- (1-temp.prime.mu) * temp.kappa
    temp.mu <- 20 * temp.prime.mu

}