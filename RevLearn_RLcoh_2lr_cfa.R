var v[ns,nt+1,2], tmp[ns,nt+1,2], pa2[ns,nt,2], pe[ns,nt], penc[ns,nt];
model {
  
  for (s in 1:ns) # subject loop
    {
      for (i in 1:2) {
        lr[s,i] ~ dbeta(lr.a[i], lr.b[i]) T(0.001,0.999)
      }
      cfa[s] ~ dbeta(cfa.a,cfa.b) T(0.001,0.999)
      #coha[s] ~ dbeta(coha.a,coha.b) T(0.001,0.999)
      #cohw[s] ~ dbeta(cohw.a,cohw.b) T(0.001,0.999)
      coha[s] ~ dnorm(coha.mu,coha.lambda) T(-2,2)
      cohw[s] ~ dnorm(cohw.mu,cohw.lambda) T(-2,5) #T(-2,2)

      temp.prime[s] ~ dbeta(temp.a, temp.b) T(0.001,0.999)
      temp[s] <- 20.0 * temp.prime[s]

      tmp[s,1,1] <- 0
	    tmp[s,1,2] <- 0

	    for (t in 1:nt)  # trial loop 
	    {
	      # 1st choice
# 	      pa1[s,t,1] <- exp(temp[s] * tmp[s,t,1]) / (exp(temp[s] * tmp[s,t,1]) + exp(temp[s] * tmp[s,t,2]) + 1.0E-10)
# 	      pa1[s,t,2] <- exp(temp[s] * tmp[s,t,2]) / (exp(temp[s] * tmp[s,t,1]) + exp(temp[s] * tmp[s,t,2]) + 1.0E-10)
        
        # modify values for current trial according to coherence with or against the group
        v[s,t,3-choice1[s,t]] <- tmp[s,t,3-choice1[s,t]] + coha[s] * against[s,t]
        v[s,t,choice1[s,t]]   <- tmp[s,t,choice1[s,t]]   + cohw[s] * with[s,t]
        
		    # compute action probability from v[s,t,]
		    pa2[s,t,1] <- exp(temp[s] * v[s,t,1]) / (exp(temp[s] * v[s,t,1]) + exp(temp[s] * v[s,t,2]) + 1.0E-10)
		    pa2[s,t,2] <- exp(temp[s] * v[s,t,2]) / (exp(temp[s] * v[s,t,1]) + exp(temp[s] * v[s,t,2]) + 1.0E-10)

		    # prediction error
		    pe[s,t] <- reward[s,t] - v[s,t,choice2[s,t]]
        penc[s,t] <- (cfa[s] * -reward[s,t]) - v[s,t,3-choice2[s,t]]

		    # learning (value update)
		    tmp[s,t+1,3-choice2[s,t]] <- v[s,t,3-choice2[s,t]] + lr[s,2] * penc[s,t]
		    tmp[s,t+1,  choice2[s,t]] <- v[s,t,  choice2[s,t]] + lr[s,1] * pe[s,t]

		    # relate data to model using a categorical distribution
# 		    choice1[s,t] ~ dcat(pa1[s,t,])
        choice2[s,t] ~ dcat(pa2[s,t,])

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
#     cohw.mu ~ dbeta(1,1) T(0.001,0.999)
#     cohw.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
#     cohw.kappa <- 3 / (cohw.prime.kappa * cohw.prime.kappa) - 1
#     cohw.a <- cohw.mu * cohw.kappa
#     cohw.b <- (1-cohw.mu) * cohw.kappa

    # for Normal distribution
    coha.mu ~ dnorm(0,0.01) T(-2,2)
    coha.sigma ~ dunif(0,10)
    coha.lambda <- 1/pow(coha.sigma,2)
    cohw.mu ~ dnorm(0,0.01) T(-2,5) #T(-2,2)
    cohw.sigma ~ dunif(0,10)
    cohw.lambda <- 1/pow(cohw.sigma,2)


    cfa.mu ~ dbeta(1,1) T(0.001,0.999)
    cfa.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
    cfa.kappa <- 3 / (cfa.prime.kappa * cfa.prime.kappa) - 1
    cfa.a <- cfa.mu * cfa.kappa
    cfa.b <- (1-cfa.mu) * cfa.kappa

    temp.prime.mu ~ dbeta(1,1) T(0.001,0.999)
    temp.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
    temp.kappa <- 3 / (temp.prime.kappa * temp.prime.kappa) - 1
    temp.a <- temp.prime.mu * temp.kappa
    temp.b <- (1-temp.prime.mu) * temp.kappa
    temp.mu <- 20 * temp.prime.mu

}