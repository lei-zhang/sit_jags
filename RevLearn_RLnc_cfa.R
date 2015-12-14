var v[ns,nt+1,2], pa[ns,nt,2], pe[ns,100] penc[ns,100];
model {
  
  for (s in 1:ns) # subject loop
    {
  	  
      lr[s] ~ dbeta(lr.a, lr.b) T(0.001,0.999)
      cfa[s] ~ dbeta(cfa.a,cfa.b) T(0.001,0.999)
      temp.prime[s] ~ dbeta(temp.a, temp.b) T(0.001,0.999)
      temp[s] <- 20.0 * temp.prime[s]

      v[s,1,1] <- 0
	    v[s,1,2] <- 0

	    for (t in 1:nt)  # trial loop 
	    {
			
		    # compute action probability from v[s,t,]
		    pa[s,t,1] <- exp(temp[s] * v[s,t,1]) / (exp(temp[s] * v[s,t,1]) + exp(temp[s] * v[s,t,2]) + 1.0E-10)
		    pa[s,t,2] <- exp(temp[s] * v[s,t,2]) / (exp(temp[s] * v[s,t,1]) + exp(temp[s] * v[s,t,2]) + 1.0E-10)

		    # prediction error
		    pe[s,t] <- reward[s,t] - v[s,t,choice[s,t]]
        penc[s,t] <- (cfa[s] * -reward[s,t]) - v[s,t,3-choice[s,t]]

		    # learning (value update)
		    v[s,t+1,3-choice[s,t]] <- v[s,t,3-choice[s,t]] + lr[s] * penc[s,t]
		    v[s,t+1,  choice[s,t]] <- v[s,t,  choice[s,t]] + lr[s] * pe[s,t]

		    # relate data to model using a categorical distribution
		    choice[s,t] ~ dcat(pa[s,t,])

      }

    }
  
    # HYPERPARAMETERS
    lr.mu ~ dbeta(1,1) T(0.001,0.999)
    lr.prime.kappa ~ dbeta(1,1) T(0.001,0.999)
    lr.kappa <- 3 / (lr.prime.kappa * lr.prime.kappa) - 1
    lr.a <- lr.mu * lr.kappa
    lr.b <- (1-lr.mu) * lr.kappa

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