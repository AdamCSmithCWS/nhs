############################### model

## w[p,s,y] = array of wing counts by periods (dim1) species (dim2) and years (dim3)
###### see below for alternate w[p,s,y,h] - hunter-specific data
#### hunter specific data would be better because it would better account for the uncertainty among hunters in the species composition
#### currently, this assumes that the selection of hunters in a given zone and year is representative
#### this over-estimates the certainty of the annual estimates of composition, and downweights the influence of the normalizing prior
## nspecies
## nyears
## nperiods (weeks?)
## castes = vector of 1:4? assuming there are 4 castes
## nhs = number of hunter survey responses
## kill[i] = total duck (or other) harvest in a season for hunter i
## year[i] = year of hunter i response to survey
## caste[i] = caste of hunter i
###########################
###########################
## periodkill[p,y,h] = total kill in period-x and year-y for each hunter-h
## nhunters[y] = number of hunters with calendar information in year-y


model {
  
  ### all species harvest
  for(i in 1:nhs){
    
    ### active hunters
    pactive[i] ~ dbern(psi) #probability a hunter is active
    ### successful hunters
    
    ### number harvested
    kill[i] ~ dpois(lambda[i]) #kill is data - each hunter's estimate of the total number of ducks killed 
    elambda1[i] <- ann[year[i]] + cst[caste[i]] + hunter[i]
    elambda[i] <- elambda1[i]*pactive[i]
    log(lambda[i]) <- elambda[i]
    ## ann[y] is a yearly intercept for all species kill
    ## cst[c] is a caste-specific intercept for all species kill
    ## hunter[i] is an individual random error term to allow for overdispersion,
    ##  t-distribution used for overdispersion to allow for heavy-tailed 
    ## tauhunter[c] is a caste-specific precision to allow hunter-level dispersion to vary among castes
    ## consider whether tauhunter should also vary among years...
    
    hunter[i] ~ dt(0,tauhunter[caste[i]],nu[caste[i]])
  }#i
  
  for(c in castes){
    ## dispersion priors
    sdhunter[c] <- 1/pow(tauhunter[c],0.5)
    tauhunter[c] ~ dgamma(0.01,0.01)
    nu[c] ~ dgamma(2,0.1)
    ## caste specific intercept priors
    cst[c] ~ dnorm(0,0.001)
    
  }#c
  
  ### yearly intercepts of total kill by first-difference
  ann[1] ~ dnorm(0,tauyear.eps)
  for(y in 2:nyears){
    ann[y] ~ dnorm(ann[y-1],tauyear)
    
  }
  
  # year priors
  sdyear <- 1/pow(tauyear,0.5)
  tauyear ~ dgamma(0.001,0.001)
  tauyear.eps <- tauyear*0.001
  
  
  ##################################
  ### derived estimates of per-hunter species kill in each year###############
  
  for(c in castes){
    for(y in 1:nyears){
      ## derived estimated means, which can then be multiplied by the extrapolation factors for each caste and year
      # estimate of the mean (per hunter) kill per caste, and year
      for(h in shuntercastes[c]:ehuntercastes[c]){
        totkillh[y,c,h] <- exp(ann[y] + cst[c] + hunter[h])
      }
      totkill[y,c] <- mean(totkillh[y,c,shuntercastes[c]:ehuntercastes[c]])
      for(p in 1:nperiods){
        ## estimate of the mean (per hunter) kill per period, caste, and year
        killpc[p,c,y] <-  ptotkill[p,y]*totkill[y,c]
        
        for(s in 1:nspecies){
          # estimate of the mean (per hunter) kill, by period, caste, year, and species
          spkillperhunter[p,c,y,s] <- killpc[p,c,y] * pr[p,s,y]
          #
        }#s
      }#p
    }#y
  }#c
  
  
  
  #total harvest estimate by species year and caste
  ## add in the estimated populations of each caste to generate final harvest estimates.
  for(y in 1:nyears){
    for(c in castes){
      for(s in 1:nspecies){
  spkill[c,y,s] <- sum(spkillperhunter[1:nperiods,c,y,s])*pops[c,y]
      }
    }
  }
  
  ### estimating the proportional-seasonal distribution of total harvest by periods
  ## periodkill[p,y,h] is data, the hunter-level total harvest by period from the calendars
  ## nkill[y,h] is also data, hunter-level total estimate of their harvest
  ### assumes that seasonal distribution is the same across castes...questionable...
  
  
  ### multinomial distribution across periods    
  for (y in 1:nyears) {
    for(h in 1:nhunters[y]){
      periodkill[1:nperiods,y,h] ~ dmulti(ptotkill[1:nperiods,y], nkill[y,h])   # multinom distr vector responses
      
      
    }#h
  }#y
  
  ##### ptotkill[p,y] is the estimated proportion of the total duck harvest in year-y that occurred in period-p
  ##### this submodel shrinks that proportion towards the mean proportion across all years
  ##### using the kappat weight and the gamma distribution below
  for (p in 1:nperiods){
    for (y in 1:nyears){
      ptotkill[p,y] <- deltat[p,y] / sum(deltat[1:nperiods,y])
      
      
      deltat[p,y] ~ dgamma(alphat[p,y], 1)
      alphat[p,y] <- mut[p]*kappat ### mut[p] is the mean proportion of the hunt occurring in period p across all years
      
      
      
    } #y
    
  }#p
  
  
  #### hyperparameter mut[p] shrinking each period's estimate
  #### towards the long-term mean proportion of the hunt occurring within that period
  # logistic transformation
  for(p in 1:nperiods){
    mut[p] <- exp.termt[p]/sum(exp.termt[1:nperiods])
    exp.termt[p] <- exp(alphabt[p])
    
    
    
  }#p
  
  alphabt[1] <- 0 ## these are the log-scale intercepts of the proportions of the hunt in each period
  
  for(p in 2:nperiods){
    alphabt[p] ~ dnorm(0,0.001)
    
  }#s
  
  
  
  kappat ~ dgamma(Skappa,Rkappa) 
  
  ### Skappa and Rkappa are defined below
  
  
  
  
  
  
  ######## this model currently doesn't use the variation among hunters in the species distribution
  ### but perhaps it should, commented-out section that follows suggests how it should work
  ##### species composition
  #     for (p in 1:nperiods){
  # 
  # for (y in 1:nyears) {
  #   for(h in 1:nhuntscs[y]){
  # w[p,1:nspecies,y,h] ~ dmulti(pr[p,1:nspecies,y], n[p,y,h])   # multinom distr vector responses
  # 
  # n[p,y,h] <- sum(w[p,1:nspecies,y,h])# yearl and period sums of all parts
  #   }#h
  # }#y
  
  ##### species composition
  for (p in 1:nperiods){
    
    for (y in 1:nyears) {
      w[p,1:nspecies,y] ~ dmulti(pr[p,1:nspecies,y], n[p,y])   # multinom distr vector responses
      
    }
    
    ##### pr[p,s,y] is the estimated proportion of the total duck harvest in year-y, period-p, that is species-s
    
    for (s in 1:nspecies){
      for (y in 1:nyears){
        pr[p,s,y] <- delta[p,s,y] / sum(delta[p,1:nspecies,y])
        
        
        delta[p,s,y] ~ dgamma(alpha[p,s,y], 1)
        alpha[p,s,y] <- mu[p,s]*kappa[p]
        
        
      } #y
      
    }#s
    
    
    # kappa estimates the weight of the priors in 
    # estimating the species-level proportions
    # it is referred to as the concentration parameter
    # it's analogous to a variance on the probabilities
    # in this parameterization, it also provides a way to re-scale the dirichlet-
    # derived probabilities (alphapriors) to something more similar to the scale of the 
    # original part counts in the multi-year alphapriors
    
    
    
    
    #### multinomial logistic regression on hyperparameter mu[p,s]
    # logistic transformation
    for( s in 1:nspecies ){
      
      mu[p,s] <- exp.term[p,s]/sum(exp.term[p,1:nspecies])
      exp.term[p,s] <- exp(alphab1[p,s])
    }
    
    
  }#p
  
  alphab[1] <- 0
  
  for(s in 2:nspecies){
    alphab[s] ~ dnorm(0,0.01)
    
  }#s
  for(s in 1:nspecies){
    for(p in 1:nperiods){
      alphab1[p,s] ~ dnorm(alphab[s],taualphab)
    }#p
    
  }#s
  taualphab ~ dgamma(0.01,0.01)
  
  # JAGS doesn't allow the parameters of ddirch to be inferred. 
  #However you can use the observation that if delta[k] ~ dgamma(alpha[k], 1), 
  #then the vector with elements delta[k] / sum(delta[1:K]), k = 1, ., K, 
  #is Dirichlet with parameters alpha[k], k = 1, ., K.'
  # Then infer in the gamma distribution instead
  
  
  ### gamma dist for kappa, derived from priors for the mean and sd of gamma 
  ### consider making this prior less informative
  
  
  for(p in 1:nperiods){
    kappa[p] ~ dgamma(Skappa,Rkappa) 
  }
  Skappa <- pow(meanGamma,2)/pow(sdGamma,2) # gamma shape prior
  Rkappa <- meanGamma/pow(sdGamma,2) # gamma rate prior
  meanGamma <- 5
  sdGamma <- 5
  
  ### derived counts
  
}

