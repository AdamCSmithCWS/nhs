############################### model

## w_psy[p,s,y] = array of wing counts by periods (dim1) species (dim2) and years (dim3)
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
## kill_pyh[p,y,h] = total kill in period-x and year-y for each hunter-h
## nhunter_y[y] = number of hunters with calendar information in year-y


model {
  
  
  ### probability a potential hunter is active
  #nactive = data (number of active hunters in a given caste and year)
  #npotential = data (number of potential hunters in a given caste and year)
  for(y in 1:nyears){
  for(c in castes){
    nactive[c,y] ~ dbinom(pactive[c,y],npotential[c,y])
    NACTIVE_cy[c,y] <- pactive[c,y]*pops[c,y]
    
    nsucc[c,y] ~ dbinom(psucc[c,y],nactive[c,y])
    NSUCC_cy[c,y] <- psucc[c,y]*pops[c,y] 
    
  }
   NSUCC_y[y] <- sum(NSUCC_cy[castes,y]) 
   NACTIVE_y[y] <- sum(NACTIVE_cy[castes,y])  
  }
  
  ### all species harvest
  for(i in 1:nhs){
    
    
   ### successful hunters, alternative zero inflated approach
    ############ loop only includes active hunters because if days[i] = 0 (i.e., they're not active), then there is no uncertainty about kill[i] (kill must = 0)
    
    ### number harvested
    kill[i] ~ dpois(lambda[i]) #kill is data - each hunter's estimate of the total number of ducks killed 
    log(lambda[i]) <- elambda1[i] #*succ[caste[i]] + 0.0001 #cheat to avoid non-zero values from zero-mean poisson
    elambda1[i] <- ann[year[i]] + cst[caste[i]] + hntr[caste[i],year[i],hunter[i]] + elambda_day[i] #elambda_day[i] acts as an offset on effort
    
    ## ann[y] is a yearly intercept for all species kill
    ## cst[c] is a caste-specific intercept for all species kill
    ## hntr[c.y,h] is an individual random error term to account for the individual hunter skill and overdispersion, currently normal distribution

    

    
    
    ### number of days truncated Poisson, because only active hunters are included and therefore days != 0
    days[i] ~ dpois(lambda_day[i])T(1,) #kill is data - each hunter's estimate of the total number of days spent hunting
    log(lambda_day[i]) <- elambda_day[i] 
    elambda_day[i] <- ann_day[year[i]] + cst_day[caste[i]] + hntr_day[caste[i],year[i],hunter[i]]
    
    ## ann[y] is a yearly intercept for all species kill
    ## cst[c] is a caste-specific intercept for all species kill
    ## hntr_day[c.y,h] is an individual random error term to account for the individual hunter behaviour and overdispersion, currently a t-distribution
    
    
  }#i
  
  for(c in castes){
    ## harvest rate priors
    retrans_hunter[c] <- 0.5*(1/tauhunter[c])
    sdhunter[c] <- 1/pow(tauhunter[c],0.5)
    tauhunter[c] ~ dgamma(0.01,0.01)
    #nu[c] ~ dgamma(2,0.1)
    ## caste specific intercept priors
    cst[c] ~ dnorm(0,0.01)
    
    #activity (days) priors
    retrans_hunter_day[c] <- 0.5*(1/tauhunter_day[c])
    sdhunter_day[c] <- 1/pow(tauhunter_day[c],0.5)
    tauhunter_day[c] ~ dgamma(0.01,0.01)
    nu_day[c] ~ dgamma(2,0.1)
    ## caste specific intercept priors
    cst_day[c] ~ dnorm(0,0.01)
    
    
    # mmu_psucc[c] ~ dnorm(0,1)
    # phi_psucc[c] ~ dgamma(0.001,0.001)
    # tau_mu_psucc[c] ~ dgamma(0.001,0.001)
    # 
    # mmu_pactive[c] ~ dnorm(0,1)
    # phi_pactive[c] ~ dgamma(0.001,0.001)
    # tau_mu_pactive[c] ~ dgamma(0.001,0.001)
    # 
    
    for(y in 1:nyears){
      
      alpha_psucc[c,y] ~ dunif(1,3) #very simple priors for the parameters of the beta prior on pactive and psucc
      beta_psucc[c,y] ~ dunif(1,3)
      alpha_pactive[c,y] ~ dunif(1,3)
      beta_pactive[c,y] ~ dunif(1,3)
      
      # alpha_psucc[c,y] = phi_psucc[c] * mu_psucc[c,y]
      # beta_psucc[c,y] = phi_psucc[c] * (1 - mu_psucc[c,y])
      # logit(mu_psucc[c,y]) <- mmu_psucc[c] + dnorm(0,tau_mu_psucc[c])
      # psucc[c,y] ~ dbeta(alpha_psucc[c,y],beta_psucc[c,y])
      psucc[c,y] ~ dbeta(alpha_psucc[c,y],beta_psucc[c,y])
      
      # alpha_pactive[c,y] = phi_pactive[c] * mu_pactive[c,y]
      # beta_pactive[c,y] = phi_pactive[c] * (1 - mu_pactive[c,y])
      # logit(mu_pactive[c,y]) <- mmu_pactive[c] + dnorm(0,tau_mu_pactive[c])
      # pactive[c,y] ~ dbeta(alpha_pactive[c,y],beta_pactive[c,y])
      pactive[c,y] ~ dbeta(alpha_pactive[c,y],beta_pactive[c,y])
      
      
      ############# hunter-specific effects
      for(h in 1:nhunter_cy[c,y]){
        ## hunter[i] is an individual random error term to allow for overdispersion,
        ##  t-distribution used for overdispersion to allow for heavy-tailed 
        ## tauhunter[c] is a caste-specific precision to allow hunter-level dispersion to vary among castes
        ## consider whether tauhunter should also vary among years...
        
        hntr_day[c,y,h] ~ dt(0,tauhunter_day[c],nu_day[c])
        #hntr[i] ~ dt(0,tauhunter[caste[i]],nu[caste[i]])
        hntr[c,y,h] ~ dnorm(0,tauhunter[c]) #normal overdispersion assuming that most of variance in harvest is a function of how many days a hunter spends hunting
        #n days probably accounts for a bit of the hunting skill effect as well as the activity effect
        
      }#h
    }#y
    
  }#c
  
  ### yearly intercepts of total kill by first-difference
  ann[1] ~ dnorm(0,0.01)
  ann_day[1] ~ dnorm(0,0.01)
  for(y in 2:nyears){
    ann[y] ~ dnorm(ann[y-1],tauyear)
    ann_day[y] ~ dnorm(ann_day[y-1],tauyear_day)
    
  }
  
  # year priors
  sdyear <- 1/pow(tauyear,0.5)
  tauyear ~ dgamma(0.001,0.001)
  sdyear_day <- 1/pow(tauyear_day,0.5)
  tauyear_day ~ dgamma(0.001,0.001)
  
  
  ##################################
  ### derived estimates of per-hunter species kill in each year###############
  
  for(c in castes){
    for(y in 1:nyears){
      ## derived estimated means, which can then be multiplied by the extrapolation factors for each caste and year
      # estimate of the mean (per hunter) kill per caste, and year
        for(h in 1:nhunter_cy[c,y]){
          
        #consider alternative extrapolation factor approach, since hunters should be a random sample and representative
        totkillh[y,c,h] <- exp(ann[y] + cst[c] + hntr[c,y,h] + ann_day[y] + cst_day[c] + hntr_day[c,y,h])
        totdaysh[y,c,h] <- exp(ann_day[y] + cst_day[c] + hntr_day[c,y,h])
        }
      mean_totkill[y,c] <- mean(totkillh[y,c,1:nhunter_cy[c,y]]) #mean kill per active hunter
      mean_totdays[y,c] <- mean(totdaysh[y,c,1:nhunter_cy[c,y]]) #mean days per active hunter
      
      mean_totkill_retrans[y,c] <- exp(ann[y] + cst[c] + ann_day[y] + cst_day[c] + retrans_hunter_day[c] + retrans_hunter[c])
      mean_totdays_retrans[y,c] <- exp(ann[y] + cst[c] + ann_day[y] + cst_day[c] + retrans_hunter_day[c])
      
      for(p in 1:nperiods){
        ## estimate of the mean (per hunter) kill per period, caste, and year
        mean_kill_pcy[p,c,y] <-  pkill_py[p,y] * mean_totkill[y,c]
        
        for(s in 1:nspecies){
          # estimate of the mean (per hunter) kill, by period, caste, year, and species
          mean_kill_pcys[p,c,y,s] <- mean_kill_pcy[p,c,y] * pcomp_psy[p,s,y]
          #
        }#s
      }#p
    }#y
  }#c
  
  
  
  #total harvest estimate by species year and caste
  ## add in the estimated populations of each caste to generate final harvest estimates.
  for(y in 1:nyears){
   
    for(c in castes){
      kill_cy[c,y] <- mean_totkill[y,c]*pops[c,y] * pactive[c,y] #total kill by caste and year
      days_cy[c,y] <- mean_totdays[y,c]*pops[c,y] * pactive[c,y] #total days by caste and year
      
      for(s in 1:nspecies){
  kill_cys[c,y,s] <- sum(mean_kill_pcys[1:nperiods,c,y,s])*pops[c,y] * pactive[c,y]
      }#s
    }#c
    
    for(s in 1:nspecies){
      kill_ys[y,s] <- sum(kill_cys[castes,y,s])
    }#s
    
    kill_y[y] <- sum(kill_cy[castes,y])
    days_y[y] <- sum(days_cy[castes,y])
    
  }#y
  
  
  
  ###################################
  ######## Multinomial components
  
  
  ### estimating the proportional-seasonal distribution of total harvest by periods
  ## kill_pyh[p,y,h] is data, the hunter-level total harvest by period from the calendars
  ## nkill_yh[y,h] is also data, hunter-level total estimate of their harvest
  ### assumes that seasonal distribution is the same across castes...questionable...
  
  
#####################################################################
  #### proportional distribution of all birds killed across periods
  ### this component ignores hunter and caste specific variation in the seasonal spread of the hunt
  ### multinomial distribution across periods  
  ### kill_pyh and nkill_yh[y,h] are data
  for (y in 1:nyears) {
    for(h in 1:nhunter_y[y]){
      kill_pyh[1:nperiods,y,h] ~ dmulti(pkill_py[1:nperiods,y], nkill_yh[y,h])   # multinom distr vector responses
    }#h
  }#y
  
  ##### pkill_py[p,y] is the estimated proportion of the total duck harvest in year-y that occurred in period-p
  for (p in 1:nperiods){
    for (y in 1:nyears){
      pkill_py[p,y] <- delta_py[p,y] / sum(delta_py[1:nperiods,y])
      delta_py[p,y] ~ dgamma(exp_alpha_py[p,y], 1)
      exp_alpha_py[p,y] <- exp(alpha_py[p,y])
      # alphat[p,y] <- mut[p]*kappat ### mut[p] is the mean proportion of the hunt occurring in period p across all years
      # 
       } #y
  }#p

  # exponential transformation to monitor the hyperparameter mean proportion in a given period
  for(p in 1:nperiods){
    mut[p] <- exp.termt[p]/sum(exp.termt[1:nperiods])
    exp.termt[p] <- exp(alpha_p[p])
  }#p
 
  #### hyperparameter alpha_p[p] and tau_alpha_py[p], shrinking each period's estimate in a given year
  #### towards last years proportion of the hunt occurring within that period
  ### time-series first difference model through years on the period-specific parameters
  alpha_p[1] <- 0 ## these are the log-scale intercepts of the proportions of the hunt in each period
  alpha_py[1,1] <- 0
  for(y in 2:nyears){
    alpha_py[1,y] ~ dnorm(alpha_py[1,y-1],tau_alpha_py[1])
  }
  tau_alpha_py[1] ~ dscaled.gamma(2,2)
  for(p in 2:nperiods){
    alpha_p[p] ~ dnorm(0,0.001) # fixed effect period-effects on total kill
    alpha_py[p,1] <- alpha_p[p]
     for(y in 2:nyears){
       alpha_py[p,y] ~ dnorm(alpha_py[p,y-1],tau_alpha_py[p]) # first difference model through time
     }
    tau_alpha_py[p] ~ dscaled.gamma(2,2)
  }#p
  
  
  

  
  
 
  
##################################################################################
  ## species composition across periods and years
  ## same basic model as the overall kill proportional distribution across periods
  
  ######## this model currently doesn't use the variation among hunters in the species distribution
  ### but perhaps it should, commented-out section that follows suggests how it could work
  ##### species composition
  #     for (p in 1:nperiods){
  # 
  # for (y in 1:nyears) {
  #   for(h in 1:nhuntscs[y]){
  # w[p,1:nspecies,y,h] ~ dmulti(pcomp_psy[p,1:nspecies,y], nparts_py[p,y,h])   # multinom distr vector responses
  # 
  # nparts_py[p,y,h] <- sum(w[p,1:nspecies,y,h])# yearl and period sums of all parts
  #   }#h
  # }#y
  
  ##### species composition
  for (p in 1:nperiods){
    
    for (y in 1:nyears) {
      w_psy[p,1:nspecies,y] ~ dmulti(pcomp_psy[p,1:nspecies,y], nparts_py[p,y])   # multinom distr vector responses
      
    }
    
    ##### pcomp_psy[p,s,y] is the estimated proportion of the species composition in year-y, period-p, that is species-s
    
    for (s in 1:nspecies){
      for (y in 1:nyears){
        pcomp_psy[p,s,y] <- delta_psy[p,s,y] / sum(delta_psy[p,1:nspecies,y])
        delta_psy[p,s,y] ~ dgamma(exp_alpha_psy[p,s,y], 1)
        exp_alpha_psy[p,s,y] <- exp(alpha_psy[p,s,y])
        
      } #y
      
    }#s
    
    #### multinomial exponential regression on hyperparameter mu[p,s]
    # exponential transformation
    for( s in 1:nspecies ){
      mu_ps[p,s] <- exp.alpha_ps[p,s]/sum(exp.alpha_ps[p,1:nspecies]) #mean proportional contribution of species during period
      exp.alpha_ps[p,s] <- exp(alpha_ps[p,s])
    }
    
    
  }#p

  
  alpha_s[1] <- 0

  for(s in 2:nspecies){
    alpha_s[s] ~ dnorm(0,0.001)   # species mean proportional contribution across years and periods
    #prior on the sd of alpha_psy[s] equal to the half-t distribution with sd = 2 and 4 degrees of freedom.  The density isflat for very small values (σS) and has a long flat tail for large values (σS).  Thesefeatures protect against mis-specification of the prior scaleS.  Choosing a largeSis harmlessas it simply makes the prior flatter, whereas ifSis too small then the long flat tail ensuresthat sufficient data will dominate the prior.  Gelman [2006] calls this a “weakly informative”prior.  For comparison, figure 10.1 also shows the density onσin the limit as the degrees offreedomdfincreases to infinity.  The distribution then becomes a half-normal with standarddeviationS.  The effect of increasing the degrees of freedom is to diminish the weight of thetail.
    
  }#s
  for(s in 1:nspecies){
    tau_alpha_psy[s] ~ dscaled.gamma(2, 4) #year-effecct variance by species
    taualpha_s[s] ~ dscaled.gamma(2, 4) # period variance by species
    
    for(p in 1:nperiods){
      alpha_ps[p,s] ~ dnorm(alpha_s[s],taualpha_s[s])
      alpha_psy[p,s,1] <- alpha_ps[p,s]
      
      for(y in 2:nyears){
        alpha_psy[p,s,y] ~ dnorm(alpha_psy[p,s,y-1],tau_alpha_psy[s]) 
      }
      
    }#p
    
  }#s
 
  # JAGS doesn't allow the parameters of ddirch to be inferred. 
  #However you can use the observation that if delta[k] ~ dgamma(alpha[k], 1), 
  #then the vector with elements delta[k] / sum(delta[1:K]), k = 1, ., K, 
  #is Dirichlet with parameters alpha[k], k = 1, ., K.'
  # Then infer in the gamma distribution instead
  
 
  
  
  #####################################################################
  #### proportional distribution of age and sex by species
  
  ##### species composition
  for (s in 1:nspecies){
    
    for (y in 1:nyears) {
      w_axsy[1:ndemog,s,y] ~ dmulti(axcomp_axsy[1:ndemog,s,y], nparts_sy[s,y])   # multinom distr vector responses
      
    }
    
    ##### axcomp_psy[d,s,y] is the estimated proportion of the species harvest that is category-d of age and sex in year-y
    ### for ducks d is 1 for AF, 2 for IF, 3 for AM, and 4 for IM 
    
    for (d in 1:ndemog){
      for (y in 1:nyears){
        axcomp_axsy[d,s,y] <- delta_axsy[d,s,y] / sum(delta_axsy[1:ndemog,s,y])
        delta_axsy[d,s,y] ~ dgamma(exp_alpha_axsy[d,s,y], 1)
        exp_alpha_axsy[d,s,y] <- exp(alpha_axsy[d,s,y])
        
      } #y
      
    }#d
    
    #### monitoring the mean demographic splits across all years for a given species
    # exponential transformation
    for( d in 1:ndemog ){
      mu_axs[d,s] <- exp.alpha_axs[d,s]/sum(exp.alpha_axs[1:ndemog,s]) #
      exp.alpha_axs[d,s] <- exp(alpha_axs[d,s])
    }#d
    
    
  }#s
  
  
  alpha_ax[1] <- 0
  tau_alpha_ax[1] ~ dscaled.gamma(2, 4)
  
  for(d in 2:ndemog){
    alpha_ax[d] ~ dnorm(0,0.001)   # mean demographic contribution across species (assuming that demography is similar across species...sketchy...)
    tau_alpha_ax[d] ~ dscaled.gamma(2, 4)
    #prior on the variance of demographic contribution across species for a given demographic grouping
    #prior on the sd of alpha_psy[s] equal to the half-t distribution with sd = 2 and 4 degrees of freedom.  The density isflat for very small values (σS) and has a long flat tail for large values (σS).  Thesefeatures protect against mis-specification of the prior scaleS.  Choosing a largeSis harmlessas it simply makes the prior flatter, whereas ifSis too small then the long flat tail ensuresthat sufficient data will dominate the prior.  Gelman [2006] calls this a “weakly informative”prior.  For comparison, figure 10.1 also shows the density onσin the limit as the degrees offreedomdfincreases to infinity.  The distribution then becomes a half-normal with standarddeviationS.  The effect of increasing the degrees of freedom is to diminish the weight of thetail.
    
  }#d
  
  for(s in 1:nspecies){
    tau_alpha_axsy[s] ~ dscaled.gamma(2,4) #variance of year-effects for the demographic parameters by species
    
    for(d in 1:ndemog){
     
      alpha_axs[d,s] ~ dnorm(alpha_ax[d],tau_alpha_ax[d])
      alpha_axsy[d,s,1] <- alpha_axs[d,s] # first year
      
      for(y in 2:nyears){
        alpha_axsy[d,s,y] ~ dnorm(alpha_axsy[d,s,y-1],tau_alpha_axsy[s]) 
      }#y
    }#d
    
  }#s
 
  
}## end of model

