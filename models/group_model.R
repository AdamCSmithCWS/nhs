############################### model
### data required
# pops = pops, # pops[c.y] total populations of permits by caste and year used to rescale all perhunter estimates to totals 
# #component that estimates p_active etc. to generate totals of active and successful hunters by year
# nactive = nactive, # nactive[c,y] number of active hunters by caste and year
# npotential = npotential, # npotential[c,y] number of potential hunters (respondents who bought a permit this year) by caste and year
# nsucc = nsucc, # nsucc[c,y] number of successful hunters by caste and year (active hunters with harvest > 0)
# #spcies composition components
# #indicators
# nyears = nyears, #integer length = 1 number of years
# nhunter_y = nhunter_y, # nhunter_y[nyears] number active hunters by year
# nhunter_cy = nhunter_cy, # nhunter_cy[castes,nyears] number active hunters by caste and year
# castes = castes, # castes (numeric, 1:4)
# ncastes = length(castes), # number of castes
# nhs = nhs, # integer length = 1 number of active hunters over all years (nrow for sumkill_active)
# #main data for overall harvest estimates
# hunter = hunter_n_cy, # vector(length = nhs) unique numeric indicator for active hunters by caste and year 
# kill = kill, # vector(length = nhs), total group (ducks, geese, murres) harvest of nhs response
# year = year, # vector(length = nhs), year of response
# caste = caste, # vector(length = nhs), caste of response
# days = days# vector(length = nhs), number of days spent hunting


model {
  
  
  ### probability a potential hunter is active
  #nactive = data (number of active hunters in a given caste and year)
  #npotential = data (number of potential hunters in a given caste and year)
  for(y in 1:nyears){
    
    ## two correction factors to account for the inter-province hunting
    ## pleave = proportion of permit population that hunted somewhere else (i.e, the proportion of the pops that are hunting somewhere else and should be removed, factor by which pops should be reduced)
    leave_hunt_cf[y,1] ~ dbinom(pleave[y],leave_hunt_cf[y,2])
    ## n_arrive[c,y] = caste and year estimate of the number of permits that should be added to the total population
    ## currently not estimated as part of the model because the number of permits used to estimate this value is extremely large (all permits sampled in all other zones) and so the binomial error associated with it should be trivial
    
    
  #### corrections to account for the inter-provincial hunting, and the proportion of the permit-population that are active
  for(c in 1:ncastes){
    
    pops_cor[c,y] <- pops[c,y]+(n_arrive[c,y])-(pops[c,y]*pleave[y])
    

    nactive[c,y] ~ dbinom(pactive[c,y],npotential[c,y])
    
    for(g in 1:ngroups){     
    nsucc[g,c,y] ~ dbinom(psucc[g,c,y],nactive[c,y])
    NSUCC_gcy[g,c,y] <- psucc[g,c,y]*NACTIVE_cy[c,y]
    
    }#g
    NACTIVE_cy[c,y] <- pactive[c,y]*pops_cor[c,y] #NACTIVE_cy is the estmiated number of non-waterfowl active hunters - used to rescale all per-hunter values in the derived quantities
    
  }#c
    for(g in 1:ngroups){     
      NSUCC_yg[y,g] <- sum(NSUCC_gcy[g,castes,y])*reg_mat[y,g]
   NACTIVE_yg[y,g] <- sum(NACTIVE_cy[castes,y])*reg_mat[y,g]  
    }#g
    NACTIVE_y[y] <- sum(NACTIVE_cy[castes,y])
    
  }#y
  
  ### all other harvest
  for(i in 1:nhs){ #nhs 
   
   ### active hunters, 
    ########### could try a model that only models the kill values for hunters with kill > 0, then moltiplies the derived parameters by the psucc
    ############ loop only includes active hunters because if days[i] = 0 (i.e., they're not active), then there is no uncertainty about kill[i] (kill must = 0)
for(g in 1:ngroups){
    ### number harvested for each group by hunter[i]
    kill[i,g] ~ dpois(lambda[i,g]) #kill is data - each hunter's estimate of the total number killed 
    lambda[i,g] <- lambda1[i,g]*z[i,g] + 0.00001 ## hack required for JAGS -- otherwise 'incompatible'-error
   
      z[i,g] <- z1[i,g]*reg_mat[year[i],g] #this reg_mat matrix has 1 for every year with a group-season and 0 for each year with no group-season
  #reg_mat removes the effect of group[y,g] in the likelihood line below, and it forces the zero-inflated portion to 0, absorbing all of the zero-counts for a given group and year with no season
    z1[i,g] ~ dbern(psi[g]) #psi = proportion of non-zeros for that group
    log(lambda1[i,g]) <- elambda1[i,g] #
    elambda1[i,g] <- cst[caste[i],year[i]] + hntr[caste[i],year[i],g,hunter[i]] + group[year[i],g]*reg_mat[year[i],g] + elambda_day[i] #elambda_day[i] acts as an offset on effort
}
    ## ann[y] is a yearly intercept for all species kill ann[year[i]] + replaced by group
    ## cst[c] is a caste-specific intercept for all species kill
    ## hntr[c.y,h] is an individual random error term to account for the individual hunter skill and overdispersion, currently normal distribution

    

    
    
    ### number of days truncated Poisson, because only active hunters are included and therefore days != 0
    days[i] ~ dpois(lambda_day[i])T(1,) #kill is data - each hunter's estimate of the total number of days spent hunting
    log(lambda_day[i]) <- elambda_day[i] 
    elambda_day[i] <- ann_day[year[i]] + cst_day[caste[i],year[i]] + hntr_day[caste[i],year[i],hunter[i]]
    
    ## ann[y] is a yearly intercept for all species kill
    ## cst[c] is a caste-specific intercept for all species kill
    ## hntr_day[c.y,h] is an individual random error term to account for the individual hunter behaviour and overdispersion, currently a t-distribution
    
    
  }#i
  
  tau_alpha_pleave ~ dscaled.gamma(0.5,50) # assumption that the
  alpha_pleave[1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
  logit(pleave[1]) <- alpha_pleave[1]
  
  for(y in 2:nyears){
    
  alpha_pleave[y] ~ dnorm(alpha_pleave[y-1],tau_alpha_pleave) 
  logit(pleave[y]) <- alpha_pleave[y]
  
  }
  
  for(g in 1:ngroups){
    ### zip priors beta-binomial priors
    ### psi is the estimated proportion of active other hunters that hunt this group - i.e., the zero component that's in addition to the over-dispersed Poisson-distribution
    alpha_psi[g] ~ dunif(1,3)
    beta_psi[g] ~ dunif(1,3)
    psi[g] ~ dbeta(alpha_psi[g],beta_psi[g])
   
    ### group-effects - these are the annual harvest-rate effects for each group, time-series from the first year that group is harvested.
    tau_group[g] ~ dscaled.gamma(0.5,50)#time-series variance
    for(y in 1:fyear[g]){
      group[y,g] ~ dnorm(0,1)
    }#y
    for(y in (fyear[g]+1):nyears){
      group[y,g] ~ dnorm(group[y-1,g],tau_group[g])
    }#y
    
    
  }#g
  
  
  
  ######## caste effects
 CCST[1] <- 0
 CCST_day[1] <- 0
 CCST[2] <- 0
 CCST_day[2] <- 0
 
  for(c in 3:ncastes){
  CCST[c] ~ dnorm(0,16) #strongly informative, shrinkage prior sd = 0.25
  CCST_day[c] ~ dnorm(0,0.01)
  }

 tau_cst_day[1] ~ dscaled.gamma(0.25,50) #~ dgamma(0.001,0.001)
 tau_cst_day[2] <- 5#never used just filling an empty vector index
 

   cst[1,1] <- CCST[1] #fixed value in first year, to help separately estimate the cst effects from ann
   cst_day[1,1] <- CCST_day[1] #fixed value in first year, to help separately estimate the cst_day effects from ann_day
   
   for(y in 2:nyears){  ### random effects for caste day, allowing them to vary randomly by year
     
     # cst[c,y] ~ dnorm(CCST[c],tau_cst[c])
     # cst_day[c,y] ~ dnorm(CCST_day[c],tau_cst_day[c])
     cst[1,y] <- CCST[1] # fixed caste effect through time.
     cst_day[1,y] ~ dnorm(CCST_day[1],tau_cst_day[1])
   }

   cst[2,1] <- cst[1,1] #fixed value in first year, to help separately estimate the cst effects from ann
   cst_day[2,1] <- cst_day[1,1] #fixed value in first year, to help separately estimate the cst_day effects from ann_day
   
   for(y in 2:nyears){  ### CAste 2 forced to be the same as 1 (there's never enough info to estimate it separately)
     
     # cst[c,y] ~ dnorm(CCST[c],tau_cst[c])
     # cst_day[c,y] ~ dnorm(CCST_day[c],tau_cst_day[c])
     cst[2,y] <- cst[1,y] # fixed caste effect through time.
     cst_day[2,y] <- cst_day[1,y]
   }
   

   for(c in 3:ncastes){
     cst[c,1] <- CCST[c] #fixed value in first year, to help separately estimate the cst effects from ann
     cst_day[c,1] <- CCST_day[c] #fixed value in first year, to help separately estimate the cst_day effects from ann_day
   }
   
   for(y in 2:nyears){  ### random effects for caste effects, allowing them to vary randomly by year
     
     for(c in 3:ncastes){
     cst[c,y] <- CCST[c] # fixed caste effect through time.
    cst_day[c,y] ~ dnorm(CCST_day[c],tau_cst_day[c])
}
   }
   # tau_cst[c] ~ dgamma(0.001,0.001) # assumption that the
   for(c in 3:ncastes){
     tau_cst_day[c] ~ dscaled.gamma(0.5,50)
   }

  
 ############################### forcing a single estimate of hunter level variance from caste D onto all other castes
   for(g in 1:ngroups){
   retrans_hunter[1,g] <- (0.5*(1/tauhunter[1,g]))/nu_ret[g]

 sdhunter[1,g] <- 1/pow(tauhunter[1,g],0.5)
 #tauhunter[1] ~ dgamma(0.01,0.01)
 tauhunter[1,g] ~ dscaled.gamma(0.5,50) #implicit prior on sigma of a half-t dist: sigma = 1*t(df = 50) , i.e., 99% prob sd < 2
 
 nu[1,g] ~ dgamma(2,0.2)
 
 nu_ret[g] <- (1.422*nu[1,g]^0.906)/(1+(1.422*nu[1,g]^0.906)) #approximate retransformation to equate a t-distribution to a normal distribution - see appendix of Link et al. 2020 BBS model selection paper
 
 ## caste specific intercept priors on harvest by group
 for(c in 2:ncastes){

   ## harvest rate priors
   retrans_hunter[c,g] <-  retrans_hunter[1,g]
   sdhunter[c,g] <- 1/pow(tauhunter[1,g],0.5)
   tauhunter[c,g] <- tauhunter[1,g]
   nu[c,g] <- nu[1,g]
   }
 }

  for(c in 1:ncastes){

   
    #activity (days) priors
    retrans_hunter_day[c] <- (0.5*(1/tauhunter_day[c]))/nu_day_ret[c]
    sdhunter_day[c] <- 1/pow(tauhunter_day[c],0.5)
    tauhunter_day[c] ~ dscaled.gamma(0.5,50) #implicit prior on sigma of a half-t dist: sigma = 0.5*t(df = 50) , i.e., 99% prob sd < 1
    nu_day[c] ~ dgamma(2,0.2)
    nu_day_ret[c] <- (1.422*nu_day[c]^0.906)/(1+(1.422*nu_day[c]^0.906)) #approximate retransformation to equate a t-distribution to a normal distribution - see appendix of Link et al. 2020 BBS model selection paper
    
    ## caste specific intercept priors
    
    for(g in 1:ngroups){
     mmu_psucc[g,c,1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
     # phi_psucc[c] ~ dscaled.gamma(0.5,50)
     tau_mmu_psucc[g,c] ~ dscaled.gamma(0.5,50)#time-series variance
    }# 
    mmu_pactive[c,1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
    #phi_pactive[c] ~ dscaled.gamma(0.5,50) #
    tau_mu_pactive[c] ~ dscaled.gamma(0.5,50) #time-series variance
    # 
    
     for(y in 2:nyears){
       for(g in 1:ngroups){
       mmu_psucc[g,c,y] ~ dnorm(mmu_psucc[g,c,y-1],tau_mmu_psucc[g,c])
       }
       mmu_pactive[c,y] ~ dnorm(mmu_pactive[c,y-1],tau_mu_pactive[c])
     }
    
    for(y in 1:nyears){
      

      for(g in 1:ngroups){
      logit(psucc[g,c,y]) <- mmu_psucc[g,c,y]
      }

      logit(pactive[c,y]) <- mmu_pactive[c,y]
      
      ############# hunter-specific effects
      for(h in 1:nhunter_cy[c,y]){
        hntr_day[c,y,h] ~ dt(0,tauhunter_day[c],nu_day[c])
        for(g in 1:ngroups){
        hntr[c,y,g,h] ~ dt(0,tauhunter[c,g],nu[c,g])
        }
      }#h
    }#y
    
  }#c
  
 ann_day[1] ~ dnorm(0,1) # fixed effect for year-1 annual activity level
  
  for(y in 2:nyears){
    ann_day[y] ~ dnorm(ann_day[y-1],tauyear_day)
    

  }
  
  # first-difference harvest and activity variance priors
   sdyear <- 1/pow(tauyear,0.5)
 tauyear ~ dscaled.gamma(0.5,50)
 sdyear_day <- 1/pow(tauyear_day,0.5)
 tauyear_day ~ dscaled.gamma(0.5,50)
  # 
  
  
  ##################################
  ### derived estimates of mean harvest by hunter ###############
  
  for(c in 1:ncastes){
    for(y in 1:nyears){
      ## derived estimated means, which can then be multiplied by the extrapolation factors for each caste and year
      # estimate of the mean (per hunter) kill per caste, and year
        for(h in 1:nhunter_cy[c,y]){
          for(g in 1:ngroups){
        #hunter-level predictions of mean kill
        totkill_hcy[y,c,h,g] <- exp(group[y,g] + cst[c,y] + hntr[c,y,g,h] + ann_day[y] + cst_day[c,y] + hntr_day[c,y,h]) *psi[g]*reg_mat[y,g]
          }#g
        totdays_hcy[y,c,h] <- exp(ann_day[y] + cst_day[c,y] + hntr_day[c,y,h])
        }
      #mean per-hunter kill and days by year and caste
      for(g in 1:ngroups){
        mean_totkill_ycg_alt[y,c,g] <- mean(totkill_hcy[y,c,1:nhunter_cy[c,y],g]) #mean kill per active hunter
        #mean per-hunter kill and days by year and caste - alternative estimate
        mean_totkill_ycg[y,c,g] <- exp(group[y,g] + cst[c,y] + ann_day[y] + cst_day[c,y] + retrans_hunter_day[c] + retrans_hunter[c,g]) *psi[g]*reg_mat[y,g]
      }
      mean_totdays_yc_alt[y,c] <- mean(totdays_hcy[y,c,1:nhunter_cy[c,y]]) #mean days per active hunter


      mean_totdays_yc[y,c] <- exp(ann_day[y] + cst_day[c,y] + retrans_hunter_day[c])
      

    }#y
  }#c
  
  

  #total harvest estimate by group year and caste
  ## mean harvest and days * population sizes of each caste and year * esimated proportion of population that is active to generate final harvest estimates.
  for(y in 1:nyears){
   
    for(c in 1:ncastes){
      for(g in 1:ngroups){
      kill_cyg[c,y,g] <- mean_totkill_ycg[y,c,g]*NACTIVE_cy[c,y] #total kill by caste and year
      days_cyg[c,y,g] <- mean_totdays_yc[y,c]*NACTIVE_cy[c,y]*reg_mat[y,g]*psi[g] #total days by caste, group, and year - using psi here is somewhat questionable - it assumes that the inflated zeros are a function of active hunters that aren't seeking this particular group and therefore that the probability of a non-zero reported harvest (after the Poisson-related zeros) represents the proportion of hunters and hunting days where someone is seeking this species
      }
      days_cy[c,y] <- mean_totdays_yc[y,c]*NACTIVE_cy[c,y] #total days by caste and year
}
    
    #### summed total harvest and activity (e.g., all ducks) across castes in a given year for each group
    for(g in 1:ngroups){
      kill_yg[y,g] <- sum(kill_cyg[castes,y,g])
    days_yg[y,g] <- sum(days_cyg[castes,y,g])
    }
    days_y[y] <- sum(days_cy[castes,y])
  }#y
  
  
 
  
}## end of model

