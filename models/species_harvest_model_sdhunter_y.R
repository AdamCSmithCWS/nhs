############################### model
############### ZIP time-series version - alternate to the standard ZIP
### data required
# pops = pops, # pops[c.y] total populations of permits by caste and year used to rescale all perhunter estimates to totals 
# #component that estimates p_active etc. to generate totals of active and successful hunters by year
# nactive = nactive, # nactive[c,y] number of active hunters by caste and year
# npotential = npotential, # npotential[c,y] number of potential hunters (respondents who bought a permit this year) by caste and year
# nsucc = nsucc, # nsucc[c,y] number of successful hunters by caste and year (active hunters with harvest > 0)
# #spcies composition components
# w_psy = partsarray, # w_psy[nperiods,nspecies,nyears] wings by period species year
# nparts_py = nparts_py, # nparts_py[nperiods,nyears] sum parts across species by period and year
# nparts_sy = nparts_sy, # nparts_sy[nspecies,nyears] sum parts species and year that have age and sex info
# kill_pyh = periodkill, # kill_pyh[nperiods,nyears,max(nhunters[y])] hunter-level total harvest by period from the calendars(separate hunter id caste doesn't matter)
# nkill_yh = nkill_yh, # nkill_yh[nyears,max(nhunters[y])] hunter-level summed harvest from calendar (separate hunter id caste doesn't matter)
# # demographic data for age and sex component of the model
# w_axsy = agesexarray, # w_axsy[ndemog,nspecies,nyears] wings by age-sex species year
# #indicators
# ndemog = ndemog, # 2 if geese (A and I) 4 if ducks (AF, IF, AM, IM) number of demographic units (age-sex combinations)
# nspecies = nspecies, # integer length = 1 number of species
# nyears = nyears, #integer length = 1 number of years
# nperiods = nperiods, # integer length = 1 number of periods
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
###### see below for alternate w[p,s,y,h] - hunter-specific data
#### hunter specific data would be better because it would better account for the uncertainty among hunters in the species composition
#### currently, this assumes that the selection of hunters in a given zone and year is representative
#### this over-estimates the certainty of the annual estimates of composition, and downweights the influence of the normalizing prior


model {
  
  
  ### probability a potential hunter is active
  #nactive = data (number of HQS responses from active hunters in a given caste and year)
  #npotential = data (number of HQS responses from potential hunters in a given caste and year)
  for(y in 1:nyears){
    
    ## two correction factors to account for the inter-province hunting
    ## parrive = proportion of hunting values that were sampled in another province (i.e., the proportion of hunters hunting in prov that are not included in pops, 1+parrive = factor by which pops should be increased)
    ## pleave = proportion of permit population that hunted somewhere else (i.e, the proportion of the pops that are hunting somewhere else and should be removed, factor by which pops should be reduced)
    arrive_hunt_cf[y,1] ~ dbinom(parrive[y],arrive_hunt_cf[y,2])
    leave_hunt_cf[y,1] ~ dbinom(pleave[y],leave_hunt_cf[y,2])
    
    
 
    prov_flow[y] <- parrive[y]-pleave[y] #difference in proportion of hunters coming and going, negative values indicate permits purchased in zone tend to hunt somewhere else
  for(c in 1:ncastes){
    
    pops_cor[c,y] <- pops[c,y]+(pops[c,y]*(parrive[y]))-(pops[c,y]*pleave[y])
    
    
    nactive[c,y] ~ dbinom(pactive[c,y],npotential[c,y])
    NACTIVE_cy[c,y] <- pactive[c,y]*pops_cor[c,y] #NACTIVE_cy used (called H sub_c sub_y in Smith et al.) to rescale all per-hunter values in the derived quantities
    
    nsucc[c,y] ~ dbinom(psucc[c,y],nactive[c,y])
    NSUCC_cy[c,y] <- psucc[c,y]*NACTIVE_cy[c,y]
    
  }
   NSUCC_y[y] <- sum(NSUCC_cy[castes,y]) 
   NACTIVE_y[y] <- sum(NACTIVE_cy[castes,y])  
  }
  
  ### all species harvest
  for(i in 1:nhs){
    
    
   ### active hunters, 
    ########### could try a model that only models the kill values for hunters with kill > 0, then moltiplies the derived parameters by the psucc
    ############ loop only includes active hunters because if days[i] = 0 (i.e., they're not active), then there is no uncertainty about kill[i] (kill must = 0)
    
    ### number harvested
    kill[i] ~ dpois(lambda[i]) #kill is data - each hunter's estimate of the total number of ducks killed 
    lambda[i] <- lambda1[i]*z[i] + 0.00001 ## hack required for JAGS -- otherwise 'incompatible'-error
    z[i] ~ dbern(psi[year[i]]) #psi = proportion of non-zeros for each year
    log(lambda1[i]) <- elambda1[i] #*succ[caste[i]] + 0.0001 #cheat to avoid non-zero values from zero-mean poisson
    elambda1[i] <- ann[year[i]] + cst[caste[i],year[i]] + hntr[caste[i],year[i],hunter[i]] + elambda_day[i] #elambda_day[i] acts as an offset on effort
    
    ## ann[y] is a yearly intercept for all species kill
    ## cst[c] is a caste-specific intercept for all species kill
    ## hntr[c.y,h] is an individual random error term to account for the individual hunter skill and overdispersion, currently normal distribution

    

    
    
    ### number of days truncated Poisson, because only active hunters are included and therefore days != 0
    days[i] ~ dpois(lambda_day[i])T(1,) #kill is data - each hunter's estimate of the total number of days spent hunting
    log(lambda_day[i]) <- elambda_day[i] 
    elambda_day[i] <- ann_day[year[i]] + cst_day[caste[i],year[i]] + hntr_day[caste[i],year[i],hunter[i]]
    
    ## ann_day[y] is a yearly intercept for all species activity
    ## cst_day[c] is a caste-specific intercept for all species activity
    ## hntr_day[c.y,h] is an individual random error term to account for the individual hunter behaviour and overdispersion, currently a t-distribution
    
    
  }#i
  
  ### zip priors 
  ### psi is the estimated annual proportion of waterfowl hunters that hunt the group of interest (e.g., ducks or geese)
#logistic random-walk time series model for psi
  

  tau_alpha_psi ~ dscaled.gamma(0.5,50) # assumption that the
  alpha_psi[1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
  
  logit(psi[1]) <- alpha_psi[1]
  
  
  tau_alpha_parrive ~ dscaled.gamma(0.5,50) # assumption that the
  alpha_parrive[1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
  logit(parrive[1]) <- alpha_parrive[1]
  
  tau_alpha_pleave ~ dscaled.gamma(0.5,50) # assumption that the
  alpha_pleave[1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
  logit(pleave[1]) <- alpha_pleave[1]
  
  
  for(y in 2:nyears){
    
      alpha_psi[y] ~ dnorm(alpha_psi[y-1],tau_alpha_psi) 
    logit(psi[y]) <- alpha_psi[y]
    
    alpha_parrive[y] ~ dnorm(alpha_parrive[y-1],tau_alpha_parrive) 
    logit(parrive[y]) <- alpha_parrive[y]
    
    alpha_pleave[y] ~ dnorm(alpha_pleave[y-1],tau_alpha_psi) 
    logit(pleave[y]) <- alpha_pleave[y]
    
  }
  

  # 
  
 CCST[1] <- 0
 CCST_day[1] <- 0
  for(c in 2:ncastes){
  CCST[c] ~ dnorm(0,0.1)
  CCST_day[c] ~ dnorm(0,0.1)
  }

 for(y in 1:nyears){  ### for caste 1, caste effects fixed at 0
   
   cst[1,y] <- 0
   cst_day[1,y] <- 0
 }
 
 for(c in 2:ncastes){
      for(y in 1:nyears){  ### random effects for caste effects, allowing them to vary randomly by year
     
    cst[c,y] ~ dnorm(CCST[c],tau_cst[c])
    cst_day[c,y] ~ dnorm(CCST_day[c],tau_cst_day[c])
    }
    tau_cst[c] ~ dscaled.gamma(0.5,50) # assumption that the
    tau_cst_day[c] ~ dscaled.gamma(0.5,50)
  }
  
  
  for(c in 1:ncastes){
    ## harvest rate variance priors
    log_tauhunter[c,1] ~ dnorm(0,1) #first-year fixed effect log-scale variance
    tau_logtauhunter[c] ~ dscaled.gamma(0.1,50)
    log(tauhunter[c,1]) <- log_tauhunter[c,1]
    sdhunter[c,1] <- 1/pow(tauhunter[c,1],0.5)
    retrans_hunter[c,1] <- 0.5*(1/tauhunter[c,1])/nu_ret[c] 
    
    for(y in 2:nyears){# log-scale time-series model on the tauhunter values
      log_tauhunter[c,y] ~ dnorm(log_tauhunter[c,y-1],tau_logtauhunter[c])
      log(tauhunter[c,y]) <- log_tauhunter[c,y]
      sdhunter[c,y] <- 1/pow(tauhunter[c,y],0.5)
      retrans_hunter[c,y] <- 0.5*(1/tauhunter[c,y])/nu_ret[c] 
      
    }
    
    nu[c] ~ dgamma(2,0.2)
    nu_ret[c] <- (1.422*nu[c]^0.906)/(1+(1.422*nu[c]^0.906)) #approximate retransformation to equate a t-distribution to a normal distribution - see appendix of Link et al. 2020 BBS model selection paper

    #activity (days) variance priors
    retrans_hunter_day[c] <- 0.5*(1/tauhunter_day[c])/nu_day_ret[c]
    sdhunter_day[c] <- 1/pow(tauhunter_day[c],0.5)
    tauhunter_day[c] ~ dscaled.gamma(0.5,50)
    nu_day[c] ~ dgamma(2,0.2)
    nu_day_ret[c] <- (1.422*nu_day[c]^0.906)/(1+(1.422*nu_day[c]^0.906)) #approximate retransformation to equate a t-distribution to a normal distribution - see appendix of Link et al. 2020 BBS model selection paper
    

    
    #logistic regression time-series model for the proportion that are active and successful
     mmu_psucc[c,1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
    # phi_psucc[c] ~ dgamma(0.001,0.001)
     tau_mmu_psucc[c] ~ dscaled.gamma(0.5,50)#time-series variance
     # 
     mmu_pactive[c,1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
     #phi_pactive[c] ~ dscaled.gamma(0.5,50) #
     tau_mu_pactive[c] ~ dscaled.gamma(0.5,50) #time-series variance
     
     ##time series model for the proportion that are active
     for(y in 2:nyears){
       mmu_psucc[c,y] ~ dnorm(mmu_psucc[c,y-1],tau_mmu_psucc[c])
       mmu_pactive[c,y] ~ dnorm(mmu_pactive[c,y-1],tau_mu_pactive[c])
     }
     
    for(y in 1:nyears){
      
      logit(psucc[c,y]) <- mmu_psucc[c,y]
      logit(pactive[c,y]) <- mmu_pactive[c,y]
      
      
      
      ############# hunter-specific effects
      for(h in 1:nhunter_cy[c,y]){
        ## hntr[c,y,h] is an individual random error term to allow for overdispersion in the harvest,
        ##  normal-distribution used for overdispersion in harvest assuming that activity accounts for most of the heavy-tailed variation in harvest
        ## hntr_day[c,y,h] is an individual random error term to allow for overdispersion in the activity (days spent hunting),
        ##  t-distribution used for overdispersion in activity to allow for heavy-tailed 
        ## variances on both activity and harvest are caste-specific precision to allow hunter-level dispersion to vary among castes
        ## consider whether tauhunter should also vary among years...
        
        hntr_day[c,y,h] ~ dt(0,tauhunter_day[c],nu_day[c])
        hntr[c,y,h] ~ dt(0,tauhunter[c,y],nu[c])
        #n days probably accounts for a bit of the hunting skill effect as well as the activity effect
        
      }#h
    }#y
    
  }#c
  
  ### yearly intercepts of total kill by first-difference 
  ann[1] ~ dnorm(0,0.1) # fixed effects for year-1 annual harvest level
  ann_day[1] ~ dnorm(0,0.1) # fixed effect for year-1 annual activity level
  
  for(y in 2:nyears){
    ann[y] ~ dnorm(ann[y-1],tauyear)
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

        #hunter-level predictions of mean kill
        totkill_hcy[y,c,h] <- exp(ann[y] + cst[c,y] + hntr[c,y,h] + ann_day[y] + cst_day[c,y] + hntr_day[c,y,h]) *psi[y]
        totdays_hcy[y,c,h] <- exp(ann_day[y] + cst_day[c,y] + hntr_day[c,y,h])
        }
      #mean per-hunter kill and days by year and caste
      mean_totkill_yc_alt[y,c] <- mean(totkill_hcy[y,c,1:nhunter_cy[c,y]]) #mean kill per active hunter
      mean_totdays_yc_alt[y,c] <- mean(totdays_hcy[y,c,1:nhunter_cy[c,y]]) #mean days per active hunter

      #mean per-hunter kill and days by year and caste - alternative estimate
      mean_totkill_yc[y,c] <- exp(ann[y] + cst[c,y] + ann_day[y] + cst_day[c,y] + retrans_hunter_day[c] + retrans_hunter[c,y]) *psi[y]
      mean_totdays_yc[y,c] <- exp(ann_day[y] + cst_day[c,y] + retrans_hunter_day[c])
      
      for(p in 1:nperiods){
        ## estimate of the mean (per hunter) kill per period, caste, and year
        mean_kill_pcy[p,c,y] <-  pkill_py[p,y] * mean_totkill_yc[y,c]
        #mean_kill_pcy[p,c,y] <-  pkill_py[p,y] * mean_totkill_retrans_yc[y,c] #alternate harvest estimate using retransformation fators
        
        for(s in 1:nspecies){
          # estimate of the mean (per hunter) kill, by period, caste, year, and species
          # mean kill by period caste and year * proportional composition of each species in each period and year
          mean_kill_pcys[p,c,y,s] <- mean_kill_pcy[p,c,y] * pcomp_psy[p,s,y]
          #
        }#s
      }#p
    }#y
  }#c
  
  

  #total harvest estimate by species year and caste
  ## mean harvest and days * population sizes of each caste and year * esimated proportion of population that is active to generate final harvest estimates.
  for(y in 1:nyears){
   
    for(c in 1:ncastes){
      kill_cy[c,y] <- mean_totkill_yc[y,c]*NACTIVE_cy[c,y] #total kill by caste and year
      days_cy[c,y] <- mean_totdays_yc[y,c]*NACTIVE_cy[c,y] #total days by caste and year
      
      for(s in 1:nspecies){
  kill_cys[c,y,s] <- sum(mean_kill_pcys[1:nperiods,c,y,s]) * NACTIVE_cy[c,y]
      }#s
    }#c
    
    
    ##### kill_ys = species level total estimated harvest by year
    for(s in 1:nspecies){
      kill_ys[y,s] <- sum(kill_cys[castes,y,s])
        for(d in 1:ndemog){
            ### kill_ysax = total harvest by age/sex species and year (e.g., number of adult female mallards killed)
          kill_ysax[d,s,y] <- axcomp_axsy[d,s,y]*kill_ys[y,s]
        }#d
      padult_sy[s,y] <- sum(axcomp_axsy[demoa,s,y])
      pfemale_sy[s,y] <- sum(axcomp_axsy[demof,s,y])
    }#s
    
    #### summed total harvest and activity (e.g., all ducks) across all species and castes in a given year
    kill_y[y] <- sum(kill_cy[castes,y])
    days_y[y] <- sum(days_cy[castes,y])
    
  }#y
  
  
 
 
 
 
 
 
 
 

  
  ###################################
  ######## Multinomial components - 3 separate parts
  ### 1 - proportional distribution of all harvest across periods
  ### 2 - proportional distribution of species harvest across periods
  ### 3 - proportional distribution of age and sex categories by species and year
  
  ### estimating the proportional-seasonal distribution of total harvest by periods
  ## kill_pyh[p,y,h] is data, the hunter-level total harvest by period from the calendars
  ## nkill_yh[y,h] is also data, hunter-level total estimate of their harvest
  ### assumes that seasonal distribution is the same across castes...questionable...
  
 # JAGS doesn't allow the parameters of ddirch to be inferred. 
 #However you can use the observation that if delta[k] ~ dgamma(alpha[k], 1), 
 #then the vector with elements delta[k] / sum(delta[1:K]), k = 1, ., K, 
 #is Dirichlet with parameters alpha[k], k = 1, ., K.'
 # Then infer in the gamma distribution instead
 
 
#####################################################################
  #### proportional distribution of all birds killed across periods
  ### this component ignores caste specific variation in the seasonal spread of the hunt
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
 
  #### multinomial dirichlet-prior, time-series model for the distribution of harvest across periods
  #### fixed effects for the mean harvest across periods in the first year
  #### hyperparameter alpha_py[p] and tau_alpha_py[p], shrinking each period's estimate in a given year
  #### towards last years proportion of the hunt occurring within that period
  ### time-series first difference model through years on the period-specific parameters
  alpha_p[1] <- 0 ## these are the log-scale intercepts of the proportions of the hunt in each period
  alpha_py[1,1] <- 0
  for(y in 2:nyears){
    alpha_py[1,y] ~ dnorm(alpha_py[1,y-1],tau_alpha_py[1])
  }
  tau_alpha_py[1] ~ dscaled.gamma(0.5,50)
  for(p in 2:nperiods){
    alpha_p[p] ~ dnorm(0,0.1) # fixed effect period-effects on total kill
    alpha_py[p,1] <- alpha_p[p]
     for(y in 2:nyears){
       alpha_py[p,y] ~ dnorm(alpha_py[p,y-1],tau_alpha_py[p]) # first difference model through time
     }
    tau_alpha_py[p] ~ dscaled.gamma(0.5,50)
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
  ## nparts_py (nwings by period and year) and w_psy (nwings by period, species, and year) are data
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
    
    # exponential transformation, just to monitor mean proportions by species
    for( s in 1:nspecies ){
      mu_ps[p,s] <- exp.alpha_ps[p,s]/sum(exp.alpha_ps[p,1:nspecies]) #mean proportional contribution of species during period
      exp.alpha_ps[p,s] <- exp(alpha_ps[p,s])
    }
    
    
  }#p

  #### multinomial dirichlet-prior, time-series model for the species composition
  
  alpha_s[1] <- 0
  
  for(p in 1:nperiods){
    taualpha_psy1[p] ~ dscaled.gamma(0.5,50) # first-year, year-effect variance 
  }
  for(s in 2:nspecies){
    alpha_s[s] ~ dnorm(0,0.1)   # species mean proportional contribution across years and periods
    #prior on the sd of alpha_psy[s] equal to the half-t distribution with sd = 2 and 4 degrees of freedom.  The density isflat for very small values (σS) and has a long flat tail for large values (σS).  Thesefeatures protect against mis-specification of the prior scaleS.  Choosing a largeSis harmlessas it simply makes the prior flatter, whereas ifSis too small then the long flat tail ensuresthat sufficient data will dominate the prior.  Gelman [2006] calls this a “weakly informative”prior.  For comparison, figure 10.1 also shows the density onσin the limit as the degrees offreedomdfincreases to infinity.  The distribution then becomes a half-normal with standarddeviationS.  The effect of increasing the degrees of freedom is to diminish the weight of thetail.
    
  }#s
  for(s in 1:nspecies){
    tau_alpha_psy[s] ~ dscaled.gamma(0.5,50) #year-effecct variance by species
    taualpha_s[s] ~ dscaled.gamma(0.5,50) # period variance by species
     
    for(p in 1:nperiods){
      alpha_ps[p,s] ~ dnorm(alpha_s[s],taualpha_s[s])
      alpha_psy1[p,s,1] ~ dnorm(0,taualpha_psy1[p])
      alpha_psy[p,s,1] <- alpha_ps[p,s] + alpha_psy1[p,s,1]
      
      for(y in 2:nyears){
        alpha_psy1[p,s,y] ~ dnorm(alpha_psy1[p,s,y-1],tau_alpha_psy[s]) 
        alpha_psy[p,s,y] <- alpha_ps[p,s] + alpha_psy1[p,s,y]
        
      }
      
    }#p
    
  }#s
 

  
  
  #####################################################################
  #### proportional distribution of age and sex by species
  #### axcomp_axsy[1:ndemog,s,y] = proportional distribution of age and sex classes by species and year
  
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
    ## mu_axs = mean across years of the species age-sex composition
    for( d in 1:ndemog ){
      mu_axs[d,s] <- exp.alpha_axs[d,s]/sum(exp.alpha_axs[1:ndemog,s]) #
      exp.alpha_axs[d,s] <- exp(alpha_axs[d,s])
    }#d
    
    
  }#s
  
  #### multinomial dirichlet-prior, time-series model for the age and sex (ducks) or age (geese) composition
  # 
  # 
     tau_alpha_ax ~ dscaled.gamma(0.5,50) #variance among species on the first-year parameter for demography
     

  for(s in 1:nspecies){
    tau_alpha_axsy[s] ~ dscaled.gamma(0.5,50) #variance of year-effects for the demographic parameters by species
    
    alpha_axs[1,s] <- 0 #fixed first demographic category = 0
 
    for(y in 1:nyears){
      alpha_axsy[1,s,y] <- alpha_axs[1,s]  #first demographic group is fixed at 0
    }#y
    
    for(d in 2:ndemog){
     
      alpha_axs[d,s] ~ dnorm(0,tau_alpha_ax)
      alpha_axsy[d,s,1] <- alpha_axs[d,s] # first year
      
      
      for(y in 2:nyears){
        alpha_axsy[d,s,y] ~ dnorm(alpha_axsy[d,s,y-1],tau_alpha_axsy[s]) 
      }#y
    }#d
    
  }#s
 
  
}## end of model

