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
    ## parrive = proportion of hunting values that were sampled in another province (i.e., the proportion of hunters hunting in prov that are not included in pops, 1+parrive = factor by which pops should be increased)
    ## pleave = proportion of permit population that hunted somewhere else (i.e, the proportion of the pops that are hunting somewhere else and should be removed, factor by which pops should be reduced)
    arrive_hunt_cf[y,1] ~ dbinom(parrive[y],arrive_hunt_cf[y,2])
    leave_hunt_cf[y,1] ~ dbinom(pleave[y],leave_hunt_cf[y,2])
    
    
    #priors for parrive and pleave simple beta distribution = beta-binomial sub-model
    alpha_pleave[y] ~ dunif(1,3)
    beta_pleave[y] ~ dunif(1,3)
    
    alpha_parrive[y] ~ dunif(1,3)
    beta_parrive[y] ~ dunif(1,3)
    
    parrive[y] ~ dbeta(alpha_parrive[y],beta_parrive[y])
    
    pleave[y] ~ dbeta(alpha_pleave[y],beta_pleave[y])
    
    prov_flow[y] <- parrive[y]-pleave[y] #difference in proportion of hunters coming and going, negative values indicate permits purchased in zone tend to hunt somewhere else
    
    #### corrections to account for the inter-provincial hunting, and the proportion of the permit-population that are active
  for(c in 1:ncastes){
    
    pops_cor[c,y] <- pops[c,y]+(pops[c,y]*(parrive[y]))-(pops[c,y]*pleave[y])
    
    
    nactive[c,y] ~ dbinom(pactive[c,y],npotential[c,y])
    NACTIVE_cy[c,y] <- pactive[c,y]*pops_cor[c,y] #NACTIVE_cy used to rescale all per-hunter values in the derived quantities
    
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
    kill[i] ~ dpois(lambda[i]) #kill is data - each hunter's estimate of the total number killed 
    lambda[i] <- lambda1[i]*z[i] + 0.00001 ## hack required for Rjags -- otherwise 'incompatible'-error
    z[i] ~ dbern(psi[year[i]]) #psi = proportion of non-zeros for each year = should be very similar to psucc, after removing the Poisson-related zeros
    log(lambda1[i]) <- elambda1[i] #
    elambda1[i] <- ann[year[i]] + cst[caste[i],year[i]] + hntr[caste[i],year[i],hunter[i]] + elambda_day[i] #elambda_day[i] acts as an offset on effort
    
    ## ann[y] is a yearly intercept for all species kill
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
  
  ### zip priors 
  ### psi is the estimated annual proportion of waterfowl hunters that hunt this group - i.e., the zero component that's in addition to the over-dispersed Poisson-distribution
  for(y in 1:nyears){
    psi[y] ~ dunif(0,1)
  }
  
  # dif_1_2[1] <- cst[1]-cst[2] #temporary parameters to assess the difference between castes D and B all results suggest significant differences
  # dif_1_2[2] <- cst_day[1]-cst_day[2]
  # 
  # dif_1_2[3] <- sdhunter[1]-sdhunter[2]
  # dif_1_2[4] <- sdhunter_day[1]-sdhunter_day[2]
  # 
  # dif_1_2[5] <- nu_day[1]-nu_day[2]
  # 
  
 CCST[1] <- 0
 CCST_day[1] <- 0
  for(c in 2:ncastes){
  CCST[c] ~ dnorm(0,0.001)
  CCST_day[c] ~ dnorm(0,0.001)
  }

 for(c in 1:ncastes){
   
 
     cst[c,1] <- CCST[c] #fixed value in first year, to help separately estimate the cst effects from ann
     cst_day[c,1] <- CCST_day[c] #fixed value in first year, to help separately estimate the cst_day effects from ann_day
   
   for(y in 2:nyears){  ### random effects for caste effects, allowing them to vary randomly by year
     
    cst[c,y] ~ dnorm(CCST[c],tau_cst[c])
    cst_day[c,y] ~ dnorm(CCST_day[c],tau_cst_day[c])
    }
    tau_cst[c] ~ dgamma(0.001,0.001) # assumption that the
    tau_cst_day[c] ~ dgamma(0.001,0.001)
    
  }
  
  
  for(c in 1:ncastes){
    ## harvest rate priors
    retrans_hunter[c] <- 0.5*(1/tauhunter[c])
    sdhunter[c] <- 1/pow(tauhunter[c],0.5)
    tauhunter[c] ~ dgamma(0.01,0.01)
    nu[c] ~ dgamma(2,0.2)
    ## caste specific intercept priors
   
    #activity (days) priors
    retrans_hunter_day[c] <- 0.5*(1/tauhunter_day[c])
    sdhunter_day[c] <- 1/pow(tauhunter_day[c],0.5)
    tauhunter_day[c] ~ dgamma(0.01,0.01)
    nu_day[c] ~ dgamma(2,0.2)
    ## caste specific intercept priors
    
    
    # mmu_psucc[c] ~ dnorm(0,1)
    # phi_psucc[c] ~ dgamma(0.001,0.001)
    # tau_mu_psucc[c] ~ dgamma(0.001,0.001)
    # 
    # mmu_pactive[c] ~ dnorm(0,1)
    # phi_pactive[c] ~ dgamma(0.001,0.001)
    # tau_mu_pactive[c] ~ dgamma(0.001,0.001)
    # 
    
    for(y in 1:nyears){
      
      alpha_psucc[c,y] ~ dunif(1,3) #very simple priors for the parameters of the beta priors on pactive and psucc
      beta_psucc[c,y] ~ dunif(1,3)
      alpha_pactive[c,y] ~ dunif(1,3)
      beta_pactive[c,y] ~ dunif(1,3)
      

      #alternative priors that are no neccessary here but might help with some time-series or other assumption to model these proportions
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
        ## hntr[c,y,h] is an individual random error term to allow for overdispersion in the harvest,
        ##  hntr_day[c,y,h] is an individual random error term to allow for overdispersion in the activity (days spent hunting),
        ##  t-distribution used for overdispersion in activity adn harvest to allow for heavy-tails
        ## all after accounting for zero-inflation
        ## variances on both activity and harvest are caste-specific precision to allow hunter-level dispersion to vary among castes
        ## 
        
        hntr_day[c,y,h] ~ dt(0,tauhunter_day[c],nu_day[c])
        hntr[c,y,h] ~ dt(0,tauhunter[c],nu[c])
        #hntr[c,y,h] ~ dnorm(0,tauhunter[c]) #alternative normal overdispersion assuming that most of variance in harvest is a function of how many days a hunter spends hunting
        #n days probably accounts for a bit of the hunting skill effect as well as the activity effect
        
      }#h
    }#y
    
  }#c
  
  ### yearly intercepts of total kill by first-difference 
  ### alternative structure to allow first-differenc time-series model, but appears to be uneccessary and identifiability issues with caste time-series
  ann[1] ~ dnorm(0,0.001) # fixed effects for year-1 annual harvest level
  ann_day[1] ~ dnorm(0,0.001) # fixed effect for year-1 annual activity level
  
  for(y in 2:nyears){
    ann[y] ~ dnorm(ann[y-1],tauyear)
    ann_day[y] ~ dnorm(ann_day[y-1],tauyear_day)
    
    #ann[y] ~ dnorm(0,0.001)
    #ann_day[y] ~ dnorm(0,0.001)
    
  }
  
  # first-difference harvest and activity variance priors
   sdyear <- 1/pow(tauyear,0.5)
 tauyear ~ dgamma(0.001,0.001)
 sdyear_day <- 1/pow(tauyear_day,0.5)
 tauyear_day ~ dgamma(0.001,0.001)
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
      mean_totkill_yc[y,c] <- exp(ann[y] + cst[c,y] + ann_day[y] + cst_day[c,y] + retrans_hunter_day[c] + retrans_hunter[c]) *psi[y]
      mean_totdays_yc[y,c] <- exp(ann_day[y] + cst_day[c,y] + retrans_hunter_day[c])
      

    }#y
  }#c
  
  

  #total harvest estimate by species year and caste
  ## mean harvest and days * population sizes of each caste and year * esimated proportion of population that is active to generate final harvest estimates.
  for(y in 1:nyears){
   
    for(c in 1:ncastes){
      kill_cy[c,y] <- mean_totkill_yc[y,c]*NACTIVE_cy[c,y] #total kill by caste and year
      days_cy[c,y] <- mean_totdays_yc[y,c]*NACTIVE_cy[c,y] #total days by caste and year
      
}
    
    #### summed total harvest and activity (e.g., all ducks) across all species and castes in a given year
    kill_y[y] <- sum(kill_cy[castes,y])
    days_y[y] <- sum(days_cy[castes,y])
    
  }#y
  
  
 
  
}## end of model

