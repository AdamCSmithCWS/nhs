# Script to prepare zonal jags data for full species harvest model for other game birds -------------
### this script cannot be directly run because the input data are not included
#### Input data cannot be shared due to privacy concerns
### The anonymized data that are included in the jdat lists created
### by this script are included in the archived repo, so the models can be run


Y <- 2019
FY = 1976
years <- FY:Y

names(years) <- paste(years)

library(foreign)
library(runjags)
library(rjags)
library(jagsUI)
library(tidyverse)
library(ggmcmc)
library(tidybayes)
library(ggrepel)
library(ggforce)
library(doParallel)
library(foreach)

#load.module("glm") 



# load output from data_prep.R --------------------------------------------


load(paste0("data/parts and harvest survey info",Y,".RData"))

provzone <- read.csv("data/Province and zone table.csv")


# load published estimates by zone ----------------------------------------

pubEsts_simple = read.csv("data/enp_nhs_a_by_zone_20200416.csv",stringsAsFactors = F)
names(pubEsts_simple) <- c("var","name","prov","zone","resid","year","mean","sd")
pubEsts_simple$lci = ceiling(pubEsts_simple$mean-(1.96*pubEsts_simple$sd))
pubEsts_simple$uci = ceiling(pubEsts_simple$mean+(1.96*pubEsts_simple$sd))
pubEsts_simple[which(pubEsts_simple$lci < 0),"lci"] <- 0


pubEsts_species = read.csv("data/enp_nhs_b_by_zone_20200416.csv",stringsAsFactors = F)
names(pubEsts_species) <- c("sp","species","prov","zone","year","mean","sd")
pubEsts_species$lci = ceiling(pubEsts_species$mean-(1.96*pubEsts_species$sd))
pubEsts_species$uci = ceiling(pubEsts_species$mean+(1.96*pubEsts_species$sd))
pubEsts_species[which(pubEsts_species$lci < 0),"lci"] <- 0

species_web_names = unique(pubEsts_species[,c("sp","species")])


pubEsts_age_sex = read.csv("data/enp_nhs_c_by_zone_20200416.csv",stringsAsFactors = F)
names(pubEsts_species) <- c("sp","species","prov","zone","year","age_ratio")






######## sampling population sizes during year of survey
popsiz_s = merge(popsiz,provzone[,c("prov","provn")],by.x = "PRSAMP",by.y = "provn",all.x = T)
popsiz_s = unique(popsiz_s)



#### total number of permits sold in each year and sampling zone
### True population sizes

popsiz_perm <- read.csv("data/popsiz_perm_total_permits_sold_by_zone_sample_caste.csv")

### total number of permits by zone and year

z_pops <- read.csv("data/z_pops_total_permits_sold_by_zone.csv")

### species lists

aou.ducks <- sps[which(sps$group == "duck"),"AOU"]
aou.goose <- sps[which(sps$group == "goose"),"AOU"]
aou.murre <- sps[which(sps$group == "murre"),"AOU"]




outscse <- read.csv("data/outscse_spec_comp_survey_data.csv")


for(spgp in c("duck","goose","murre")){
### begining of loop through provinces only engage this loop if running the full analysis
### for a single province and zone, skip the next 4 lines
### and enter something like the following (e.g., to run Ontario-zone 3)

# group data set up -------------------------------------------------------

  
  
  if(spgp == "goose"){
    
    Y <- 2019
    FY = 1976
    years <- FY:Y
    
    names(years) <- paste(years)
    
    aou.spgp = aou.goose
    period = period.goose
    cal.spgp = calg
    allkill = allkill
    phunt = "PRHUNTG"
    zhunt = "ZOHUNTG"
    wkill = "TOGOK"
    wact = "ACTIVEWF"
    wsucc = "SUTOGO"
    wday = "DAYWF"
    nyears = length(years)
    demog = data.frame(BSEX = rep(c("U","U"),each = 1),
                       BAGE = rep(c("A","I"),times = 1),
                       stringsAsFactors = F)
    minyr <- min(years)
    provs2 <- provs
    non_res_combine = c("NF 1","NF 2","PE 1","NS 1","NS 2","BC 2","NT 1","YT 1","NB 1")
    
    
  }
  if(spgp == "duck"){
    
    Y <- 2019
    FY = 1976
    years <- FY:Y
    
    names(years) <- paste(years)
    aou.spgp = aou.ducks
    period = period.duck
    cal.spgp = cald
    allkill = allkill
    phunt = "PRHUNT"
    zhunt = "ZOHUNT"
    wkill = "TODUK"
    wact = "ACTIVEWF"
    wsucc = "SUTODU"
    wday = "DAYWF"
    
    nyears = length(years)
    demog = data.frame(BSEX = rep(c("F","M"),each = 2),
                       BAGE = rep(c("A","I"),times = 2),
                       stringsAsFactors = F)
    minyr <- min(years)
    provs2 <- provs
    non_res_combine = c("NF 1","NF 2","PE 1","NS 1","NS 2","BC 2","NT 1","YT 1")
    
  }
  
  
  
  if(spgp == "murre"){
    
    Y <- 2019
    FY = 2014#### previous years Murre harvest was calculated differently, pre 2013 only total MURRK, and in 2013 it was a mix of infor from DAYOT and calendars and species composition
    years <- FY:Y
    
    names(years) <- paste(years)
    
    aou.spgp = aou.murre
    period = period.murre
    cal.spgp = calm
    allkill = allkill
    phunt = "PRHUNTM"
    zhunt = "ZOHUNTM"
    wkill = "MURRK"
    wact = "ACTIVEM"
    wsucc = "SUCCM"
    wday = "DAYM" #?
    nyears = length(years)
    demog = data.frame(BSEX = rep(c("U","U"),each = 1),
                       BAGE = rep(c("A","I"),times = 1),
                       stringsAsFactors = F)
    minyr <- FY
    provs2 = "NF"
        non_res_combine = c("NF 1","NF 2","PE 1","NS 1","NS 2","BC 2","NT 1","YT 1")
    
  }
  
  
  
  
  for(pr in provs2){
  
  
  
  zns <- unique(period[which(period$pr == pr),"zo"])
  

  for(z in zns){
    
    
    # periods -----------------------------------------------------------------
    
    
    periods <- period[which(period$pr == pr & period$zo == z),]
    sumkill = allkill[which(allkill[,phunt] == pr &
                              allkill[,zhunt] == z &
                              allkill$YEAR %in% years),]
    
    sumkill$year = sumkill$YEAR-(minyr-1)
    
    
    nperiods <- max(periods$period)
    
    
    
    prts1 <- outscse[which(outscse$PRHUNT == pr &
                             outscse$ZOHUNT == z &
                             outscse$AOU %in% aou.spgp &
                             outscse$YEAR %in% years),]
    
    luni <- function(x){
      out <- length(unique(x))
    }
    yrspersp <- tapply(prts1$YEAR,prts1$AOU,luni)
    
    # removing species that only show up in <half of years --------------------
    prts1 <- prts1[which(prts1$AOU %in% names(yrspersp)[which(yrspersp > 2)]),]
    
    
    for(per in periods$period){
      sy <- periods[which(periods$period == per),"startweek"]
      ey <- periods[which(periods$period == per),"endweek"]
      prts1[which(prts1$WEEK %in% c(sy:ey)),"period"] <- per
    }
    
    prdspersp <- tapply(prts1$period,prts1$AOU,luni)#number of periods a species has been observed
    #could use the above values and the line below to remove species
    # that only appear in very few periods
    #prts1 <- prts1[which(prts1$AOU %in% names(prdspersp)[which(prdspersp > 4)]),]
    
    prts1$spfact = factor(prts1$AOU)
    prts1$spn = as.integer(prts1$spfact)
    nspecies <- max(prts1$spn)
    
    #nyears <- length(min(prts1$YEAR,na.rm = T):max(prts1$YEAR,na.rm = T))
    partsarray <- array(data = 0,dim = c(nperiods,nspecies,nyears))
    ndemog = nrow(demog)
    
    agesexarray <- array(data = 0,dim = c(ndemog,nspecies,nyears))
    
    
    sp.save = unique(prts1[,c("PRHUNT","ZOHUNT","AOU","spfact","spn")])
    sp.save[,"PRHUNT"] <- as.character(sp.save[,"PRHUNT"])
    sp.save[,"spfact"] <- as.character(sp.save[,"spfact"])
    # 
    
    for(sp in 1:nspecies){
      
      for(y in 1:nyears){
        yr <- y+(minyr-1)
        
        for(per in 1:nperiods){
          
          partsarray[per,sp,y] <- nrow(prts1[which(prts1$period == per & prts1$spn == sp & prts1$YEAR == yr),])
          
          
        }#per
        
        if(spgp == "duck"){
          for(dg in 1:ndemog){
            ag <- demog[dg,"BAGE"]
            sx <- demog[dg,"BSEX"]
            
            agesexarray[dg,sp,y] <- nrow(prts1[which(prts1$BAGE == ag & prts1$BSEX == sx & prts1$spn == sp & prts1$YEAR == yr),])
          }#dg
        }
        if(spgp %in% c("murre","goose")){ ### lumps all sexes including unknowns just tracks ages
          for(dg in 1:ndemog){
            ag <- demog[dg,"BAGE"]
            
            agesexarray[dg,sp,y] <- nrow(prts1[which(prts1$BAGE == ag & prts1$spn == sp & prts1$YEAR == yr),])
          }#dg
        }
        
        
      }#y
      
      
    }#sp
    
    
    
    sp.save.out = sp.save
    
    
    
    
    
    
    
    
    
    #########################
    ### 
    
    # compiling calendar info -------------------------------------------------
    
    
    ## generate an array 
    ## periodkill[p,y,h] = total kill in period-x and year-y for each hunter-h
    ## nhunter_y[y] = number of hunters with calendar information in year-y
    
    
    nhunter_y = vector(length = nyears)
    names(nhunter_y) = as.character(years)
    
    for(y in years){
      
      tmp <- cal.spgp[[as.character(y)]]
      tmp1 <- tmp[which(tmp$PRHUNT == pr &
                          tmp$ZOHUNT == z),]
      nhunter_y[as.character(y)] <- length(unique(tmp1$PERMIT))
      
      
      # ### identify earliest day in the hunting season across all years
      # tmp1[which(tmp1$MONH >12),"MONH"] = tmp1[which(tmp1$MONH >12),"MONH"]-12
      # tmp1[which(tmp1$MONH < 9),"MONH"] = NA
      # tmp1$date = as.Date(paste(tmp1$MONH,
      #                           tmp1$DAYH,sep = "-"),
      #                     format = "%m-%d")
      # 
      # 
      # 
      # if(y == years[[1]]){
      #   min_day = min(tmp1$date,na.rm = TRUE)
      # }else{
      #   min_day = min(c(min_day,min(tmp1$date,na.rm = TRUE)))
      # }
      # 
    }
    min_day <- "09-01" ### No hunting in August, so all week definitions begin on September 1
              #substr(as.character(min_day),start = 6,stop = nchar(as.character(min_day)))
    
    periodkill = array(0,
                       dim = c(nperiods,nyears,max(nhunter_y)))
    ## the following loop is clumsy and slow, but it works
    NAcounts = list()
    length(NAcounts) <- nyears
    
    for(yn in 1:nyears){
      y = years[yn]
      tmp <- cal.spgp[[as.character(y)]]
      tmp1 <- tmp[which(tmp$PRHUNT == pr &
                          tmp$ZOHUNT == z),]
      tmp1[which(tmp1$MONH >12),"MONH"] = tmp1[which(tmp1$MONH >12),"MONH"]-12
      tmp1$yearhunt = tmp1$YEAR
      tmp1[which(tmp1$MONH < 9),"yearhunt"] = tmp1[which(tmp1$MONH < 9),"YEAR"]+1
      tmp1$date = as.Date(paste(tmp1$yearhunt,
                                tmp1$MONH,
                                tmp1$DAYH,sep = "-"),
                          format = "%Y-%m-%d")
      
      if(any(is.na(tmp1$date))){
        for(jj in which(is.na(tmp1$date))){
          ### if day is 31, then it's likely that the month doesn't have 31 days
          ### fix below assumes that the month is correct and the day is wrong
          if(tmp1[jj,"DAYH"] == 31){ tmp1[jj,"DAYH"] <- 30 
          tmp1[jj,"date"] <- as.Date(paste(tmp1$yearhunt[jj],
                                           tmp1$MONH[jj],
                                           tmp1$DAYH[jj],sep = "-"),
                                     format = "%Y-%m-%d")
          }
          if(tmp1[jj,"DAYH"] == 29 & tmp1[jj,"MONH"] == 2){ tmp1[jj,"DAYH"] <- 28 
          tmp1[jj,"date"] <- as.Date(paste(tmp1$yearhunt[jj],
                                           tmp1$MONH[jj],
                                           tmp1$DAYH[jj],sep = "-"),
                                     format = "%Y-%m-%d")
          }
        }
      }
      
      min_day_y <- as.Date(paste(y,min_day,sep = "-"),format = "%Y-%m-%d")
      
      tmp1$week = as.integer(ceiling((tmp1$date-(min_day_y-1))/7))
      
      tmp1$hunterf = as.integer(factor(tmp1$PERMIT))
      
      if(any(is.na(tmp1$COUNT))){
        
        NAcounts[[yn]] <- tmp1
        tmp1[which(is.na(tmp1$COUNT)),"COUNT"] <- 1 ## decision to include a non-zero value because the rows include all of the relevant information (hunter, day, prov, week permit etc, just missing hte count, likely that the program in those few years made a mistake)
      }
      
      for(h in 1:nhunter_y[yn]){
        tmp2 = tmp1[which(tmp1$hunterf == h),]
        for(per in 1:nperiods){
          wks1 = periods[which(periods$period == per),"startweek"]
          wks2 = periods[which(periods$period == per),"endweek"]
          wt2 = which(tmp2$week %in% c(wks1:wks2))
          if(length(wt2) == 0){next}
          periodkill[per,yn,h] <- round(sum(tmp2[wt2,"COUNT"]))
          
        }#per
      }#h
    }#yn
    
    ### below can be commented out but prints the 
    ### observed proportional composition of the hunt by years (rows) and periods (columns)
    ### helps for checking that there are no missing data
    for(y in 1:nyears){
    print(round(rowSums(periodkill[,y,],na.rm = F)/sum(rowSums(periodkill[,y,],na.rm = F)),3))
    if(any(is.na(round(rowSums(periodkill[,y,],na.rm = F)/sum(rowSums(periodkill[,y,],na.rm = F)),3)))){print(y)}
      }
    
    
    
    
    
    # Correction factors for inter-provincial hunting -------------------------
    # the population sizes of permits are based on the sampling process:
    # historically hunters were sampled based on where the permit was purchased. THis was used as the best estimate of where they did their hunting
    # recently, many hunters are sampled based on where they indicate, at the time of purchase, they will do most of their hunting. This is necessary for online purchases, and a better approach for all permits
    # These corrections calculate the proportion of the permits sampled from this local population that hunted somewhere else (leave_cf)
    # and the proportion of the permits that are hunting in this zone which were sampled from another zone.
    # as there is more sampling from online permits, the population sizes will converge.
    # this correction factor is an improvement over the previous model because it treats all hunters actively hunting in this zone the same way (asumes activity and mean harvests are the same, regardless of where they're sampled)
    # in a few zones, this different correction makes a significant difference in the number of successful, active, and harvest totals.
    # These differences (ON 2, especially) happen to coincide with a change in teh sampling rates (~2001)
    
    
    
    
    
    sumkillall = allkill[which(((allkill[,phunt] == pr &
                                   allkill[,zhunt] == z)|(allkill[,"PRSAMP"] == pr &
                                                            allkill[,"ZOSAMP"] == z)) &
                                 allkill$YEAR %in% years),]
    
    arrive_hunt_cf <- matrix(1,nrow = nyears,ncol = 2)
    leave_hunt_cf <- matrix(1,nrow = nyears,ncol = 2)
    
    sumkillall$huntpr <- FALSE
    sumkillall$samppr <- FALSE
    
    sumkillall[which(sumkillall[,phunt] == pr &
                       sumkillall[,zhunt] == z),"huntpr"] <- TRUE
    sumkillall[which(sumkillall[,"PRSAMP"] == pr &
                       sumkillall[,"ZOSAMP"] == z),"samppr"] <- TRUE
    
    
    sumkillout = allkill[-which(allkill[,"PRSAMP"] == pr &
                                  allkill[,"ZOSAMP"] == z),]
    sumkillout = sumkillout[which(sumkillout$YEAR %in% years),]
    sumkillout_huntpr <- sumkillout[which(sumkillout[,phunt] == pr &
                                            sumkillout[,zhunt] == z),]
    
    sumkillout_huntout <- sumkillout[-which(sumkillout[,phunt] == pr &
                                              sumkillout[,zhunt] == z),]
    n_out_huntz <- sumkillout_huntpr %>% 
      group_by(PRSAMP,ZOSAMP,YEAR) %>% 
      summarise(nperms_in = n())
    n_out_nohuntz <- sumkillout_huntout %>% 
      group_by(PRSAMP,ZOSAMP,YEAR) %>% 
      summarise(nperms_out = n())
    
    n_in_out <- left_join(n_out_nohuntz,n_out_huntz)
    
    n_in_out <- left_join(n_in_out,z_pops)
    if(any(is.na(n_in_out$nperms_in))){
      n_in_out[which(is.na(n_in_out$nperms_in)),"nperms_in"] <- 0
    }
    n_in_out$prz <- paste(n_in_out$PRSAMP,n_in_out$ZOSAMP,sep = "_")
    przs <- unique(n_in_out$prz)
    n_alt_zones <- length(przs)
    n_in_out$nextra <- as.integer(round((n_in_out$nperms_in/n_in_out$nperms_out)*n_in_out$TOTSALE)) #number of extra permits to add to local population based on the proportion of sampled permits in each year and zone(other zones only) that hunterd in this zone
    
    n_in_out_y <- n_in_out %>% 
      group_by(YEAR) %>% 
      summarise(nextra = sum(nextra))
    
    # vvs = c("nperms_out","nperms_in","TOTSALE")
    # 
    # arrive_array <- array(data = 0,dim = c(nyears,n_alt_zones,3))
    # for(y in 1:nyears){
    # yn = years[y]
    # for(zz in 1:n_alt_zones){
    #   zzn = przs[zz]
    #   for(nn in 1:length(vvs)){
    #     vv = vvs[nn]
    #     tmp = n_in_out[which(n_in_out$YEAR == yn & n_in_out$prz == zzn),vv]
    #     arrive_array[y,zz,nn] <- as.integer(tmp)
    #   }
    #
    #   }}
    
    
    
    
    for(y in years){
      yi = y-(FY-1)
      tmp = table(sumkillall[which(sumkillall$YEAR == y),c("samppr","huntpr")])
      
      
      nsampprov = sum(tmp["TRUE",])#number of hunters sampled in that prov/zone (ratio of this to population = simple extrapolation factor)
      #nhuntprov = sum(tmp[,"TRUE"])#number of hunters hunting in that prov/zone
      #nhunt_samp_prov = sum(tmp["TRUE","TRUE"])
      if(sum(dim(tmp)) == 4){
        nsampprov_huntaltprov = tmp["TRUE","FALSE"]
      }else{
        if(!("FALSE" %in% dimnames(tmp)$huntpr)){
          nsampprov_huntaltprov <- 0 
        }else{
          nsampprov_huntaltprov = tmp["TRUE","FALSE"] 
        }
        
        
      }
      leave_hunt_cf[yi,1] <- nsampprov_huntaltprov
      leave_hunt_cf[yi,2] <- nsampprov
      
      
    }
    
    
    # collecting and sorting the total kill by caste for the zone -------------
    
    
    if(paste(pr,z) %in% non_res_combine){
      
      #combines castes A and E into resident non-renewal hunters
      #### for 8 zones this is necessary because there are very few non-resident hunters
      ##### now with sharing of info through time, this is worth reconsidering...
      
      sumkill[which(sumkill$CASTE == "E"),"CASTE"] <- "A" 
      sumkill$caste = factor(as.character(sumkill$CASTE),ordered = T,levels = c("D","B","A")) #D-renewal > 1year, B-renewal = 1year, A-nonrenewal (new hunter) plus the few nonresidents
      
    }else{
      sumkill$caste = factor(as.character(sumkill$CASTE),ordered = T,levels = c("D","B","A","E")) #D-renewal > 1year, B-renewal = 1year, A-nonrenewal (new hunter), E-nonresident
      
    }
    
    if(any(is.na(sumkill$caste))){
      sumkill <- sumkill[which(!is.na(sumkill$caste)),]
    }
    castes = 1:max(as.integer(sumkill$caste)) #
    
    # population sizes (number of permits in each caste and year) active potential and successful--------------------------------------------------------
    #pops[c,y]
    pops = matrix(0,nrow = max(castes),ncol = nyears)
    
    for(cc in castes[(3:length(castes))]){ # loops through the castes A and E(if present)
      for(y in 1:nyears){
        yn = as.integer(substr(as.character(years[y]),3,4))
        pops[cc,y] <- popsiz_s[which(popsiz_s$SAMPLE == levels(sumkill$caste)[cc] & popsiz_s$YEAR == yn &
                                       popsiz_s$prov == pr & popsiz_s$ZOSAMP == z),"TOTPERM"]
        
      }
    }
    
    cfact = matrix(NA,nrow = 2,ncol = nyears) ## matrix of the yearly proportion of RESREN hunters that were drawn from caste D (row = 1) and B (row = 2) 
    ### correction factors for RESREN hunters
    for(y in 1:nyears){
      permpop = popsiz_perm[which(popsiz_perm$SAMPLE == "B" & popsiz_perm$YEAR == as.character(years[y]) &
                                    popsiz_perm$prov == pr & popsiz_perm$ZOSALE == z),"TOTSALE"] ## total number of renewal permits sold in the zone (sum of B and D) - true known numbers from this years permit file
      for(cc in castes[1:2]){ # loops through the castes D and B
        yn = as.integer(substr(as.character(years[y]),3,4))
        tmpnum = popsiz_s[which(popsiz_s$SAMPLE == levels(sumkill$caste)[cc] & popsiz_s$YEAR == yn &
                                  popsiz_s$prov == pr & popsiz_s$ZOSAMP == z),"TOTPERM"] #total estimated permits in the caste[cc] (info from last years and this years permit file)
        tmpdenom = sum(popsiz_s[which(popsiz_s$SAMPLE %in% levels(sumkill$caste)[1:2] & popsiz_s$YEAR == yn &
                                        popsiz_s$prov == pr & popsiz_s$ZOSAMP == z),"TOTPERM"]) # total estimate permits in castes B and D (infor from last years and this years file)
        
        
        cfact[cc,y] = tmpnum/tmpdenom #ratio of estimated permits in caste[cc]/caste[B and D]
        
        pops[cc,y] <- as.integer(round(permpop*cfact[cc,y],0)) # new division of known renewal hunters into castes B and D
        #### this could be a component of the model...
        
      }
      #print(permpop - sum(pops[1:2,y]))
    }
  

# arrival corrections -----------------------------------------------------

      
    n_arrive = matrix(0,nrow = length(castes),ncol = nyears)
    for(y in 1:nyears){
      for(cc in 1:length(castes)){
        n_arrive[cc,y] <- as.integer(round(n_in_out_y[which(n_in_out_y$YEAR == years[y]),"nextra"]*(pops[cc,y]/sum(pops[,y]))))#splitting the number of arriving hunters based on teh yearly distribution of the castes
      }
    }
    
    
    
    # separating active and inactive ------------------------------------------
    
    npotential <- as.matrix(table(sumkill$caste,sumkill$year))
    succ = sumkill[which(sumkill[,wsucc] == "Y"),]
    nsucc <- as.matrix(table(succ$caste,succ$year))
    
    if(pr %in% c("YT","NT")){
      nsucc <- (table(sumkill[,wsucc],sumkill$caste,sumkill$year))
      nsucc <- nsucc["Y",,]
    } 
    
    sumkill_active = sumkill[which(sumkill[,wact] == "Y"),]
    
    #### insert 0 for all NA-kill values and active hunters (applies to active WF hunters with NA values for geese)
    
    if(any(is.na(sumkill_active[,wkill]) & spgp == "goose")){
      sumkill_active[which(is.na(sumkill_active[,wkill])),wkill] <- 0
    }
    ### this bit is different for the 1994-1999 range and the 2000 onwards. The NAs make up 2/3rds of hte rows in 1994-1999, and only 1/3 from 2000 onwards
    
    
    
    nactive <- as.matrix(table(sumkill_active$caste,sumkill_active$year))
    
    if(any(sumkill_active[,wday] < 1 & spgp == "murre")){
      sumkill_active[which(sumkill_active[,wday] < 1),wday] <- sumkill_active[which(sumkill_active[,wday] < 1),"DAYOT"]
    }
    
    if(any(nsucc > nactive)){break("number successful > number active, problem with the data")}
    
    
    
    caste = as.integer(sumkill_active[,"caste"])
    year = sumkill_active[,"year"]
    kill = sumkill_active[,wkill]
    nhs = nrow(sumkill_active)
    days = sumkill_active[,wday]
    
    if(any(days < 1) | any(is.na(days))){
      mday_per_kill <- sum(days[which(days > 0)])/sum(kill[which(days > 0)],na.rm = T)
      days[which(days == 0 | is.na(days))] <- ceiling(mday_per_kill*(kill[which(days == 0 | is.na(days))]+1))
    }
    
    days = ceiling(days)
    
    clsw = which(names(sumkill_active) %in% c(wkill,wday,"year","caste"))
    
    if(any(days < 1)){break("number of days includes zeros for Active hunters")}
    
    #nhunter_cy[c,y] #number of active hunters by caste and year
    nhunter_cy = matrix(0,nrow = max(castes),ncol = nyears)
    
    for(y in 1:nyears){
      
      for(c in castes){
        ww = which(sumkill_active$year == y & sumkill_active$caste == levels(sumkill_active$caste)[c])
        if(length(ww) == 0){print(paste("no hunter responses in caste",c,"year",y,pr,z))}
        sumkill_active[ww,"hunter_n_cy"] <- as.integer(factor(sumkill_active[ww,"PERMIT"]))
        if(length(ww) == 0){
          nhunter_cy[c,y] <- 1 ## minimum number of hunters = 1 to avoid indexing errors in the jags model
          
        }else{
          nhunter_cy[c,y] <- max(sumkill_active[ww,"hunter_n_cy"])
          
        }
        
      }
    }
    
    
    hunter_n_cy = sumkill_active$hunter_n_cy
    
    
    
    
    # total number of parts by year and period --------------------------------
    
    
    nparts_py = matrix(nrow = nperiods,
                       ncol = nyears)
    for(p in 1:nperiods){
      for(y in 1:nyears){
        nparts_py[p,y] <- sum(partsarray[p,,y],na.rm = T)# yearl and period sums of all parts
      }
    }
    

    # total number of parts by species and period --------------------------------
    
    
    nparts_sy = matrix(nrow = nspecies,
                       ncol = nyears)
    for(s in 1:nspecies){
      for(y in 1:nyears){
        nparts_sy[s,y] <- sum(agesexarray[,s,y],na.rm = T)# yearl and species sums of all parts for age and sex
      }
    }
    
    
    # total harvest by year and hunter ----------------------------------------
    
    
    nkill_yh = matrix(nrow = nyears,
                      ncol = max(nhunter_y))
    for(y in 1:nyears){
      for(h in 1:nhunter_y[y]){
        nkill_yh[y,h] <- sum(periodkill[,y,h],na.rm = T) #simple sum of the data
      }
    }
    
    if(ndemog == 4){
      demof <- c(1,2)
      demoa <- c(1,3)
    }else{
      demof <- 1
      demoa <- 1
    }
    
    
    
    
    # compiling JAGS data object ----------------------------------------------
    
    
    jdat = list(pops = pops, # pops[c.y] total populations of permits by caste and year used to rescale all perhunter estimates to totals 
                #component that estimates p_active etc. to generate totals of active and successful hunters by year
                nactive = nactive, # nactive[c,y] number of active hunters by caste and year
                npotential = npotential, # npotential[c,y] number of potential hunters (respondents who bought a permit this year) by caste and year
                nsucc = nsucc, # nsucc[c,y] number of successful hunters by caste and year (active hunters with harvest > 0)
                #spcies composition components
                w_psy = partsarray, # w_psy[nperiods,nspecies,nyears] wings by period species year
                nparts_py = nparts_py, # nparts_py[nperiods,nyears] sum parts across species by period and year
                nparts_sy = nparts_sy, # nparts_sy[nspecies,nyears] sum parts species and year
                kill_pyh = periodkill, # kill_pyh[nperiods,nyears,max(nhunters[y])] hunter-level total harvest by period from the calendars(separate hunter id caste doesn't matter)
                nkill_yh = nkill_yh, # nkill_yh[nyears,max(nhunters[y])] hunter-level summed harvest from calendar (separate hunter id caste doesn't matter)
                # demographic data for age and sex component of the model
                w_axsy = agesexarray, # w_axsy[ndemog,nspecies,nyears] wings by age-sex species year
                #indicators
                ndemog = ndemog, # 2 if geese (A and I) 4 if ducks (AF, IF, AM, IM) number of demographic units (age-sex combinations)
                nspecies = nspecies, # integer length = 1 number of species
                nyears = nyears, #integer length = 1 number of years
                nperiods = nperiods, # integer length = 1 number of periods
                nhunter_y = nhunter_y, # nhunter_y[nyears] number active hunters by year
                nhunter_cy = nhunter_cy, # nhunter_cy[castes,nyears] number active hunters by caste and year
                ncastes = max(castes), # castes (numeric, 1:4)
                castes = castes, # castes (numeric, 1:4)
                nhs = nhs, # integer length = 1 number of active hunters over all years (nrow for sumkill_active)
                n_alt_zones = n_alt_zones,#number of zones other than this one
                #main data for overall harvest estimates
                hunter = hunter_n_cy, # vector(length = nhs) unique numeric indicator for active hunters by caste and year 
                kill = kill, # vector(length = nhs), total group (ducks, geese, murres) harvest of nhs response
                year = year, # vector(length = nhs), year of response
                caste = caste, # vector(length = nhs), caste of response
                days = days, #vector(length = nhs), number of days spent hunting
                n_arrive = n_arrive,#matrix nrow = ncastes, ncol = nyears, number of permits to add to the local populations based on the permist sampled outside of this zone but hutning here
                leave_hunt_cf = leave_hunt_cf,
                demof = demof,
                demoa = demoa)#
    
    
    
    
    
    save(list = c("jdat","sp.save.out"),
         file = paste("data/data",pr,z,spgp,"save.RData",sep = "_"))
    
    
  }#z
  
}#pr



}#spgp

