#### why do we care where the permits were purchased?
## it's only because of the archaic sampling system.
## if we know where the hunters plan to hunt, that's a better way to stratify them
## and then we only need to generate estimates for a single stratification (hunter-type by hunt-strata)
## and avoids the need to poststratify after the analysis (e.g., generate estimates for hunters purchasing in SK who hunted in AB)
### complicates the extrapolation of harvest into the past when hunters were stratified by zone of sale
### will require re-calculation of population sizes and extrapolation factors using historical data
### or, an assumption that since almost all hunters hunt in the zone of sale, then it won't make a significant difference to the 
### time-series if we just change the definition of the strata going forward.
### although, should we consider out-of-prov hunters as non-residents? Do they share hunting behaviours more with US-res or CAN-res?


#### eventually, we should produce response-level and part-level predictions
#### so that we can generate spatial estimates of hunting activity and harvest rates

##### 
## generalize this so that it applies across groups (geese, murres, ducks, as well as single species)

## broaden the model to include all zones at once
## share information among neighbouring regions on the species composition in a given year
## share information between years using a random-walk structure where the means in a given year are a function of last year's means
## also include model for age and sex ratios


#############
## these next few lines can be run
## after the various local folders have been adjusted



###############################################
###############

## need to remove the scaled gamma and the glm module
## this may remove the tendency to get the inconsistent with parents errors

## also

## add the relevant sex and age ratios as explicit calculations
# AGE RATIO: IMMATURES/ADULT: 
#   The ratio corresponds to the number of immatures per adult bird in the sample. Ratios were calculated if the total sample equals or exceeds 20 parts.
# 
# SEX RATIO: MALES PER FEMALE: 
#   The ratio corresponds to the number of males per female bird in the sample. Ratios were calculated if the total sample equals or exceeds 20 parts.


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

# compile total harvest estimates into a dataframe ------------------------

### compile total harvest estimates into a dataframe of
#### permit, year, caste, totalkill
 
cls = c("PERMIT",
   "CASTE",
   "YEAR",
   "SELYEAR",
   "PRHUNT",
   "ZOHUNT",
   "LATD",
   "LOND",
   "TODUK",
   "TOGOK",
   "COOTK",
   "WOODK",
   "SNIPK",
   "DOVEK",
   "PIGEK",
   "CRANK",
   "RAILK",
   "MURRK",
   "RNDMURK",
   "DAYWF",
   "DAYOT",
   "DAYM",
   "PRHUNTG",
   "ZOHUNTG",
   "LATG",
   "LONG",
   "PRHUNTM",
   "ZOHUNTM",
   "LATM",
   "LONM",
   "SUCCWF",
   "SUTODU",
   "SUTOGO",
   "SUCCOT",
   "SUCCM",
   "ACTIVEOT",
   "ACTIVE",
   "ACTIVEWF",
   "ACTIVEM",
   "POTNTL",
   "PRSALE",
   "ZOSALE",
   "PRSAMP",
   "ZOSAMP")
allkill <- NULL

for(y in years){
  tmp1 <- harvw[[as.character(y)]]
  tmp <- tmp1[,which(names(tmp1) %in% cls)]
#tmp = tmp1

  if(y == years[1]){
    allkill <- tmp
  }else{
    allkill <- bind_rows(allkill,tmp)
  }
}

trem = which(allkill$CASTE %in% c("C","F","H"))
if(length(trem)>0){
  allkill = allkill[-trem,]
}### removing the unused castes; there are permits that have this caste designation across all years


tkp = which(allkill$POTNTL == "Y")
if(length(tkp)>0){
  allkill = allkill[tkp,]
}### removing the hunters sampled from last year's permit file who indicated they didn't buy a permit this year
### and are therefore not potential hunters


trem = which(allkill$PERMIT == 0)
if(length(trem)>0){
  allkill = allkill[-trem,]
}### removes a single permit from 1985 with no permit number


# tkp = which(allkill$PRHUNT %in% c("AB","BC","MB","NB","NF","NS","NT","ON","PE","PQ","SK","YT","")) #drops NU and non provincial values
# if(length(tkp)>0){
#   allkill = allkill[tkp,]
# }### 



allkill$uniperm = allkill$PERMIT + allkill$SELYEAR*1000000 + allkill$YEAR*10000000000
dupuni = allkill$uniperm[duplicated(allkill$uniperm)]
# dupdf = allkill[which(allkill$uniperm %in% dupuni),]
# dupdf = dupdf[order(dupdf$uniperm),]

wmigoo <- which(allkill$PRHUNTG == "")
allkill$PRHUNTG = as.character(allkill$PRHUNTG)
allkill[wmigoo,"PRHUNTG"] <- as.character(allkill[wmigoo,"PRHUNT"])
allkill[wmigoo,"ZOHUNTG"] <- allkill[wmigoo,"ZOHUNT"]



wsud = which(allkill$TODUK > 0)
allkill$SUTODU <- "N"
allkill[wsud,"SUTODU"] <- "Y"


wsud = which(allkill$TOGOK > 0)
allkill$SUTOGO <- "N"
allkill[wsud,"SUTOGO"] <- "Y"



tkeepP = which(allkill$PRSAMP %in% provs) #keeps only permits sampled in primary provinces. for now ignores territories

allkill = allkill[tkeepP,]



nrow(allkill) == length(unique(allkill$uniperm))
allkill$year = allkill$YEAR-(min(allkill$YEAR)-1)
allkill$caste = factor(allkill$CASTE,
                       ordered = T,
                       levels = c("A","B","D","E"))


# 
# save(list = c("allkill"),
#      file = "data/allkill.RData")

######## sampling population sizes
popsiz_s = merge(popsiz,provzone[,c("prov","provn")],by.x = "PRSAMP",by.y = "provn",all.x = T)
popsiz_s = unique(popsiz_s)



#### total number of permits in each year

popsiz_perm = merge(perms,provzone[,c("prov","provn")],by.x = "PRSALE",by.y = "provn",all.x = T)
popsiz_perm = unique(popsiz_perm)


### total number of permits by zone and year

z_pops <- popsiz_perm %>%
  select(-PRSALE) %>% 
  rename(PRSAMP = prov,ZOSAMP = ZOSALE) %>% 
  group_by(PRSAMP,ZOSAMP,YEAR) %>% 
  summarise(TOTSALE = sum(TOTSALE))

# popsiz_perm$yr = str_sub(popsiz_perm$YEAR,start = 3,end = 4)
# tmp = left_join(popsiz_perm,popsiz_s[,c("zone","caste","TOTPERM","yr","prov")])


### species lists


others = c("COOTK","WOODK","SNIPK","DOVEK","PIGEK","CRANK","RAILK","MURRK")

#prov_otherk <- read.csv(stringsAsFactors = F,"data/OTHERK_by_Prov.csv")



# regulations compile -----------------------------------------------------

regs_other <- list()
length(regs_other) <- length(others)
names(regs_other) <- others

for(spgp in others){ 
 tmp <- read.csv(file = paste0("data/reg_",spgp,".csv"))
names(tmp)[which(names(tmp) == "QC")] <- "PQ"
regs_other[[spgp]] <- tmp
}


non_res_combine = paste(rep(provs,each = 3),rep(c(1,2,3),times = length(provs)))
#this above just ensures all non-resident hunters are combined with resident hunters for these groups, separating out caste E is rarely feasible (even caste B is sketchy)
keep_E <- paste(rep(c("MB","NB","SK"),each = 3),rep(c(1,2,3),times = 3))
# province and zone loops -------------------------------------------------
non_res_combine <- non_res_combine[-which(non_res_combine %in% keep_E)]

for(pr in provs){
  
  
  #################### try keeping all caste effects constant through time - done (except caste-D day effect)
  #################### or just giving a simple distributional prior so that we can still estimate the caste effects
  #################### or tweaking the priors so that they are strongly informative - done
  
  
  # regulations selection -----------------------------------------------------
  # # selecting years ---------------------------------------------------------
  # years = regs_other[which(regs_other[,pr] > 0),"YEAR"]
  # years <- years[which(years >= FY)]
  # if(spgp == "MURRK"){
  #   years <- 2006:2013
  # }
  # 
  # if(spgp == "SNIPK"){
  #   years <- 1991:Y
  # }
  # 
  nyears = length(years)
  minyr <- min(years)
  # 
  for(spgp in others){ 
    tmp <- regs_other[[spgp]][,c("YEAR",pr)]
    tmp[which(tmp[,pr] > 0),pr] <- 1
    names(tmp) <- c("YEAR",spgp)
    if(spgp == others[[1]]){
      regs <- tmp
      }else{
        regs <- merge(regs,tmp,by = "YEAR")
      }
    
  }
  regs <- regs[which(regs$YEAR >= FY),]
  regs <- regs[,which(colSums(regs) > 0)]
  grps <- names(regs)[-1] #the -1 removes the column called
  ngroups <- length(grps) #to enter model as data
  if("SNIPK" %in% grps){
    regs[which(regs$YEAR < 1992), "SNIPK"] <- 0
  }### remove Snipe hunt per 1991
  if("MURRK" %in% grps){
    regs[which(regs$YEAR >2012 | regs$YEAR < 2001), "MURRK"] <- 0
  }### remove Snipe hunt per 1991
  reg_mat <- as.matrix(regs[,grps]) #to enter model as data ensuring that group-level annual estimates are never > 0 in years with no season.
  grps_f <- factor(grps,levels = grps,ordered = TRUE) #ensures consistent ordering of the harvested groups
  
  fyear = NA
  for(g in 1:ngroups){
    fyear[g] <- min(which(regs[,g+1] > 0))
  }

  # data set up -------------------------------------------------------
  
  allkill = allkill
  phunt = "PRHUNT"
  zhunt = "ZOHUNT"
  wkill = grps
  wact = "ACTIVEOT"
  wsucc = paste0("SU",gsub("K",replacement = "",x = grps)) 
  wday = "DAYOT"
  
  
  

   zns <- as.integer(unique(allkill[which(allkill[,phunt] == pr),zhunt]))
  zns <- zns[which(zns > 0)]
  for(z in zns){
    



  
  
sumkill = allkill[which(allkill[,phunt] == pr &
                             allkill[,zhunt] == z &
                             allkill$YEAR %in% years),]

# if(minyr != FY){
#   FY1 <- minyr
# sumkill$year = sumkill$YEAR-(minyr-1)
# }else{
   FY1 = FY
# }
    
    
    
 

   
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
                                popsiz_perm$prov == pr & popsiz_perm$ZOSALE == z),"TOTSALE"]
  for(cc in castes[1:2]){ # loops through the castes D and B
    yn = as.integer(substr(as.character(years[y]),3,4))
    tmpnum = popsiz_s[which(popsiz_s$SAMPLE == levels(sumkill$caste)[cc] & popsiz_s$YEAR == yn &
                                   popsiz_s$prov == pr & popsiz_s$ZOSAMP == z),"TOTPERM"]
    tmpdenom = sum(popsiz_s[which(popsiz_s$SAMPLE %in% levels(sumkill$caste)[1:2] & popsiz_s$YEAR == yn &
                            popsiz_s$prov == pr & popsiz_s$ZOSAMP == z),"TOTPERM"])
    
    
    cfact[cc,y] = tmpnum/tmpdenom

    pops[cc,y] <- as.integer(round(permpop*cfact[cc,y],0))

    
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

nsucc <- array(NA,dim = c(ngroups,length(castes),nyears))
for(i in 1:ngroups){
  ws = wsucc[i]
  wk = wkill[i]
sumkill[,ws] <- "N"

sumkill[which(sumkill[,wk] > 0),ws] <- "Y"
if(wk == "MURRK"){
  sumkill[which(sumkill$YEAR > 2012),ws] <- "N"
}
sumkill[,ws] <- factor(sumkill[,ws],levels = c("N","Y"),ordered = TRUE)
#
  for(y in 1:nyears){
    # if(pr %in% c("YT","NT")){
    #   nsucc <- (table(sumkill[,wsucc],sumkill$caste,sumkill$year))
    #   nsucc <- nsucc["Y",,]
    # } 
    
        succt <- sumkill[which(sumkill[,"year"] == y),]
nsucct <- as.matrix(table(succt$caste,succt[,ws]))

  nsucc[i,castes,y] <- nsucct[,"Y"]

}

}


npotential <- as.matrix(table(sumkill$caste,sumkill$year))


sumkill_active = sumkill[which(sumkill[,wact] == "Y"),]
nactive <- (table(sumkill_active$caste,sumkill_active$year))
if(pr %in% c("YT","NT")){
  #nactive <- array(NA,dim = c(length(castes),nyears))
  nactive <- (table(sumkill[,wact],sumkill$caste,sumkill$year))
  nactive <- nactive["Y",,]
   }
  
for(i in 1:ngroups){
  print(grps[i])
print(nsucc[i,,]/nactive)
  }


if(any(sumkill_active[,wday] < 1 & spgp == "murre")){
  sumkill_active[which(sumkill_active[,wday] < 1),wday] <- sumkill_active[which(sumkill_active[,wday] < 1),"DAYOT"]
}

for(g in 1:ngroups){
if(any(nsucc[g,,] > nactive)){break("number successful > number active, problem with the data")}
}


caste = as.integer(sumkill_active[,"caste"])
year = sumkill_active[,"year"]
kill = as.matrix(sumkill_active[,wkill])
nhs = nrow(sumkill_active)
days = sumkill_active[,wday]

if(any(days < 1) | any(is.na(days))){
  mday_per_kill <- sum(days[which(days > 0)])/sum(rowSums(kill[which(days > 0),]),na.rm = T)
  days[which(days == 0 | is.na(days))] <- ceiling(mday_per_kill*(rowSums(kill[which(days == 0 | is.na(days)),],na.rm = T)+1))
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





# compiling JAGS data object ----------------------------------------------


jdat = list(pops = pops, # pops[c.y] total populations of permits by caste and year used to rescale all perhunter estimates to totals 
            #component that estimates p_active etc. to generate totals of active and successful hunters by year
            nactive = nactive, # nactive[c,y] number of active hunters by caste and year
            npotential = npotential, # npotential[c,y] number of potential hunters (respondents who bought a permit this year) by caste and year
            nsucc = nsucc, # nsucc[c,y] number of successful hunters by caste and year (active hunters with harvest > 0)
            #spcies composition components
            nyears = nyears, #integer length = 1 number of years
            ngroups = ngroups, #integer number of groups included in the other category for this zone
            fyear = fyear, #vector of the first years for each group
            reg_mat = reg_mat, # matrix of the yearly seasons for each group nrow = nyears, ncol = ngroups
            nhunter_cy = nhunter_cy, # nhunter_cy[castes,nyears] number active hunters by caste and year
            ncastes = max(castes), # castes (numeric, 1:4)
            castes = castes, # castes (numeric, 1:4)
            nhs = nhs, # integer length = 1 number of active hunters over all years (nrow for sumkill_active)
            #main data for overall harvest estimates
            hunter = hunter_n_cy, # vector(length = nhs) unique numeric indicator for active hunters by caste and year - same as nactive, but only needed for hunter-level predictions
            kill = kill, # matrix(nrow = nhs,ncol = ngroups), total group (ducks, geese, murres) harvest of nhs response
            year = year, # vector(length = nhs), year of response
            caste = caste, # vector(length = nhs), caste of response
            days = days, #vector(length = nhs), number of days spent hunting
            n_arrive = n_arrive,# 
            leave_hunt_cf = leave_hunt_cf
            )#





save(list = c("jdat","grps"),
     file = paste("data/data",pr,z,"other_save.RData",sep = "_"))


    }#z
  
}#pr




# MCMC loops --------------------------------------------------------------

n_cores <- length(provs)
cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


fullrun <- foreach(pr = provs,
                   .packages = c("jagsUI","tidyverse"),
                   .inorder = FALSE,
                   .errorhandling = "pass") %dopar%
  {

#for(pr in provs2){
# Set up parallel stuff


for(z in 1:3){
# fullrun <- foreach(z = zns,
#                    .packages = c("jagsUI","tidyverse"),
#                    .inorder = FALSE,
#                    .errorhandling = "pass") %dopar%
#   {
    
    
    mod.file = "models/group_model.R" # 
    
    if(file.exists(paste("data/data",pr,z,"other_save.RData",sep = "_"))){
    load(paste("data/data",pr,z,"other_save.RData",sep = "_"))

parms = c("NACTIVE_y",
          "NSUCC_yg",
          "nu_day",
          "sdhunter_day",
          # "mean_totkill_ycg",
          # "mean_totdays_ycg",
          # "mean_totkill_ycg_alt",
          # "mean_totdays_ycg_alt",
          #"kill_cyg",
          "kill_yg",
          "days_yg",
          "days_y",
          "nu",
          "sdhunter",
          "cst",
          "cst_day",
          "group",
          "ann_day",
          "parrive",
          "pleave",
          "psi")


#adaptSteps = 200              # Number of steps to "tune" the samplers.
burnInSteps = 5000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=1000          # Total number of steps in each chain to save.
thinSteps=10                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.

t1 = Sys.time()


  
  out2 = try(jagsUI(data = jdat,
                    parameters.to.save = parms,
                    n.chains = 3,
                    n.burnin = burnInSteps,
                    n.thin = thinSteps,
                    n.iter = nIter,
                    parallel = T,
                    #modules = "glm",
                    model.file = mod.file),silent = F)

  
  t2 = Sys.time()
if(class(out2) != "try-error"){
# 
#   pgg_psi = ggs(out2$samples,family = "psi")
#   #pgg_sdhunter = ggs(out2$samples,family = "sdhunter")
#   pgg_ann = ggs(out2$samples,family = "group")
#   pgg_nu = ggs(out2$samples,family = "nu")
#   #pgg_ky = ggs(out2$samples,family = "kill_y")
#   #pgg_ky = filter(pgg_ky,!grepl(pattern = "alt",Parameter))
# 
#   pgg <- rbind(pgg_psi,pgg_nu)
#   try(ggmcmc(pgg,file = paste0("output/conv/",pr,z,".pdf"),param_page = 5),silent = T)
#   #
# ggmcmc(pgg_ann,file = paste0("output/conv/ann_",pr,z,".pdf"),param_page = 5)
#   
  
  save(list = c("out2","jdat","grps"),
       file = paste("output/other harvest zip",pr,z,"alt mod.RData"))
  
rm(list = "out2")
}
}

  }#z

}#pr
stopCluster(cl = cluster)




# # plotting comparisons to published estimates -----------------------------

source("functions/comparison_plotting_function_other.R")

source("functions/utility_functions.R")




# regulations compile -----------------------------------------------------

regs_other <- list()
length(regs_other) <- length(others)
names(regs_other) <- others

for(spgp in others){ 
  tmp <- read.csv(file = paste0("data/reg_",spgp,".csv"))
  names(tmp)[which(names(tmp) == "QC")] <- "PQ"
  regs_other[[spgp]] <- tmp
}


non_res_combine = paste(rep(provs,each = 3),rep(c(1,2,3),times = length(provs)))
#this above just ensures all non-resident hunters are combined with resident hunters for these groups, separating out caste E is rarely feasible (even caste B is sketchy)
keep_E <- paste(rep(c("MB","NB","SK"),each = 3),rep(c(1,2,3),times = 3))
# province and zone loops -------------------------------------------------
non_res_combine <- non_res_combine[-which(non_res_combine %in% keep_E)]






jjsimcomp = 1
simcomp_list <-  list() 



for(pr in provs){
  
  

  # 
  nyears = length(years)
  minyr <- min(years)
  # 
  for(spgp in others){ 
    tmp <- regs_other[[spgp]][,c("YEAR",pr)]
    tmp[which(tmp[,pr] > 0),pr] <- 1
    names(tmp) <- c("YEAR",spgp)
    if(spgp == others[[1]]){
      regs <- tmp
    }else{
      regs <- merge(regs,tmp,by = "YEAR")
    }
    
  }
  regs <- regs[which(regs$YEAR >= FY),]
  regs <- regs[,which(colSums(regs) > 0)]
  grps <- names(regs)[-1] #the -1 removes the column called
  ngroups <- length(grps) #to enter model as data
  if("SNIPK" %in% grps){
    regs[which(regs$YEAR < 1992), "SNIPK"] <- 0
  }### remove Snipe hunt per 1991
  reg_mat <- as.matrix(regs[,grps]) #to enter model as data ensuring that group-level annual estimates are never > 0 in years with no season.
  grps_f <- factor(grps,levels = grps,ordered = TRUE) #ensures consistent ordering of the harvested groups
  
  fyear = NA
  for(g in 1:ngroups){
    fyear[g] <- min(which(regs[,g+1] > 0))
  }
  
  # data set up -------------------------------------------------------
  
  allkill = allkill
  phunt = "PRHUNT"
  zhunt = "ZOHUNT"
  wkill = grps
  wact = "ACTIVEOT"
  wsucc = paste0("SU",gsub("K",replacement = "",x = grps)) 
  wday = "DAYOT"
  
  
  
  mod.file = "models/group_model_zip23.R" # 
  
  
  
  
  zns <- as.integer(unique(allkill[which(allkill[,phunt] == pr),zhunt]))
  zns <- zns[which(zns > 0)]
  for(z in zns){
    
    
  
  
  
 
             mod.saved = paste("output/other harvest zip",pr,z,"alt mod.RData")

      
      #mod.saved = paste("output/full harvest time sdhunter",pr,z,spgp,"alt mod.RData") #paste("output/full harvest",pr,z,spgp,"alt mod.RData")
        if(file.exists(mod.saved)){
          load(mod.saved) #load(paste("output/full harvest",pr,z,spgp,"alt mod.RData"))#        load(paste("output/full harvest caste time",pr,z,spgp,"mod.RData"))

         

          
          var_pair = list(new = list("NACTIVE_y",
                              "NSUCC_yg",
                              "kill_yg",
                              "days_y",
                              "days_yg"),
                      old = list("ACTIOT",
                               wsucc,
                               wkill,
                               "DAYOT",
                               NA),
                      newgrps = list(NA,
                                     wsucc,
                                     wkill,
                                     NA,
                                     gsub(grps,pattern = "K",replacement = "Days"))) 

         

          
          
          
          

  
  simcomp_list[[jjsimcomp]] <- comp_plot_simple_other(prov = pr,
                                                      zone = z,
                                                      M = out2)
 
  
  jjsimcomp <- jjsimcomp + 1  











}#end if jags output exists 



 }#z

}#pr

  
########## add a time-series plot of hte sd hunter values across castes (maybe possible to have a single sdhunter for all but caste 4)
# plotting hunter effects -------------------------------------------------

# 
#  
#   
#   jjhunter = 1
#   hunter_list <- list()
#   
#   
#   for(pr in provs2){
#     zns <- unique(period[which(period$pr == pr),"zo"])
#     for(z in zns){
#       #       if(file.exists(paste("output/full harvest",pr,z,spgp,"mod.RData"))){
#       # load(paste("output/full harvest",pr,z,spgp,"mod.RData"))
#         if(file.exists(paste("output/hunter_effects",pr,z,spgp,"alt mod.RData"))){
#           
#           load(paste("output/hunter_effects",pr,z,spgp,"alt mod.RData"))#        load(paste("output/full harvest caste time",pr,z,spgp,"mod.RData"))
#           
#           
#           
#           
#           
#           
#           hunter_list[[jjhunter]] <- comp_plot_hunter(prov = pr,zone = z)
#           
#           jjhunter = 1+jjhunter      
#         }
#         
#       rm(out3)   
#     
#     }
#   }
# 
#   
#   # plotting pdfs -----------------------------------------------------------
  
  
  
  asuf <- c("ZIP")
  

  
  pdf(paste("output/comparison graphs simple other",asuf," ",".pdf"),
      width = 10,
      height = 7.5)
  
  for(pp in 1:length(simcomp_list)){
    plt = simcomp_list[[pp]]
    for(j in 1:length(plt)){
      print(plt[[j]])
    }}
  dev.off()
  
  
  
  
  

