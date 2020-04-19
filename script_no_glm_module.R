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


Y <- 2018
years <- 1975:Y

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
   "SUCCOT",
   "SUCCM",
   "ACTIVEOT",
   "ACTIVE",
   "ACTIVEWF",
   "ACTIVEM",
   "POTNTL")
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

trem = which(allkill$CASTE %in% c("C","F"))
if(length(trem)>0){
  allkill = allkill[-trem,]
}### removing the unused castes; there are two permits that have this caste designation across all years


tkp = which(allkill$POTNTL == "Y")
if(length(tkp)>0){
  allkill = allkill[tkp,]
}### removing the hunters sampled from last year's permit file who indicated they didn't buy a permit this year
### and are therefore not potential hunters


trem = which(allkill$PERMIT == 0)
if(length(trem)>0){
  allkill = allkill[-trem,]
}### removes a single permit from 1985 with no permit number


tkp = which(allkill$PRHUNT %in% c("AB","BC","MB","NB","NF","NS","NT","ON","PE","PQ","SK","YT","")) #drops NU and non provincial values
if(length(tkp)>0){
  allkill = allkill[tkp,]
}### 



allkill$uniperm = allkill$PERMIT + allkill$SELYEAR*1000000 + allkill$YEAR*10000000000
dupuni = allkill$uniperm[duplicated(allkill$uniperm)]
# dupdf = allkill[which(allkill$uniperm %in% dupuni),]
# dupdf = dupdf[order(dupdf$uniperm),]




nrow(allkill) == length(unique(allkill$uniperm))
 allkill$year = allkill$YEAR-(min(allkill$YEAR)-1)
 allkill$caste = factor(allkill$CASTE,
                        ordered = T,
                        levels = c("A","B","D","E"))



 
 ######## sampling population sizes
popsiz_s = merge(popsiz,provzone[,c("prov","provn")],by.x = "PRSAMP",by.y = "provn",all.x = T)
popsiz_s = unique(popsiz_s)


#### total number of permits in each year

popsiz_perm = merge(perms,provzone[,c("prov","provn")],by.x = "PRSALE",by.y = "provn",all.x = T)
popsiz_perm = unique(popsiz_perm)


### species lists

aou.ducks <- sps[which(sps$group == "duck"),"AOU"]
aou.goose <- sps[which(sps$group == "goose"),"AOU"]
aou.murre <- sps[which(sps$group == "murre"),"AOU"]







# correcting the age and sex indicators -----------------------------------



outscse[which(outscse$BAGE %in% c("1","S","T")),"BAGE"] <- "A"
outscse[which(outscse$BAGE %in% c("2")),"BAGE"] <- "I"
outscse[which(outscse$BAGE %in% c("3")),"BAGE"] <- "U"
outscse[which(outscse$BAGE %in% c("")),"BAGE"] <- "U"

outscse[which(outscse$BSEX %in% c("1")),"BSEX"] <- "M"
outscse[which(outscse$BSEX %in% c("2")),"BSEX"] <- "F"
outscse[which(outscse$BSEX %in% c("3")),"BSEX"] <- "U"
outscse[which(outscse$BSEX %in% c("")),"BSEX"] <- "U"

#outscse$BAGE = factor(outscse$BAGE)
#round(prop.table(table(outscse$BAGE,outscse$AOU),2),2)

#outscse$BSEX = factor(outscse$BSEX)
#round(prop.table(table(outscse$BSEX,outscse$AOU),2),2)



### for a given species and year, need estimates of the sex and age structure
## assume independence of age and sex?
## use a strongly informative prior on the variance, given the small number of parts
## or estimate all 4 (or 9 incl unknowns) categories independently, with an 
## use the same time-series structure used for hte other multinomial sub-models




########## consider if the renewing hunters adjustment is necessary.


for(spgp in c("goose","duck","murre")){
### begining of loop through provinces only engage this loop if running the full analysis
### for a single province and zone, skip the next 4 lines
### and enter something like the following (e.g., to run Ontario-zone 3)

# group data set up -------------------------------------------------------

  
  if(spgp == "goose"){
    aou.spgp = aou.goose
    period = period.goose
    cal.spgp = calg
    allkill = allkill
    phunt = "PRHUNTG"
    zhunt = "ZOHUNTG"
    wkill = "TOGOK"
    wact = "ACTIVEWF"
    wsucc = "SUCCWF"
    wday = "DAYWF"
    years = 1975:Y
    nyears = length(years)
    demog = data.frame(BSEX = rep(c("U","U"),each = 1),
                       BAGE = rep(c("A","I"),times = 1),
                       stringsAsFactors = F)
    minyr <- min(years)
    provs2 <- provs
    
    
  }
  if(spgp == "duck"){
    aou.spgp = aou.ducks
    period = period.duck
    cal.spgp = cald
    allkill = allkill
    phunt = "PRHUNT"
    zhunt = "ZOHUNT"
    wkill = "TODUK"
    wact = "ACTIVEWF"
    wsucc = "SUCCWF"
    wday = "DAYWF"
    years = 1975:Y
    nyears = length(years)
    demog = data.frame(BSEX = rep(c("F","M"),each = 2),
                       BAGE = rep(c("A","I"),times = 2),
                       stringsAsFactors = F)
    minyr <- min(years)
    provs2 <- provs
  }
  
  
  
  if(spgp == "murre"){
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
    years = 2014:Y #### previous years Murre harvest was calculated differently, pre 2013 only total MURRK, and in 2013 it was a mix of infor from DAYOT and calendars and species composition
    nyears = length(years)
    demog = data.frame(BSEX = rep(c("U","U"),each = 1),
                       BAGE = rep(c("A","I"),times = 1),
                       stringsAsFactors = F)
    minyr <- 2014
    provs2 = "NF"
    
  }
  

  non_res_combine = c("NF 1","NF 2","PE 1","NS 1","NS 1","BC 2","NT 1","YT 1")
  
  
  

# Province and Zone loop --------------------------------------------------

  
#for(pr in provs2[c(3,5,6)]){
  
  # Set up parallel stuff
  n_cores <- length(provs2)
  cluster <- makeCluster(n_cores, type = "PSOCK")
  registerDoParallel(cluster)
  
  
  
  fullrun <- foreach(pr = provs2,
                      .packages = c("jagsUI","tidyverse"),
                      .inorder = FALSE,
                      .errorhandling = "pass") %dopar%
    {
      
  pzcount = 0
  
  zns <- unique(period[which(period$pr == pr),"zo"])
  for(z in zns){
pzcount = pzcount + 1





# periods -----------------------------------------------------------------


periods <- period[which(period$pr == pr & period$zo == z),]
sumkill = allkill[which(allkill[,phunt] == pr &
                             allkill[,zhunt] == z &
                             allkill$YEAR %in% years),]

if(minyr != 1975){
sumkill$year = sumkill$YEAR-(minyr-1)
}
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
    prts1 <- prts1[which(prts1$AOU %in% names(yrspersp)[which(yrspersp > (0.33*length(years)))]),]
    
    
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
   

    if(pzcount == 1){
      sp.save.out = sp.save
    }else{
      sp.save.out = rbind(sp.save.out,sp.save)
    }
    
   
   



    
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
  
}

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
      
    
  tmp1$week = as.integer(ceiling((tmp1$date-(min(tmp1$date)-1))/7))
  
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
      periodkill[per,yn,h] <- sum(tmp2[wt2,"COUNT"])
    
  }#per
  }#h
}#yn

### below can be commented out but prints the 
### observed proportional composition of the hunt by years (rows) and periods (columns)
### helps for checking that there are no missing data
# for(y in 1:nyears){
# print(round(rowSums(periodkill[,y,],na.rm = F)/sum(rowSums(periodkill[,y,],na.rm = F)),3))
# if(any(is.na(round(rowSums(periodkill[,y,],na.rm = F)/sum(rowSums(periodkill[,y,],na.rm = F)),3)))){print(y)}
#   }



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

# 
# for(y in years){
#   cfact[1,y] <- popsiz_s[which(popsiz_s$SAMPLE == "D" & popsiz_s$)]
#   
# }#y


# separating active and inactive ------------------------------------------






npotential <- as.matrix(table(sumkill$caste,sumkill$year))
succ = sumkill[which(sumkill[,wsucc] == "Y"),]
nsucc <- as.matrix(table(succ$caste,succ$year))


sumkill_active = sumkill[which(sumkill[,wact] == "Y"),]

#### insert 0 for all NA-kill values and active hunters (applies to active WF hunters with NA values for geese)

if(any(is.na(sumkill_active[,wkill]) & spgp == "goose")){
  sumkill_active[which(is.na(sumkill_active[,wkill])),wkill] <- 0
}


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

clsw = which(names(sumkill_active) %in% c(wkill,wday,"year","caste"))

if(any(days < 1)){break("number of days includes zeros for successful hunters")}

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

#### still need a predictive array to generate the PEFs 
#### still need a predictive array to generate the PEFs 
#### still need a predictive array to generate the PEFs 
#### still need a predictive array to generate the PEFs 
#### still need a predictive array to generate the PEFs 

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
            #main data for overall harvest estimates
            hunter = hunter_n_cy, # vector(length = nhs) unique numeric indicator for active hunters by caste and year 
            kill = kill, # vector(length = nhs), total group (ducks, geese, murres) harvest of nhs response
            year = year, # vector(length = nhs), year of response
            caste = caste, # vector(length = nhs), caste of response
            days = days)# vector(length = nhs), number of days spent hunting








parms = c("NACTIVE_y",
          "NSUCC_y",
          "nu_day",
          "sdhunter_day",
          "mean_totkill_yc",
          "mean_totdays_yc",
          "mean_totkill_yc_alt",
          "mean_totdays_yc_alt",
          "kill_cy",
          "kill_ys",
          "kill_y",
          "days_y")

adaptSteps = 200              # Number of steps to "tune" the samplers.
burnInSteps = 5000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=6000          # Total number of steps in chains to save.
thinSteps=10                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps) / nChains ) # Steps per chain.

t1 = Sys.time()
#if(spgp == "duck"){
  mod.file = "species_harvest_model.R" # I think this should work for geese and murres too
#}
   

# out2 = try(jags.model( file = mod.file, 
#                    data= jdat ,  
#                    #inits= newinits,  
#                    n.chains= nChains , 
#                    n.adapt= adaptSteps ),silent = F)
# if(class(out2) != "try-error"){
# 
# # Burn-in:
# cat( "Burning in the MCMC chain...\n" )
# update( out2 , n.iter=burnInSteps )
# # The saved MCMC chain:
# cat( "Sampling final MCMC chain...\n" )
# codaSamples = coda.samples( out2 , variable.names=parms ,
#                             n.iter=nIter , thin=thinSteps )
# #codaSamples = coda.samples( jagsMod , variable.names=parms ,
# #                            n.iter=50 , thin=1 )

  
  out2 = try(jagsUI(data = jdat,
                    parameters.to.save = parms,
                    n.chains = 3,
                    n.burnin = burnInSteps,
                    n.thin = thinSteps,
                    n.iter = nIter,
                    parallel = T,
                    modules = NULL,
                    model.file = mod.file),silent = F)
t2 = Sys.time()
if(class(out2) != "try-error"){


# g = ggs(out2$samples)
# for(pp in parms){
#   gg = filter(g,grepl(Parameter,pattern = pp))
#   ggmcmc(gg,file = paste("output/converge",pr,z,spgp,pp,"mcmc.pdf"))
# }




save(list = c("out2","jdat","sp.save.out"),
     file = paste("output/full harvest",pr,z,spgp,"mod.RData"))

rm(list = "out2")

}

  }#z
  
}#pr

stopCluster(cl = cluster)


}#spgp
# plotting comparisons to published estimates -----------------------------

source("comparison_plotting_function_species.R")
source("comparison_plotting_function.R")
source("utility_functions.R")
for(spgp in c("goose","duck","murre")){
  ### begining of loop through provinces only engage this loop if running the full analysis
  ### for a single province and zone, skip the next 4 lines
  ### and enter something like the following (e.g., to run Ontario-zone 3)
  
  # group data set up -------------------------------------------------------
  
  
  if(spgp == "goose"){
    aou.spgp = aou.goose
    period = period.goose
    cal.spgp = calg
    allkill = allkill
    phunt = "PRHUNTG"
    zhunt = "ZOHUNTG"
    wkill = "TOGOK"
    wact = "ACTIVEWF"
    wsucc = "SUCCWF"
    wday = "DAYWF"
    years = 1975:Y
    nyears = length(years)
    demog = data.frame(BSEX = rep(c("U","U"),each = 1),
                       BAGE = rep(c("A","I"),times = 1),
                       stringsAsFactors = F)
    minyr <- min(years)
    provs2 <- provs
    
    
  }
  if(spgp == "duck"){
    aou.spgp = aou.ducks
    period = period.duck
    cal.spgp = cald
    allkill = allkill
    phunt = "PRHUNT"
    zhunt = "ZOHUNT"
    wkill = "TODUK"
    wact = "ACTIVEWF"
    wsucc = "SUCCWF"
    wday = "DAYWF"
    years = 1975:Y
    nyears = length(years)
    demog = data.frame(BSEX = rep(c("F","M"),each = 2),
                       BAGE = rep(c("A","I"),times = 2),
                       stringsAsFactors = F)
    minyr <- min(years)
    provs2 <- provs
  }
  
  
  
  if(spgp == "murre"){
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
    years = 2014:Y #### previous years Murre harvest was calculated differently, pre 2013 only total MURRK, and in 2013 it was a mix of infor from DAYOT and calendars and species composition
    nyears = length(years)
    demog = data.frame(BSEX = rep(c("U","U"),each = 1),
                       BAGE = rep(c("A","I"),times = 1),
                       stringsAsFactors = F)
    minyr <- 2014
    provs2 = "NF"
    
  }
  
 

# plotting loop -----------------------------------------------------------

   
  jjcomp = 1
  compps <-  list() 
    
  for(pr in provs2){
    zns <- unique(period[which(period$pr == pr),"zo"])
    for(z in zns){
      if(file.exists(paste("output/full harvest",pr,z,spgp,"mod.RData"))){
load(paste("output/full harvest",pr,z,spgp,"mod.RData"))
#        load(paste("output/full harvest caste time",pr,z,spgp,"mod.RData"))
        
        var_pair = data.frame(new = c("NACTIVE_y",
                              "NSUCC_y",
                              "kill_y",
                              "days_y"),
                      duck = c("ACTIWF",
                               "SUTODU",
                               "TODUK",
                               "DAYWF"),
                      stringsAsFactors = F) ## add ofther spgp columns to match

plts = list()
length(plts) = nrow(var_pair)
pdf(paste("output/comparison graphs",pr,z,"simple.pdf"))

for(i in 1:nrow(var_pair)){

  
plts[[i]] <-  comp_plot_simple(group = spgp,
var = var_pair[i,"new"],
prov = pr,
zone = z,
M = out2)


print(plts[[i]])
}

dev.off()





# species comparisons -----------------------------------------------------


spplts <- comp_plot_species(prov = pr,
                            zone = z,
                            nspecies = jdat$nspecies)

pdf(paste0("output/species_level_harvests_",pr,z,".pdf"),width = 8,height = 10)
for(pp in 1:length(spplts)){print(spplts[[pp]])}
dev.off()


# comparing retransformation options --------------------------------------

# 
# 
# ### mean kill
# dsum = as.data.frame(out2$summary)
# names(dsum)[3:7] <- c("lci","lqrt","med","uqrt","uci")
# dsum$Parameter = row.names(dsum)
# d1 = filter(dsum,grepl(Parameter,pattern = "mean_totkill_yc"))
# d1$vers = "smear"
# d1$yr = jags_dim(var = "mean_totkill_yc",dat = d1)
# d1$caste = jags_dim(var = "mean_totkill_yc",dat = d1,dim = 2)
# 
# d2 = filter(dsum,grepl(Parameter,pattern = "mean_totkill_retrans_yc"))
# d2$vers = "retrans"
# d2$yr = jags_dim(var = "mean_totkill_retrans_yc",dat = d2)
# d2$caste = jags_dim(var = "mean_totkill_retrans_yc",dat = d2,dim = 2)
# 
# dd = bind_rows(d1,d2)
# 
# dd$year = years[dd$yr]
# dd$castes = levels(sumkill_active$caste)[dd$caste]
# 
# for(i in 1:nrow(dd)){
#   cc = dd[i,"caste"]
#   yy = dd[i,"yr"]
#   
#   dd[i,"nhunter"] <- nhunter_cy[cc,yy]
# }
# 
# 
# # if(max(to_plot$nrts) > 200){
# #   ncby_y = ceiling(to_plot$nrts/50)
# #   annot = c("each dot ~ 50 routes")
# # }else{
# #   ncby_y = to_plot$nrts
# #   annot = c("each dot = 1 route")
# #   
# # }
# 
# ulim = max(dd$uci)
# ddb = dd[which(dd$vers == "smear"),]
# ddb$hunterplot <- (ddb$nhunter/max(ddb$nhunter))*(ulim/2)
# ddbmx = tapply(ddb$nhunter,ddb$castes,max)
# wm = NULL
# ddbmn = tapply(ddb$nhunter,ddb$castes,min)
# wmn = NULL
# 
# for(j in 1:length(ddbmx)){
#   wm[j] <- which(ddb$nhunter == ddbmx[j] & ddb$castes == names(ddbmx)[j])
#   wmn[j] <- which(ddb$nhunter == ddbmn[j] & ddb$castes == names(ddbmn)[j])
# }
# ddbm = ddb[c(wm,wmn),]
# compp = ggplot(data = dd,aes(x = year,y = mean,fill = vers))+
#   geom_bar(data = ddb,inherit.aes = FALSE,aes(x = year,y = hunterplot),fill = grey(0.2),alpha = 0.1,stat = "identity")+
#   geom_point(aes(colour = vers))+
#   geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.3)+
#   labs(title = paste0("retrans comparison KILL",pr," zn",z," (mean and 95 CI)"))+
#   scale_y_continuous(limits = c(0,ulim))+
#   scale_color_viridis_d(aesthetics = c("colour","fill"), end = 0.7)+
#   theme_classic()+
#   geom_text_repel(data = ddbm,inherit.aes = FALSE,aes(x = year,y = hunterplot,label = nhunter),size = 3,colour = grey(0.2),alpha = 0.75,nudge_y = ulim*-0.1)+
#   facet_wrap(facets = ~castes,ncol = 2,scales = "fixed")
# 
# compps[[jjcomp]] <- compp
# jjcomp = jjcomp +1
# ### mean days
# dsum = as.data.frame(out2$summary)
# names(dsum)[3:7] <- c("lci","lqrt","med","uqrt","uci")
# dsum$Parameter = row.names(dsum)
# d1 = filter(dsum,grepl(Parameter,pattern = "mean_totdays_yc"))
# d1$vers = "smear"
# d1$yr = jags_dim(var = "mean_totdays_yc",dat = d1)
# d1$caste = jags_dim(var = "mean_totdays_yc",dat = d1,dim = 2)
# 
# d2 = filter(dsum,grepl(Parameter,pattern = "mean_totdays_retrans_yc"))
# d2$vers = "retrans"
# d2$yr = jags_dim(var = "mean_totdays_retrans_yc",dat = d2)
# d2$caste = jags_dim(var = "mean_totdays_retrans_yc",dat = d2,dim = 2)
# 
# dd = bind_rows(d1,d2)
# 
# dd$year = years[dd$yr]
# dd$castes = levels(sumkill_active$caste)[dd$caste]
# 
# for(i in 1:nrow(dd)){
#   cc = dd[i,"caste"]
#   yy = dd[i,"yr"]
#   
#   dd[i,"nhunter"] <- nhunter_cy[cc,yy]
# }
# 
# 
# # if(max(to_plot$nrts) > 200){
# #   ncby_y = ceiling(to_plot$nrts/50)
# #   annot = c("each dot ~ 50 routes")
# # }else{
# #   ncby_y = to_plot$nrts
# #   annot = c("each dot = 1 route")
# #   
# # }
# 
# ulim = max(dd$uci)
# ddb = dd[which(dd$vers == "smear"),]
# ddb$hunterplot <- (ddb$nhunter/max(ddb$nhunter))*(ulim/2)
# ddbmx = tapply(ddb$nhunter,ddb$castes,max)
# wm = NULL
# ddbmn = tapply(ddb$nhunter,ddb$castes,min)
# wmn = NULL
# 
# for(j in 1:length(ddbmx)){
#   wm[j] <- which(ddb$nhunter == ddbmx[j] & ddb$castes == names(ddbmx)[j])
#   wmn[j] <- which(ddb$nhunter == ddbmn[j] & ddb$castes == names(ddbmn)[j])
# }
# ddbm = ddb[c(wm,wmn),]
# compp = ggplot(data = dd,aes(x = year,y = mean,fill = vers))+
#   geom_bar(data = ddb,inherit.aes = FALSE,aes(x = year,y = hunterplot),fill = grey(0.2),alpha = 0.1,stat = "identity")+
#   geom_point(aes(colour = vers))+
#   geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.3)+
#   labs(title = paste0("retrans comparison DAYS",pr," zn",z," (mean and 95 CI)"))+
#   scale_y_continuous(limits = c(0,ulim))+
#   scale_color_viridis_d(aesthetics = c("colour","fill"), end = 0.7)+
#   theme_classic()+
#   geom_text_repel(data = ddbm,inherit.aes = FALSE,aes(x = year,y = hunterplot,label = nhunter),size = 3,colour = grey(0.2),alpha = 0.75,nudge_y = ulim*-0.1)+
#   facet_wrap(facets = ~castes,ncol = 2,scales = "fixed")
# 
# 
# compps[[jjcomp]] <- compp
# jjcomp = jjcomp +1





}#end if jags output exists 



 }#z

}#pr

  pdf(paste0("output/retransformation comparison.pdf"),
      width = 8,
      height = 6)
  for(jj in 1:length(compps)){
    print(compps[[jj]])
  }
  dev.off()
  
}#spgp (species group)
#######################################
## set up some automatic plotting of important variables and compare with this year's 10-year trend graphs

###### still to Do...
