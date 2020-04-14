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
library(tidyverse)
#load.module("glm") 

 

# load output from data_prep.R --------------------------------------------


load(paste0("data/parts and harvest survey info",Y,".RData"))

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

################### this is where the reporting bias might be an issue - renewal-hunter adjustment factors


trem = which(allkill$PERMIT == 0)
if(length(trem)>0){
  allkill = allkill[-trem,]
}### removes a single permit from 1985 with no permit number


tkp = which(allkill$PRHUNT %in% c("AB","BC","MB","NB","NF","NS","NT","ON","PE","PQ","SK","YT","")) #drops NU and non provincial values
if(length(tkp)>0){
  allkill = allkill[tkp,]
}### removes a single permit from 1985 with no permit number



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
  
pzcount = 0
for(pr in provs2[c(3,5,6)]){
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
  days[which(days == 0 | is.na(days))] <- ceiling(mday_per_kill*(kill[which(days == 0)]+1))
}
if(any(days < 1)){break("number of days includes zeros for successful hunters")}

#nhunter_cy[c,y] #number of active hunters by caste and year
nhunter_cy = matrix(0,nrow = max(castes),ncol = nyears)

for(y in 1:nyears){
  
  for(c in castes){
    ww = which(sumkill_active$year == y & sumkill_active$caste == levels(sumkill_active$caste)[c])
    if(length(ww) == 0){print(paste("no hunter responses in caste",c,"year",y,pr,z))}
    sumkill_active[ww,"hunter_n_cy"] <- as.integer(factor(sumkill_active[ww,"PERMIT"]))
    if(length(ww) == 0){
      nhunter_cy[c,y] <- 0
      
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








parms = c("kill_ys",
          "axcomp_axsy",
          "sdhunter",
          "sdhunter_day")

adaptSteps = 200              # Number of steps to "tune" the samplers.
burnInSteps = 5000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=2000          # Total number of steps in chains to save.
thinSteps=10                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.

t1 = Sys.time()
#if(spgp == "duck"){
  mod.file = "species_harvest_model.R" # I think this should work for geese and murres too
#}
   

out2 = try(jags.model( file = mod.file, 
                   data= jdat ,  
                   #inits= newinits,  
                   n.chains= nChains , 
                   n.adapt= adaptSteps ),silent = F)
if(class(out2) != "try-error"){

# Burn-in:
cat( "Burning in the MCMC chain...\n" )
update( out2 , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( out2 , variable.names=parms ,
                            n.iter=nIter , thin=thinSteps )
#codaSamples = coda.samples( jagsMod , variable.names=parms ,
#                            n.iter=50 , thin=1 )

t2 = Sys.time()

sum <- summary(codaSamples)



library(ggmcmc)

g = ggs(codaSamples)
for(pp in parms){
  gg = filter(g,grepl(Parameter,pattern = pp))
  ggmcmc(gg,file = paste("output/converge",pr,z,spgp,pp,"mcmc.pdf"))
}


newinits = coef(out2) # this object can be used as initial values to re-start the jags


save(list = c("sum","out2","jdat","codaSamples","newinits","sp.save.out"),
     file = paste("output/full harvest",pr,z,spgp,"mod.RData"))

rm(list = "out2")
}

  }#z

}#pr

}#spgp (species group)
#######################################
## set up some automatic plotting of important variables and compare with this year's 10-year trend graphs

###### still to Do...

#### add the extrapolation factors to the model calculations
#### compare (graphs) to the published harvest estimates
#### all before the tech meetings

pzcount = 0
for(pr in unique(period.duck$pr)){
  zns <- unique(period.duck[which(period.duck$pr == pr),"zo"])
  for(z in zns){
    pzcount = pzcount + 1
    if(file.exists(paste(pr,z,"full harvest model draft.RData"))){
    load(paste(pr,z,"full harvest model draft.RData"))
      }else{
      next
      }
    sumq = sum$quantiles
    
    
    
 nperiods = jdat$nperiods
 
 nspecies = jdat$nspecies
 w_psy = jdat$w_psy
    
pdf(file = paste(pr,z," mean species proportions across periods.pdf"))
for(s in 1:nspecies){
  spn <- sp.save[which(sp.save$spn == s),"species"]
  dd1 = ceiling(sqrt(nyears))

    if(round(sqrt(nyears)) != dd1){dd2 = dd1-1}else{dd2 = dd1}
  if(dd1*dd2 < nyears){dd2 = dd1}
  
    par(mfrow = c(dd1,dd2),
      mar = c(1,4,3,1))
  
  for(y in 1:jdat$nyears){
    
    plot(y = 3,x = 0,
         xlim = c(1,jdat$nperiods),
         ylim = c(0,max(sumq[paste0("pr[",rep(1:nperiods,each = nyears),",",s,",",rep(1:nyears,times = nperiods),"]"),"97.5%"])),
         main = paste(pr,z,spn,y+(Y-nyears)),
         ylab = "p of kill in period",
         xlab = "")
    
    muq = sumq[paste0("mu[",1:nperiods,",",s,"]"),]
    
    lines(y = muq[,"50%"],
          x = 1:nperiods,
          lwd = 2,
          col = "darkorange")
    for(per in 1:nperiods){
      n.obs <- w_psy[per,s,y]
      p.obs <- n.obs/sum(w_psy[per,1:nspecies,y])
      muq = sumq[paste0("mu[",per,",",s,"]"),]
      
      arrows(x0 = per,
             x1 = per,
             y0 = muq["2.5%"],
             y1 = muq["97.5%"],
             lwd = 2,
             col = "darkorange",
             length = 0)
      
      points(y = sumq[paste0("pr[",per,",",s,",",y,"]"),"50%"],
             x = per,
             pch = 19)
      arrows(x0 = per,
             x1 = per,
             y0 = sumq[paste0("pr[",per,",",s,",",y,"]"),"2.5%"],
             y1 = sumq[paste0("pr[",per,",",s,",",y,"]"),"97.5%"],
             lwd = 1,
             col = grey(0.5),
             length = 0)
      points(y = p.obs,
             x = per,
             pch = 1,
             col = "blue",
             cex = 1.2)
      
    }
  }
}#s
dev.off()

periodkill = jdat$periodkill
nhunter_y = jdat$nhunter_y

pdf(file = paste(pr,z," proportion of total hunt in periods.pdf"))
dd1 = ceiling(sqrt(nyears))
if(round(sqrt(nyears)) != dd1){dd2 = dd1-1}else{dd2 = dd1}
if(dd1*dd2 < nyears){dd2 = dd1}
  par(mfrow = c(dd1,dd2),
      mar = c(1,3,3,1))
  
  for(y in 1:jdat$nyears){
    
    plot(y = 3,x = 0,
         xlim = c(1,jdat$nperiods),
         ylim = c(0,max(sumq[paste0("ptotkill[",rep(1:nperiods,each = nyears),",",rep(1:nyears,times = nperiods),"]"),"97.5%"])),
         main = paste(pr,z,y+(Y-nyears)),
         ylab = "p of totkill in period",
         xlab = "")
    
    muq = sumq[paste0("mut[",1:nperiods,"]"),]
    
    lines(y = muq[,"50%"],
          x = 1:nperiods,
          lwd = 2,
          col = "darkorange")
    for(per in 1:nperiods){
      n.obs <- sum(periodkill[per,y,1:nhunter_y[y]],na.rm = T)
      p.obs <- n.obs/sum(periodkill[,y,1:nhunter_y[y]])
      muq = sumq[paste0("mut[",per,"]"),]
      
      arrows(x0 = per,
             x1 = per,
             y0 = muq["2.5%"],
             y1 = muq["97.5%"],
             lwd = 2,
             col = "darkorange",
             length = 0)
      
      points(y = sumq[paste0("ptotkill[",per,",",y,"]"),"50%"],
             x = per,
             pch = 19)
      arrows(x0 = per,
             x1 = per,
             y0 = sumq[paste0("ptotkill[",per,",",y,"]"),"2.5%"],
             y1 = sumq[paste0("ptotkill[",per,",",y,"]"),"97.5%"],
             lwd = 1,
             col = grey(0.5),
             length = 0)
      points(y = p.obs,
             x = per,
             pch = 1,
             col = "blue",
             cex = 1.2)
      
    }
  }

dev.off()


}#z
  
}#pr


















# 
# 
# ######### to add to summary and diagnostic plots:
# 
# yn <- 2016
# years <- (yn-30):yn
# 
# names(years) <- paste(years)
# home.fold <- "m:/My Documents/Harvest Survey A146/"
# 
# home.fold <- "C:/nhstemp/update 2016/"
# setwd(home.fold)
# year.fold <- paste(home.fold,yn,"/",sep = "")
# #year.fold2 <- paste(home.fold,y,sep = "")
# sum.fold <- paste0(home.fold,"update 2016/")
# sashome <- "C:\\Program Files\\SASHome\\SASFoundation\\9.4"
# 
# library(foreign)
# library(runjags)
# #library(emdbook)
# #library(bbmle)
# 
# #specieslevel <- F     ### change to true if species level summaries are desired, otherwise AOU-level summaries
# goose <- TRUE         ### change to false, if no goose species included (simplifies the PEF file extraction)
# murre <- T      #change to fales if no murre species should be included
# zone <- T           ### change to false, if provincial summaries are desired
# 
# load("completed June 1.RData")
# 
# # for(pr in unique(period.duck$pr)){
# #   zns <- unique(period.duck[which(period.duck$pr == pr),"zo"])
# #   for(z in zns){
# #     
#      pr = "AB"
#      z = 1
#     
#     
#     tmp <- period.duck[which(period.duck$pr == pr & period.duck$zo == z),]
#     
#     nperiods <- max(tmp$period)
#     
#     
#     prts1 <- outscse[which(outscse$PRHUNT == pr &
#                            outscse$ZOHUNT == z &
#                            outscse$AOU %in% aou.ducks),]
#     
#     luni <- function(x){
#       out <- length(unique(x))
#     }
#     yrspersp <- tapply(prts1$year,prts1$AOU,luni)
#     #remov all sp with < 10 years occurrence
#     prts1 <- prts1[which(prts1$AOU %in% names(yrspersp)[which(yrspersp > 9)]),]
#     
#     for(per in tmp$period){
#       sy <- tmp[which(tmp$period == per),"startweek"]
#       ey <- tmp[which(tmp$period == per),"endweek"]
#       prts1[which(prts1$WEEK %in% c(sy:ey)),"period"] <- per
#     }
#     
#     prdspersp <- tapply(prts1$period,prts1$AOU,luni)
#     prts1 <- prts1[which(prts1$AOU %in% names(prdspersp)[which(prdspersp > 4)]),]
#     
#     nspecies <- length(unique(prts1$AOU))
#     minyr <- min(prts1$year,na.rm = T)
#     nyears <- length(min(prts1$year,na.rm = T):max(prts1$year,na.rm = T))
#     partsarray <- array(data = 0,dim = c(nperiods,nspecies,nyears))
#     
#     for(per in 1:nperiods){
#       spc = 0
#       for(sp in unique(prts1$AOU)){
#         spc <- spc+1
#         for(y in 1:nyears){
#           yr <- y+(minyr-1)
#           partsarray[per,spc,y] <- nrow(prts1[which(prts1$period == per & prts1$AOU == sp & prts1$year == yr),])
#           
#           
#         }#y
#       }#sp
#       
#     }#per
#     
#     n <- matrix(NA,nrow = nperiods,ncol = nyears)  
#     for(per in 1:nperiods){
#       for(y in 1:nyears){
#         
#         n[per,y] <- sum(partsarray[per,,y])
#       }
#     }
#     
#     #### on Monday finish compiling data and try to run the model.
#     
# #     
# #     jin <- function(chain) return(switch(chain,
# #                                          "1"=list(delta = array(runif(nspecies*nyears*nperiods,0.2,0.8),dim = c(nperiods,nspecies,nyears))),
# #                                          "2"=list(delta = array(runif(nspecies*nyears*nperiods,0.2,0.8),dim = c(nperiods,nspecies,nyears))),
# #                                          "3"=list(delta = array(runif(nspecies*nyears*nperiods,0.2,0.8),dim = c(nperiods,nspecies,nyears)))))
# #     
#     
#     jdat = list(w = partsarray,
#                 nspecies = nspecies,
#                 nyears = nyears,
#                 nperiods = nperiods,
#                 midp = floor(nperiods/2),
#                 n = n)
#     
#     
#     load(paste(pr,z,"logistic species mean across period post jags.RData"))
#     
#     
#     sumq <- sum[["quantiles"]]
#     
#     
#     
#     
#     
#     pdf(file = paste(pr,z," mean across periods.pdf"))
#     for(s in 1:nspecies){
#       spn <- unique(prts1$AOU)[s]
#       par(mfrow = c(4,4),
#           mar = c(1,3,3,1))
# 
#       for(y in (nyears-15):nyears){
#       
#       plot(y = 3,x = 0,
#            xlim = c(1,nperiods),
#            ylim = c(0,max(sumq[paste0("pr[",rep(1:nperiods,each = 16),",",s,",",rep((nyears-15):nyears,times = nperiods),"]"),"97.5%"])),
#            main = paste(spn,y),
#            ylab = "proportion",
#            xlab = "")
# 
#         for(per in 1:nperiods){
#         n.obs <- partsarray[per,s,y]
#         p.obs <- n.obs/sum(partsarray[per,1:nspecies,y])
#         points(y = p.obs,
#                x = per,
#                pch = 1)
#         points(y = sumq[paste0("pr[",per,",",s,",",y,"]"),"50%"],
#                x = per,
#                pch = 19)
#         arrows(x0 = per,
#                x1 = per,
#                y0 = sumq[paste0("pr[",per,",",s,",",y,"]"),"2.5%"],
#                y1 = sumq[paste0("pr[",per,",",s,",",y,"]"),"97.5%"],
#                lwd = 1,
#                col = grey(0.5),
#                length = 0)
#         
#         }
#       }
#     }#s
#     dev.off()
#     
#     x[i,j, drop=missing(i)]
#     sub.mcmc <- codaSamples[,"alphab[4]",]
#     plot(sub.mcmc)
#     
#     #}
#     