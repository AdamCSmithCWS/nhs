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


Y <- 2018
years <- 1975:Y

names(years) <- paste(years)
 home.fold1 <- "m:/My Documents/Harvest Survey A146/"
# 
 home.fold <- getwd()
# setwd(home.fold)

library(foreign)
library(runjags)
library(rjags)
library(tidyverse)

 

# load output from data_prep.R --------------------------------------------


load(paste0("parts and harvest survey info",Y,".RData"))

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
   "DAYWF",
   "DAYOT",
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


# allkillwf = allkill[which(allkill$ACTIVEWF == "Y"),]
# 
# 
# allkillwf$year = allkillwf$YEAR-(min(allkillwf$YEAR)-1)
# allkillwf$caste = factor(allkillwf$CASTE,
#                        ordered = T,
#                        levels = c("A","B","D","E"))
# 
# 
# 
# allkillot = allkill[which(allkill$ACTIVEOT == "Y"),]
# 
# allkillot$year = allkillot$YEAR-(min(allkillot$YEAR)-1)
# allkillot$caste = factor(allkillot$CASTE,
#                          ordered = T,
#                          levels = c("A","B","D","E"))
# 
# 
# 
# 
# allkillmu = allkill[which(allkill$ACTIVEM == "Y"),]
# 
# allkillmu$year = allkillmu$YEAR-(min(allkillmu$YEAR)-1)
# allkillmu$caste = factor(allkillmu$CASTE,
#                          ordered = T,
#                          levels = c("A","B","D","E"))
# 
# 




popsiz = merge(popsiz,provzone[,c("prov","provn")],by.x = "PRSAMP",by.y = "provn",all.x = T)
popsiz = unique(popsiz)


### species lists

aou.ducks <- sps[which(sps$group == "duck"),"AOU"]
aou.goose <- sps[which(sps$group == "goose"),"AOU"]
aou.murre <- sps[which(sps$group == "murre"),"AOU"]







 #save.image(paste0("full data prep updated harvest model",Y,".RData"))



###################### 
## everything from this line down should work
## adjust the "years = 1975:1984" line to change the
## time-window for the analysis

# load(paste0("full data prep updated harvest model",Y,".RData"))


# correcting the age and sex indicators -----------------------------------



outscse[which(outscse$BAGE %in% c("1","S","T")),"BAGE"] <- "A"
outscse[which(outscse$BAGE %in% c("2")),"BAGE"] <- "I"
outscse[which(outscse$BAGE %in% c("3")),"BAGE"] <- "U"
outscse[which(outscse$BAGE %in% c("")),"BAGE"] <- "U"

outscse[which(outscse$BSEX %in% c("1")),"BSEX"] <- "M"
outscse[which(outscse$BSEX %in% c("2")),"BSEX"] <- "F"
outscse[which(outscse$BSEX %in% c("3")),"BSEX"] <- "U"
outscse[which(outscse$BSEX %in% c("")),"BSEX"] <- "U"

outscse$BAGE = factor(outscse$BAGE)
round(prop.table(table(outscse$BAGE,outscse$AOU),2),2)

outscse$BSEX = factor(outscse$BSEX)
round(prop.table(table(outscse$BSEX,outscse$AOU),2),2)

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
## pr = "ON"
## z = 3

  if(spgp == "goose"){
    aou.spgp = aou.goose
    period = period.goose
    cal.spgp = calg
    allkill = allkillwf
    phunt = "PRHUNTG"
    zhunt = "ZOHUNTG"
    wkill = "TOGOK"
    years = 1975:Y
    nyears = length(years)
    
  }
  
pzcount = 0
for(pr in provs){
  zns <- unique(period[which(period$pr == pr),"zo"])
  for(z in zns){
pzcount = pzcount + 1
# pr = "NF"
# z = 2


periods <- period[which(period$pr == pr & period$zo == z),]
sumkill = allkill[which(allkill[,phunt] == pr &
                             allkill[,zhunt] == z &
                             allkill$YEAR %in% years),]


  nperiods <- max(periods$period)
    
    
    # 
    # periods <- period.duck[which(period.duck$pr == pr & period.duck$zo == z),]
    # 
    # nperiods <- max(periods$period)
    # 
    prts1 <- outscse[which(outscse$PRHUNT == pr &
                          outscse$ZOHUNT == z &
                          outscse$AOU %in% aou.spgp &
                            outscse$YEAR %in% years),]
    
    luni <- function(x){
      out <- length(unique(x))
    }
    yrspersp <- tapply(prts1$YEAR,prts1$AOU,luni)
    #remov all sp with < 50% years occurrence
    prts1 <- prts1[which(prts1$AOU %in% names(yrspersp)[which(yrspersp > (0.5*length(years)))]),]
    
    for(per in periods$period){
      sy <- periods[which(periods$period == per),"startweek"]
      ey <- periods[which(periods$period == per),"endweek"]
    prts1[which(prts1$WEEK %in% c(sy:ey)),"period"] <- per
    }
      
    prdspersp <- tapply(prts1$period,prts1$AOU,luni)#number of periods a species has been observed
    #could use the above values and the line below to remove species
    # that only appear in very few periods
    #prts1 <- prts1[which(prts1$AOU %in% names(prdspersp)[which(prdspersp > 4)]),]
    
   nspecies <- length(unique(prts1$AOU))
   minyr <- min(prts1$YEAR,na.rm = T)
   #nyears <- length(min(prts1$YEAR,na.rm = T):max(prts1$YEAR,na.rm = T))
   partsarray <- array(data = 0,dim = c(nperiods,nspecies,nyears))

    for(per in 1:nperiods){
      spc = 0
      sp.save = data.frame(spn = 1:nspecies,
                           species = NA)
      for(sp in unique(prts1$AOU)){
        spc <- spc+1
        sp.save[which(sp.save$spn == spc),"species"] <- sp
        for(y in 1:nyears){
         yr <- y+(minyr-1)
          partsarray[per,spc,y] <- nrow(prts1[which(prts1$period == per & prts1$AOU == sp & prts1$YEAR == yr),])
          
           
        }#y
      }#sp
      sp.save[,"province"] <- pr
      sp.save[,"zone"] = z
      if(pzcount == 1){
        sp.save.out = sp.save
      }else{
        sp.save.out = rbind(sp.save.out,sp.save)
      }

    }#per
   

   
   
  # setting up initial values using observed data
jinmeans = partsarray 
for(y in 1:nyears){
  for(pp in 1:nperiods){
    for(sp in 1:nspecies){
  jinmeans[pp,sp,y] <- sum(partsarray[,sp,])/sum(partsarray)
    }
                         }
} 


# jin <- function(chain) return(switch(chain,
#                                       "1"=list(delta = array(runif(nspecies*nyears*nperiods,0.2,0.8),dim = c(nperiods,nspecies,nyears))),
#                                       "2"=list(delta = array(runif(nspecies*nyears*nperiods,0.2,0.8),dim = c(nperiods,nspecies,nyears))),
#                                       "3"=list(delta = array(runif(nspecies*nyears*nperiods,0.2,0.8),dim = c(nperiods,nspecies,nyears)))))
#  
jin <- function(chain) return(switch(chain,
                                     "1"=list(delta = jinmeans),
                                     "2"=list(delta = jinmeans),
                                     "3"=list(delta = jinmeans)))
## function to generate initial values across chains in jags




#########################
### compiling calendar info 
## generate an array 
## periodkill[p,y,h] = total kill in period-x and year-y for each hunter-h
## nhunters[y] = number of hunters with calendar information in year-y


nhunters = vector(length = nyears)
names(nhunters) = as.character(years)

for(y in years){

  tmp <- cal.spgp[[as.character(y)]]
  tmp1 <- tmp[which(tmp$PRHUNT == pr &
                      tmp$ZOHUNT == z),]
  nhunters[as.character(y)] <- length(unique(tmp1$PERMIT))
  
}

periodkill = array(0,
                   dim = c(nperiods,nyears,max(nhunters)))
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
  
  for(h in 1:nhunters[yn]){
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
for(y in 1:nyears){
print(round(rowSums(periodkill[,y,],na.rm = F)/sum(rowSums(periodkill[,y,],na.rm = F)),3))
if(any(is.na(round(rowSums(periodkill[,y,],na.rm = F)/sum(rowSums(periodkill[,y,],na.rm = F)),3)))){print(y)}
  }



# collecting and sorting the total kill by caste for the zone -------------




#combines castes B and D into resident renewal hunters

#combines castes B and D into resident renewal hunters

##sumkill[which(sumkill$CASTE == "B"),"CASTE"] <- "D" #combines castes B and D into resident renewal hunters
#combines castes B and D into resident renewal hunters
#combines castes B and D into resident renewal hunters

sumkill$caste = factor(as.character(sumkill$CASTE),ordered = T,levels = c("D","B","A","E")) #D-renewal > 1year, B-renewal = 1year, A-nonrenewal (new hunter), E-nonresident
sumkill = sumkill[order(sumkill$caste),]
caste = as.integer(sumkill[,"caste"])
year = sumkill[,"year"]
kill = sumkill[,wkill]
nhs = nrow(sumkill)
castes = 1:max(caste) #



# population sizes (number of permits in each caste and year) --------------------------------------------------------

#pops[c,y]
pops = matrix(0,nrow = max(castes),ncol = nyears)

for(cc in castes){
  for(y in 1:nyears){
    yn = as.integer(substr(as.character(years[y]),3,4))
    pops[cc,y] <- popsiz[which(popsiz$SAMPLE == levels(sumkill$caste)[cc] & popsiz$YEAR == yn &
                                 popsiz$prov == pr & popsiz$ZOSAMP == z),"TOTPERM"]
  }
}




# indicators for start and stop of sorted caste data ----------------------


shuntercastes <- c(1)#first row of nhs data for hunter caste
ehuntercastes = max(which(caste == 1))#last row for caste
for(c in castes[-1]){
  shuntercastes[c] = min(which(caste == c))
  ehuntercastes[c] = max(which(caste == c))
}



# total number of parts by year and period --------------------------------


nparts_py = matrix(nrow = nperiods,
           ncol = nyears)
for(p in 1:nperiods){
  for(y in 1:nyears){
    nparts_py[p,y] <- sum(partsarray[p,,y],na.rm = T)# yearl and period sums of all parts
  }
}


# total harvest by year and hunter ----------------------------------------


nkill = matrix(nrow = nyears,
               ncol = max(nhunters))
for(y in 1:nyears){
  for(h in 1:nhunters[y]){
nkill[y,h] <- sum(periodkill[,y,h],na.rm = T) #simple sum of the data
  }
}



# compiling JAGS data object ----------------------------------------------


jdat = list(w_psy = partsarray,
            nspecies = nspecies,
            nyears = nyears,
            nperiods = nperiods,
            nhunters = nhunters,
            castes = castes,
            nhs = nhs,
            kill = kill,
            year = year,
            caste = caste,
            kill_pyh = periodkill,
            nkill = nkill,
            n = n,
            shuntercastes = shuntercastes,
            ehuntercastes = ehuntercastes,
            pops = pops,
            nactive = nactive,
            npotential = npotential)






parms = c("pr",
          "alpha",
          "mu",
          "kappa","kappat",
          "alphab1",
          "alphab",
          "totkill",
          "killpc",
          "spkillperhunter",
          "sdhunter",
          "mut",
          "pr",
          "ptotkill",
          "ann",
          "cst",
          "spkill"
                     )

adaptSteps = 1000              # Number of steps to "tune" the samplers.
burnInSteps = 5000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=100          # Total number of steps in chains to save.
thinSteps=10                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.

t1 = Sys.time()

    
out2 = jags.model( file = "full zonal species proportional kill model.R", 
                   data= jdat ,  
                   #inits= newinits,  
                   n.chains= nChains , 
                   n.adapt= adaptSteps )

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
  ggmcmc(gg,file = paste(pp,"mcmc.pdf"))
}


newinits = coef(out2) # this object can be used as initial values to re-start the jags


save(list = c("sum","out2","jdat","codaSamples","newinits","prts1","sp.save.out"),
     file = paste(pr,z,spgp,"full harvest model draft.RData"))



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
nhunters = jdat$nhunters

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
      n.obs <- sum(periodkill[per,y,1:nhunters[y]],na.rm = T)
      p.obs <- n.obs/sum(periodkill[,y,1:nhunters[y]])
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