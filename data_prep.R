
####### the following commented out lines ~ 400 lines
## represent the add-hoc processes required to load all the 
## historical harvest survey data
## the line :load("full data prep updated harvest model.RData")
## on about line-400 will load everything created below



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


sashome <- "C:\\Program Files\\SASHome\\SASFoundation\\9.4"
provs = c("AB","BC","SK","MB","ON","PQ","NS","PE","NB","NF")#,"NU","NT","YT") #All prov
#ignoring territories above

sps <- read.csv(paste(home.fold,"/Bird names 2010.csv", sep = ""))
                          species <- unique(sps[which(sps$group %in% c("duck","goose","murre")),"specieslevelenglish"])
                          species <- species[-which(species == "Hybrid Mallard/Northern Pintail")]
                          gnames <- unique(sps[which(sps$group == "goose"),"specieslevelenglish"])
                          dnames <- unique(sps[which(sps$group == "duck"),"specieslevelenglish"])
                          dnames <- dnames[-which(dnames == "Hybrid Mallard/Northern Pintail")]
#
#
#
goose <- TRUE         ### change to false, if no goose species included (simplifies the PEF file extraction)
murre <- T      #change to fales if no murre species should be included
zone <- T           ### change to false, if provincial summaries are desired
# #
# #
# ############## extracting harvest survey data for all years
#
provzone = read.csv("province and zone table.csv",stringsAsFactors = F)
casteslist = read.csv("caste table.csv",stringsAsFactors = F)

#
#
harvw <- list()
length(harvw) <- length(years)
names(harvw) <- as.character(years)
cald = harvw
calg = cald
cls = c("PRHUNT",
        "ZOHUNT",
        "AOU",
        "MONH",
        "DAYH",
        "BAGE",
        "BSEX",
        "SAMPLE",
        "PERMIT",
        "YEAR",
        #"FOLYEAR",
        #"JDLWA",
        "YRHUNT",
        #"JDHUN",
        "WEEK")
#
for (y in years){
  dir.yr <- paste0(home.fold1,y)

  fil.yr <- paste0("harv",substring(y,3,4),"w")
  harvw[[as.character(y)]] <- read.ssd(libname = dir.yr,
                                       sectionnames = fil.yr,
                                       sascmd = file.path(sashome, "sas.exe"))
  fil.yr <- paste0("dcal",substring(y,3,4))
  cald[[as.character(y)]] <- read.ssd(libname = dir.yr,
                                        sectionnames = fil.yr,
                                        sascmd = file.path(sashome, "sas.exe"))
  fil.yr <- paste0("gcal",substring(y,3,4))
  calg[[as.character(y)]] <- read.ssd(libname = dir.yr,
                                        sectionnames = fil.yr,
                                        sascmd = file.path(sashome, "sas.exe"))

 fil.yr = paste0("persum",substring(y,3,4))
 tmpp <- read.ssd(libname = paste0(home.fold1,"/PermitSummary"),
                                     sectionnames = fil.yr,
                                     sascmd = file.path(sashome, "sas.exe"))
 if(any(tmpp$YEAR > 50,na.rm = T) ){
   tmpp$YEAR = tmpp$YEAR+2000
 }else{
   tmpp$YEAR = tmpp$YEAR+1900

 }


 fil.yr = paste0("popsiz",substring(y,3,4))
 tmppop <- read.ssd(libname = paste0(home.fold1,"/PopulationSize"),
                  sectionnames = fil.yr,
                  sascmd = file.path(sashome, "sas.exe"))


  fil.yr <- paste0("scs",substring(y,3,4),"e")
  tmp <- read.ssd(libname = dir.yr,
                                      sectionnames = fil.yr,
                                      sascmd = file.path(sashome, "sas.exe"))

  tmp[which(tmp$PRHUNT == ""),"PRHUNT"] <- tmp[which(tmp$PRHUNT == ""),"PRSALE"]
  tmp[which(tmp$PRHUNT == ""),"ZOHUNT"] <- tmp[which(tmp$PRHUNT == ""),"ZOSALE"]

  ## fixing a handful of years in which column names varied and years were recorded as 2 digits
  if(c("YHUN") %in% names(tmp)){
    names(tmp)[which(names(tmp) == "YHUN")] <- "YRHUNT"
  }
  if(any(tmp$YEAR < min(years),na.rm = T)){
    tmp[which(tmp$YEAR < min(years)),"YEAR"] <- tmp[which(tmp$YEAR < min(years)),"YEAR"]+1900
  }
  if(any(tmp$YRHUNT < min(years),na.rm = T)){
    tmp[which(tmp$YRHUNT < min(years)),"YRHUNT"] <- tmp[which(tmp$YRHUNT < min(years)),"YRHUNT"]+1900
  }
  # if(any(tmp$JDHUN < 1000)){
  #   tmp[which(tmp$JDHUN < 1000),"JDHUN"] <- tmp[which(tmp$JDHUN < 1000),"JDHUN"]+((y-1900)*10)
  # }

  miscls = cls[-which(cls %in% names(tmp))]


  tmp = tmp[,cls]

  if(y == years[1]) {
    outscse <- tmp
    perms = tmpp
    popsiz = tmppop
  }else{
    outscse <- rbind(outscse,tmp)
    perms = rbind(perms,tmpp)
    popsiz = rbind(popsiz,tmppop)
    }
  #



  }#y





#define periods across all years

zones <- 1:3
pers <- 1:20

period.duck <- expand.grid(pr = provs,zo = zones,period = pers,stringsAsFactors = F)

period.duck[,"startweek"] <- NA
period.duck[,"endweek"] <- NA

period.goose <- period.duck

for(pr in provs){
  pzones <- unique(outscse[which(outscse$PRHUNT == pr),"ZOHUNT"])
  if(anyNA(pzones)){pzones <- pzones[-which(is.na(pzones))]}
  for(z in pzones){
    tmp <- outscse[which(outscse$PRHUNT == pr & outscse$ZOHUNT == z & outscse$AOU %in% sps[which(sps$group == "duck"),"AOU"]),]

    testm <- table(tmp[,c("AOU")],tmp[,c("WEEK")])

    wsums <- colSums(testm)
    wprops <- wsums/sum(wsums)

    ##### identify periods based on weeks with at least 5% of the parts across all years
    per1 <- 1
    per2 <- NA
    p = 1
    mw <- F
    q <- 0

    for(w in 1:length(wprops)){
      if(mw){

        if(sum(wprops[per1[p]:(per1[p]+q)]) > 0.05){
          q <- 0
          per2[p] <- w
          p <- p+1
          mw <- F
        }else{
          q <- q+1
        }
      }else{

        if(wprops[w] > 0.05){
          per1[p] <- w
          per2[p] <- w
          p <- p+1
        }else{
          per1[p] <- w
          mw <- T
          q <- q+1
        }
      }

    }#w
    if(length(per1) > length(per2)){per1 <- per1[-length(per1)]
    per2[p-1] <- length(wprops)}

    for(j in 1:length(per1)){
      rs <- which(period.duck$pr == pr & period.duck$zo == z & period.duck$period == j)
      period.duck[rs,"startweek"] <- per1[j]
      period.duck[rs,"endweek"] <- per2[j]

    }

  }#z

}#pr
period.duck <- period.duck[-which(is.na(period.duck$startweek)),]
period.duck <- period.duck[order(period.duck$pr,period.duck$zo),]
write.csv(period.duck,"period.duck.csv",row.names = F)


##### goose periods

for(pr in provs){
  pzones <- unique(outscse[which(outscse$PRHUNT == pr),"ZOHUNT"])
  if(anyNA(pzones)){pzones <- pzones[-which(is.na(pzones))]}
  for(z in pzones){
    tmp <- outscse[which(outscse$PRHUNT == pr & outscse$ZOHUNT == z & outscse$AOU %in% sps[which(sps$group == "goose"),"AOU"]),]

    testm <- table(tmp[,c("AOU")],tmp[,c("WEEK")])

    wsums <- colSums(testm)
    wprops <- wsums/sum(wsums)

    ##### identify periods based on weeks with at least 5% of the parts across all years
    per1 <- 1
    per2 <- NA
    p = 1
    mw <- F
    q <- 0

    for(w in 1:length(wprops)){
      if(mw){

        if(sum(wprops[per1[p]:(per1[p]+q)]) > 0.05){
          q <- 0
          per2[p] <- w
          p <- p+1
          mw <- F
        }else{
          q <- q+1
        }
      }else{

        if(wprops[w] > 0.05){
          per1[p] <- w
          per2[p] <- w
          p <- p+1
        }else{
          per1[p] <- w
          mw <- T
          q <- q+1
        }
      }

    }#w
    if(length(per1) > length(per2)){per1 <- per1[-length(per1)]
    per2[p-1] <- length(wprops)}

    for(j in 1:length(per1)){
      rs <- which(period.goose$pr == pr & period.goose$zo == z & period.goose$period == j)
      period.goose[rs,"startweek"] <- per1[j]
      period.goose[rs,"endweek"] <- per2[j]

    }

  }#z

}#pr
period.goose <- period.goose[-which(is.na(period.goose$startweek)),]
period.goose <- period.goose[order(period.goose$pr,period.goose$zo),]
write.csv(period.goose,"period.goose.csv",row.names = F)




# list = c("harvw",
#          "cald",
#          "calg",
#          "years",
#          "sps",
#          "period.duck",
#          "period.goose",
#          "outscse"),

save.image(file = paste0("parts and harvest survey info",Y,".RData"))
