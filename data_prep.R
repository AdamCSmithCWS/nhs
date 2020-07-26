

# running the new harvest analysis with the full historical databa --------


Y <- 2019
years <- 1976:Y

names(years) <- paste(years)


library(runjags)
library(rjags)
library(tidyverse)
library(lubridate)



provs = c("AB","BC","SK","MB","ON","PQ","NS","PE","NB","NF")#,"NU","NT","YT") #All prov
#ignoring territories above

sps <- read.csv(paste("data/Bird names 2010.csv", sep = ""))
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
# ############## base tables
#
provzone = read.csv("data/province and zone table.csv",stringsAsFactors = F)
casteslist = read.csv("data/caste table.csv",stringsAsFactors = F)



# Permit files -------------------------------------------------------------
tmp <- readLines("data/Permits.tsv")
nrs = length(tmp)
perm = read.table("data/Permits.tsv",
                  stringsAsFactors = F,skipNul = T, blank.lines.skip = F,comment.char = "",
                  nrows = nrs,
                  header = TRUE,
                  sep = "\t",
                  quote = "",
                  dec = ".",
                  strip.white = F)
perm.o <- perm

tmp <- readLines("data/PermitHistorical.tsv")
nrs = length(tmp)
permh = read.table("data/PermitHistorical.tsv",
                   stringsAsFactors = F,skipNul = T, blank.lines.skip = F,comment.char = "",
                   nrows = nrs,
                   header = TRUE,
                   sep = "\t",
                   quote = "",
                   dec = ".",
                   strip.white = F)
permh.o <- permh

intcols = c("SMONTH",
            "SDAY",
            "SALEZONE",
            "SALELAT",
            "SALELON",
            "HUNTING_LATITUDE",
            "HUNTING_LONGITUDE",
            "HUNTING_ZONE",
            "RESIDENCE_LATITUDE",
            "RESIDENCE_LONGITUDE",
            "RESIDENCE_ZONE",
            "SAMPLING_ZONE")
for(i in intcols){
  perm[,i] <- as.integer(perm[,i])
}

perm[which(perm$SDAY == 0 | is.na(perm$SDAY) | perm$SDAY > 31),"SDAY"] <- 1 #if missing sale day info, assume first of month
perm[which(perm$SMONTH == 0| is.na(perm$SMONTH) | perm$SMONTH > 12),"SMONTH"] <- 9 #if missing sale month info, assume September
perm[which(perm$SMONTH %in% c(4,6,9,11) & perm$SDAY == 31),"SMONTH"] <- perm[which(perm$SMONTH %in% c(4,6,9,11) & perm$SDAY == 31),"SMONTH"]-1  #if day = 31 and month is wrong, set month back by 1
perm[which(perm$SMONTH %in% c(2) & perm$SDAY > 28),"SDAY"] <- 28 #if day is February 29 set to feb 28 (weird, but avoids the odd record with feb 29 as date but not in leap year)

perm$fdy <- paste(perm$SYEAR,perm$SMONTH,perm$SDAY,sep = "-")
#perm$dy <- paste(2016,perm$SMONTH,perm$SDAY,sep = "-")

perm <- mutate(perm,full_date = ymd(fdy),
         doy = yday(ymd(fdy))) 


cutoff_date <- yday(ymd("2016-3-10")) #this is the last day on the harvest calendars (March 10)
start_date <- yday(ymd("2016-7-31")) #August 1 is the earliest day that one can purchase a new permit for the coming year

perm <- filter(perm,(doy < cutoff_date | doy > start_date))

perm$year_season <- perm$SYEAR
perm[which(perm$doy < cutoff_date),"year_season"] <- perm[which(perm$doy < cutoff_date),"year_season"]-1

py = table(perm$year_season)
py2 = table(perm$SYEAR)





#
#
harvw <- list()
length(harvw) <- length(years)
names(harvw) <- as.character(years)
cald = harvw
calg = cald
calm = cald

cls = c("PRHUNT",
        "ZOHUNT",
        "AOU",
        "MONH",
        "DAYH",
        "BAGE",
        "BSEX",
        "PAGE",
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

   if(y > 2012){
   fil.yr <- paste0("mcal",substring(y,3,4))
   calm[[as.character(y)]] <- read.ssd(libname = dir.yr,
                                       sectionnames = fil.yr,
                                       sascmd = file.path(sashome, "sas.exe"))
 }
 fil.yr = paste0("persal",substring(y,3,4))
 tmpp <- read.ssd(libname = paste0(home.fold1,"/PermitSales"),
                                     sectionnames = fil.yr,
                                     sascmd = file.path(sashome, "sas.exe"))
 if(any(tmpp$YEAR > 50,na.rm = T) ){
   tmpp$YEAR = tmpp$YEAR+1900
 }else{
   tmpp$YEAR = tmpp$YEAR+2000

 }


 fil.yr = paste0("popsiz",substring(y,3,4))
 tmppop <- read.ssd(libname = paste0(home.fold1,"/PopulationSize"),
                  sectionnames = fil.yr,
                  sascmd = file.path(sashome, "sas.exe"))


 ### if desired to swap BAGE for PAGE (geese), then scsYY is required, instead of scsYYe
 ### but then additional changes are needed to align with old data
  fil.yr <- paste0("scs",substring(y,3,4),"e")
  tmp <- read.ssd(libname = dir.yr,
                                      sectionnames = fil.yr,
                                      sascmd = file.path(sashome, "sas.exe"))
  # fil.yr <- paste0("scs",substring(y,3,4)) 
  # tmp2 <- read.ssd(libname = dir.yr,
  #                  sectionnames = fil.yr,
  #                  sascmd = file.path(sashome, "sas.exe"))
  # 
  # tmp2u <- unique(tmp2[,c("PRHUNT","ZOHUNT","AOU","MONH","DAYH","BAGE","BSEX","PAGE","PERMIT")])

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

  if(length(miscls) > 0){
    
  if(miscls == "PAGE"){
    tmp$PAGE <- ""
  }
  }
  
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

print(y)

  }#y




# Fix species AOU values incl Eiders --------------------------------------

outscse[which(outscse$AOU == 1600),"AOU"] <- 1590 #cod for COEI changed in ~1990

#define periods across all years

zones <- 1:3
pers <- 1:20

period.duck <- expand.grid(pr = provs,zo = zones,period = pers,stringsAsFactors = F)

period.duck[,"startweek"] <- NA
period.duck[,"endweek"] <- NA

period.goose <- period.duck
period.murre <- period.duck

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
write.csv(period.duck,"data/period.duck.csv",row.names = F)


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
write.csv(period.goose,"data/period.goose.csv",row.names = F)



##### murre periods

for(pr in "NF"){
  pzones <- unique(outscse[which(outscse$PRHUNT == pr),"ZOHUNT"])
  if(anyNA(pzones)){pzones <- pzones[-which(is.na(pzones))]}
  for(z in pzones){
    tmp <- outscse[which(outscse$PRHUNT == pr & outscse$ZOHUNT == z & outscse$AOU %in% sps[which(sps$group == "murre"),"AOU"]),]
    
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
      rs <- which(period.murre$pr == pr & period.murre$zo == z & period.murre$period == j)
      period.murre[rs,"startweek"] <- per1[j]
      period.murre[rs,"endweek"] <- per2[j]
      
    }
    
  }#z
  
}#pr
period.murre <- period.murre[-which(is.na(period.murre$startweek)),]
period.murre <- period.murre[order(period.murre$pr,period.murre$zo),]
write.csv(period.murre,"data/period.murre.csv",row.names = F)



save.image(file = paste0("data/parts and harvest survey info",Y,".RData"))
