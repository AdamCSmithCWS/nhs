
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



others = c("COOTK","WOODK","SNIPK","DOVEK","PIGEK","CRANK","RAILK","MURRK")



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

for(pr in provs[12]){
  
  
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





