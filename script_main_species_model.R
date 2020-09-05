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
# if(length(tkp)>0){
  allkill = allkill[tkp,]
#}### removing the hunters sampled from last year's permit file who indicated they didn't buy a permit this year
### and are therefore not potential hunters


trem = which(allkill$PERMIT == 0)
if(length(trem)>0){
  allkill = allkill[-trem,]
}### removes a single permit from 1985 with no permit number



allkill$uniperm = allkill$PERMIT + allkill$SELYEAR*1000000 + allkill$YEAR*10000000000
dupuni = allkill$uniperm[duplicated(allkill$uniperm)]
## there are no duplicates.
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



 save(list = c("allkill"),
      file = "data/allkill.RData")
 
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

aou.ducks <- sps[which(sps$group == "duck"),"AOU"]
aou.goose <- sps[which(sps$group == "goose"),"AOU"]
aou.murre <- sps[which(sps$group == "murre"),"AOU"]







# correcting the age and sex indicators -----------------------------------

outscse[which(outscse$PAGE != ""),"BAGE"] <- outscse[which(outscse$PAGE != ""),"PAGE"]
# outscse[which(outscse$BAGE %in% c("2")),"BAGE"] <- "I"
# outscse[which(outscse$BAGE %in% c("3")),"BAGE"] <- "U"
# outscse[which(outscse$BAGE %in% c("")),"BAGE"] <- "U"



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
    mod.file = "models/species_harvest_model.R" # 
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
    mod.file = "models/species_harvest_model.R" #
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
    mod.file = "models/species_harvest_model.R" # I think this should work for murres too
 
    
    non_res_combine = c("NF 1","NF 2","PE 1","NS 1","NS 2","BC 2","NT 1","YT 1")
    
  }

  

# Province and Zone loop --------------------------------------------------
  n_cores <- length(provs2)
  cluster <- makeCluster(n_cores, type = "PSOCK")
  registerDoParallel(cluster)
  
  
  
  fullrun <- foreach(pr = provs2,
                     .packages = c("jagsUI","tidyverse"),
                     .inorder = FALSE,
                     .errorhandling = "pass") %dopar%
    {
      
  
  zns <- unique(period[which(period$pr == pr),"zo"])
  
  # Set up parallel stuff
   
  for(z in zns){


load(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))




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
          "days_y",
          "nu",
          "sdhunter",
          "cst",
          "cst_day",
          "ann",
          "axcomp_axsy",
          "ann_day",
          "padult_sy",
          "pfemale_sy",
          "kill_ysax",
          "pcomp_psy",
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

   


# MCMC sampling -----------------------------------------------------------


  
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



  save(list = c("out2","jdat","sp.save.out"),
       file = paste("output/full harvest zip",pr,z,spgp,"alt mod.RData"))
  

rm(list = "out2")

}

  }#z

}#pr
  stopCluster(cl = cluster)
  


}#spgp
# plotting comparisons to published estimates -----------------------------







# plotting comparisons to published estimates -----------------------------

source("functions/comparison_plotting_function_caste_year.R")
source("functions/comparison_plotting_function_sdhunter_year.R")
source("functions/comparison_plotting_function_species_agesex.R")
source("functions/comparison_plotting_function_species.R")
source("functions/comparison_plotting_function_species_period_props.R")
source("functions/comparison_plotting_function.R")
source("functions/comparison_plotting_function_hunter_distr.R")

source("functions/utility_functions.R")




for(spgp in c("goose","duck","murre")){
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
    mod.file = "models/species_harvest_model.R" # 
    non_res_combine = c("NF 1","NF 2","PE 1","NS 1","NS 1","BC 2","NT 1","YT 1","NB 1")
    
    z_means <- allkill %>% 
      filter(ACTIVEWF == "Y",
             ZOHUNTG %in% c(1,2,3)) %>% 
      group_by(PRHUNTG,ZOHUNTG,YEAR,CASTE) %>% 
      summarise(mean_harv = mean(TOGOK,na.rm = T),
                DAYWF = mean(DAYWF,na.rm = T),
                sd_harv = sd(TOGOK,na.rm = T),
                nresp = n())
    names(z_means) <- c("prov","zone","year","caste","mean_harv","mean_day","sd_harv","nresp")
    
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
    mod.file = "models/species_harvest_model.R" #
    non_res_combine = c("NF 1","NF 2","PE 1","NS 1","NS 1","BC 2","NT 1","YT 1")
    
    z_means <- allkill %>% 
      filter(ACTIVEWF == "Y",
             ZOHUNT %in% c(1,2,3)) %>% 
      group_by(PRHUNT,ZOHUNT,YEAR,CASTE) %>% 
      summarise(mean_harv = mean(TODUK,na.rm = T),
                DAYWF = mean(DAYWF,na.rm = T),
                sd_harv = sd(TODUK,na.rm = T),
                nresp = n())
    names(z_means) <- c("prov","zone","year","caste","mean_harv","mean_day","sd_harv","nresp")
    
    
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
    mod.file = "models/species_harvest_model.R" # I think this should work for murres too
    
    z_means <- allkill %>% 
      filter(ACTIVEM == "Y",
             ZOHUNTM %in% c(1,2)) %>% 
      group_by(PRHUNTM,ZOHUNTM,YEAR,CASTE) %>% 
      summarise(mean_harv = mean(MURRK,na.rm = T),
                DAYM = mean(DAYM,na.rm = T),
                sd_harv = sd(MURRK,na.rm = T),
                nresp = n())
   names(z_means) <- c("prov","zone","year","caste","mean_harv","mean_day","sd_harv","nresp")
    
    non_res_combine = c("NF 1","NF 2","PE 1","NS 1","NS 1","BC 2","NT 1","YT 1")
    
  }
  
  
 

# plotting loop -----------------------------------------------------------

   
  jjcomp = 1
  compps <-  list() 
  
  jjsp = 1
  spplts_list <-  list() 
  
  jjsimcomp = 1
  simcomp_list <-  list() 
  
  jjcst = 1
  cst_list <-  list() 
  
  
  jjpsy = 1
  psy_list <-  list() 
  
  jjpaxsy = 1
  paxsy_list <- list()
  
  
  # jjsdhunter = 1
  # sdhunter_list <- list()
  # 
  
  for(pr in provs2){
    zns <- unique(period[which(period$pr == pr),"zo"])
    for(z in zns){
#       if(file.exists(paste("output/full harvest",pr,z,spgp,"mod.RData"))){
# load(paste("output/full harvest",pr,z,spgp,"mod.RData"))
      
      if(spgp == "goose"){
        
             mod.saved = paste("output/full harvest zip",pr,z,spgp,"alt mod.RData")
        
      }else{
             mod.saved = paste("output/full harvest zip",pr,z,spgp,"alt mod.RData")
      }
      
      #mod.saved = paste("output/full harvest time sdhunter",pr,z,spgp,"alt mod.RData") #paste("output/full harvest",pr,z,spgp,"alt mod.RData")
        if(file.exists(mod.saved)){
          load(mod.saved) #load(paste("output/full harvest",pr,z,spgp,"alt mod.RData"))#        load(paste("output/full harvest caste time",pr,z,spgp,"mod.RData"))

         

          
          var_pair = data.frame(new = c("NACTIVE_y",
                              "NSUCC_y",
                              "kill_y",
                              "days_y"),
                      duck = c("ACTIWF",
                               "SUTODU",
                               "TODUK",
                               "DAYWF"),
                      goose = c("ACTIWF",
                               "SUTOGO",
                               "TOGOK",
                               "DAYWF"),
                      stringsAsFactors = F) ## add ofther spgp columns to match

plts = list()
length(plts) = nrow(var_pair)
#pdf(paste("output/comparison graphs",pr,z,"simple.pdf"))

for(i in 1:nrow(var_pair)){

  
plts[[i]] <-  comp_plot_simple(group = spgp,
var = var_pair[i,"new"],
prov = pr,
zone = z,
M = out2)


#print(plts[[i]])
}


simcomp_list[[jjsimcomp]] <- plts
#dev.off()

jjsimcomp <- jjsimcomp + 1



# species harvests -----------------------------------------------------

spplts_list[[jjsp]] <- comp_plot_species(prov = pr,
                            zone = z)

jjsp = jjsp +1
# pdf(paste0("output/species_level_harvests_",pr,z,".pdf"),width = 8,height = 10)
# for(pp in 1:length(spplts)){print(spplts[[pp]])}
# dev.off()


# caterpillar plots -------------------------------------------------------
# gg = ggs(out2$samples)
# pdf(file = paste0("output/converge/caterpillar zip",spgp,pr,z,".pdf"), 
#     height = 22,width = 8.5)
# for(pps in c("nu","sdhunter","parrive","pleave","ann","psi")){
#   print(ggs_caterpillar(gg,family = pps))
# }
# dev.off()
# 





# sdhunter_list[[jjsdhunter]] <- comp_plot_sdhunter_year(prov = pr,
#                                          zone = z)
# 
# jjsdhunter = jjsdhunter +1

# Species composition -----------------------------------------------------


psy_list[[jjpsy]] <- comp_plot_psy(prov = pr,
              zone = z)

jjpsy <- jjpsy+1
# age and sex composition -------------------------


paxsy_list[[jjpaxsy]] <- comp_plot_axsy(prov = pr,
                                        zone = z)

  jjpaxsy = jjpaxsy + 1

# caste comparisons -----------------------------------------------------

cst_list[[jjcst]] <- comp_plot_caste_year(prov = pr,
                                         zone = z)

jjcst = jjcst +1
# pdf(paste0("output/species_level_harvests_",pr,z,".pdf"),width = 8,height = 10)
# for(pp in 1:length(spplts)){print(spplts[[pp]])}
# dev.off()

# comparing retransformation options --------------------------------------


source("functions/palette.R")
### mean kill
dsum = as.data.frame(out2$summary)
names(dsum)[3:7] <- c("lci","lqrt","med","uqrt","uci")
dsum$Parameter = row.names(dsum)
d1 = filter(dsum,grepl(Parameter,pattern = "mean_totkill_yc\\["))
d1$vers = "retrans"
d1$yr = jags_dim(var = "mean_totkill_yc",dat = d1)
d1$caste = jags_dim(var = "mean_totkill_yc",dat = d1,dim = 2)

d2 = filter(dsum,grepl(Parameter,pattern = "mean_totkill_yc_alt"))
d2$vers = "smear"
d2$yr = jags_dim(var = "mean_totkill_yc_alt",dat = d2)
d2$caste = jags_dim(var = "mean_totkill_yc_alt",dat = d2,dim = 2)

dd = bind_rows(d1,d2)

dd$year = years[dd$yr]



# if(spgp == "goose"){
#   for(y in 1:nyears){
#     
#     dpsi = filter(dsum,grepl(Parameter,pattern = paste0("psi[",y,"]"),fixed = T))
#   
#   for(jj in c("mean","lci","lqrt","med","uqrt","uci")){
#     dd[which(dd$yr == y),jj] <- dd[which(dd$yr == y),jj]*dpsi$med
#   }
# }
# }
  csts = c("D","B","A","E")


for(i in 1:max(dd$caste)){
  ww = which(dd$caste == i)
  dd[ww,"castes"] <- csts[i]
  
}

  
  ## observed means
  ob_m <- filter(z_means,
                 prov == pr,
                 zone == z)
  psi = filter(dsum,grepl(Parameter,pattern = "psi\\["))
  psi = data.frame(psi = psi$mean,
                   year = 1976:2019)
ob_m = left_join(ob_m,psi,by = "year")
ob_m = mutate(ob_m,
              mean_harv = mean_harv*psi)


if(max(dd$caste) == 3){
  dd$castes = factor((dd$castes),ordered = T,levels = c("D","B","A")) #D-renewal > 1year, B-renewal = 1year, A-nonrenewal (new hunter), E-nonresident
  ob_m$castes = factor((ob_m$caste),ordered = T,levels = c("D","B","A"))
}else{
  
  dd$castes = factor((dd$castes),ordered = T,levels = c("D","B","A","E")) #D-renewal > 1year, B-renewal = 1year, A-nonrenewal (new hunter), E-nonresident
  ob_m$castes = factor((ob_m$caste),ordered = T,levels = c("D","B","A","E"))
}


for(i in 1:nrow(dd)){
  cc = dd[i,"caste"]
  yy = dd[i,"yr"]

  dd[i,"nhunter"] <- jdat$nhunter_cy[cc,yy]
  dd[i,"nperm"] <- jdat$pops[cc,yy]
}



ulim = max(dd$uci)
ddb = dd[which(dd$vers == "retrans"),]
ddb$hunterplot <- (ddb$nhunter/max(ddb$nhunter))*(ulim/2)
ddb$permplot <- (ddb$nhunter/ddb$nperm)*(ulim*4)
ddbmx = tapply(ddb$nhunter,ddb$castes,max)
wm = NULL
ddbmn = tapply(ddb$nhunter,ddb$castes,min)
wmn = NULL

for(j in 1:length(ddbmx)){
  wm[j] <- which(ddb$nhunter == ddbmx[j] & ddb$castes == names(ddbmx)[j])
  wmn[j] <- which(ddb$nhunter == ddbmn[j] & ddb$castes == names(ddbmn)[j])
}
ddbm = ddb[c(wm,wmn),]

dd$vers = factor(dd$vers,ordered = T,levels = c("smear","retrans"))

ob_m$psi = ob_m$psi*(ulim/2)
ob_m$sd_harv = (ob_m$sd_harv/ob_m$mean_harv)*(ulim/4)
compp = ggplot(data = dd,aes(x = year,y = mean,fill = vers))+
  geom_bar(data = ddb,inherit.aes = FALSE,aes(x = year,y = hunterplot),fill = grey(0.2),alpha = 0.1,stat = "identity")+
  #geom_point(data = ddb,aes(x = year, y = permplot),colour = "blue",inherit.aes = FALSE,alpha = 0.2)+
  geom_point(aes(colour = vers))+
  geom_point(data = ob_m,aes(x = year, y = mean_harv),colour = grey(0.5),inherit.aes = FALSE,alpha = 0.2)+
  geom_point(data = ob_m,aes(x = year, y = psi),colour = "green",inherit.aes = FALSE,alpha = 0.2)+
  geom_point(data = ob_m,aes(x = year, y = sd_harv),colour = "blue",inherit.aes = FALSE,alpha = 0.2)+
  geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.3)+
  labs(title = paste0("retrans comparison KILL",pr," zn",z," (mean and 95 CI)"))+
  scale_y_continuous(limits = c(0,ulim))+
  my_col + #scale_color_viridis_d(aesthetics = c("colour","fill"), end = 0.7)+
  theme_classic()+
  geom_text_repel(data = ddbm,inherit.aes = FALSE,aes(x = year,y = hunterplot,label = nhunter),size = 3,colour = grey(0.2),alpha = 0.75,nudge_y = ulim*-0.1)+
  facet_wrap(facets = ~castes,ncol = 2,scales = "fixed")

compps[[jjcomp]] <- compp
jjcomp = jjcomp +1
### mean days
dsum = as.data.frame(out2$summary)
names(dsum)[3:7] <- c("lci","lqrt","med","uqrt","uci")
dsum$Parameter = row.names(dsum)
d1 = filter(dsum,grepl(Parameter,pattern = "mean_totdays_yc\\["))
d1$vers = "retrans"
d1$yr = jags_dim(var = "mean_totdays_yc",dat = d1)
d1$caste = jags_dim(var = "mean_totdays_yc",dat = d1,dim = 2)

d2 = filter(dsum,grepl(Parameter,pattern = "mean_totdays_yc_alt"))
d2$vers = "smear"
d2$yr = jags_dim(var = "mean_totdays_yc_alt",dat = d2)
d2$caste = jags_dim(var = "mean_totdays_yc_alt",dat = d2,dim = 2)

dd = bind_rows(d1,d2)

dd$year = years[dd$yr]

for(i in 1:max(dd$caste)){
  ww = which(dd$caste == i)
  dd[ww,"castes"] <- csts[i]
  
}

if(max(dd$caste) == 3){
  dd$castes = factor((dd$castes),ordered = T,levels = c("D","B","A")) #D-renewal > 1year, B-renewal = 1year, A-nonrenewal (new hunter), E-nonresident
  
}else{
  
dd$castes = factor((dd$castes),ordered = T,levels = c("D","B","A","E")) #D-renewal > 1year, B-renewal = 1year, A-nonrenewal (new hunter), E-nonresident
}


for(i in 1:nrow(dd)){
  cc = dd[i,"caste"]
  yy = dd[i,"yr"]

  dd[i,"nhunter"] <- jdat$nhunter_cy[cc,yy]
}


# if(max(to_plot$nrts) > 200){
#   ncby_y = ceiling(to_plot$nrts/50)
#   annot = c("each dot ~ 50 routes")
# }else{
#   ncby_y = to_plot$nrts
#   annot = c("each dot = 1 route")
#
# }

ulim = max(dd$uci)
ddb = dd[which(dd$vers == "smear"),]
ddb$hunterplot <- (ddb$nhunter/max(ddb$nhunter,na.rm = T))*(ulim/2)
ddbmx = tapply(ddb$nhunter,ddb$castes,max)
wm = NULL
ddbmn = tapply(ddb$nhunter,ddb$castes,min)
wmn = NULL

for(j in 1:length(ddbmx)){
  wm[j] <- which(ddb$nhunter == ddbmx[j] & ddb$castes == names(ddbmx)[j])
  wmn[j] <- which(ddb$nhunter == ddbmn[j] & ddb$castes == names(ddbmn)[j])
}
ddbm = ddb[c(wm,wmn),]

dd$vers = factor(dd$vers,ordered = T,levels = c("smear","retrans"))


compp = ggplot(data = dd,aes(x = year,y = mean,fill = vers))+
  geom_bar(data = ddb,inherit.aes = FALSE,aes(x = year,y = hunterplot),fill = grey(0.2),alpha = 0.1,stat = "identity")+
  geom_point(aes(colour = vers))+
  geom_point(data = ob_m,aes(x = year, y = mean_day),colour = grey(0.5),inherit.aes = FALSE,alpha = 0.2)+
  #geom_point(data = ob_m,aes(x = year, y = psi),colour = "green",inherit.aes = FALSE,alpha = 0.2)+
  geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.3)+
  labs(title = paste0("retrans comparison DAYS",pr," zn",z," (mean and 95 CI)"))+
  scale_y_continuous(limits = c(0,ulim))+
  my_col + #scale_color_viridis_d(aesthetics = c("colour","fill"), end = 0.7)+
  theme_classic()+
  geom_text_repel(data = ddbm,inherit.aes = FALSE,aes(x = year,y = hunterplot,label = nhunter),size = 3,colour = grey(0.2),alpha = 0.75,nudge_y = ulim*-0.1)+
  facet_wrap(facets = ~castes,ncol = 2,scales = "fixed")


compps[[jjcomp]] <- compp
jjcomp = jjcomp +1





}#end if jags output exists 



 }#z

}#pr

  
########## add a time-series plot of hte sd hunter values across castes (maybe possible to have a single sdhunter for all but caste 4)
# plotting hunter effects -------------------------------------------------


  # 
  # 
  # jjhunter = 1
  # hunter_list <- list()
  # 
  # 
  # for(pr in provs2){
  #   zns <- unique(period[which(period$pr == pr),"zo"])
  #   for(z in zns){
  #     #       if(file.exists(paste("output/full harvest",pr,z,spgp,"mod.RData"))){
  #     # load(paste("output/full harvest",pr,z,spgp,"mod.RData"))
  #       if(file.exists(paste("output/hunter_effects",pr,z,spgp,"alt mod.RData"))){
  #         
  #         load(paste("output/hunter_effects",pr,z,spgp,"alt mod.RData"))#        load(paste("output/full harvest caste time",pr,z,spgp,"mod.RData"))
  #         
  #         
  #         
  #         
  #         
  #         
  #         hunter_list[[jjhunter]] <- comp_plot_hunter(prov = pr,zone = z)
  #         
  #         jjhunter = 1+jjhunter      
  #       }
  #       
  #     rm(out3)   
  #   
  #   }
  # }

  
  # plotting pdfs -----------------------------------------------------------
  
  
  
  asuf <- c("ZIP")
  
#   pdf(paste0("output/sdhunter timeseries",asuf," ",spgp,".pdf"),
#   width = 8,
#   height = 6)
# for(jj in 1:length(sdhunter_list)){
#   print(sdhunter_list[[jj]])
# }
# dev.off()


  #asuf <- c(" alt")
  pdf(paste0("output/retransformation comparison",asuf," ",spgp,".pdf"),
      width = 8,
      height = 6)
  for(jj in 1:length(compps)){
    print(compps[[jj]])
  }
  dev.off()
  

  
  pdf(paste("output/comparison graphs simple",asuf," ",spgp,".pdf"))
  
  for(pp in 1:length(simcomp_list)){
    plt = simcomp_list[[pp]]
    for(j in 1:length(plt)){
      print(plt[[j]])
    }}
  dev.off()
  
  
  
 
  
  # pdf(paste0("output/caste effects",asuf," ",spgp,".pdf"),
  #     width = 8,
  #     height = 6)
  # for(jj in 1:length(cst_list)){
  #   print(cst_list[[jj]])
  # }
  # dev.off()
  
  
  pdf(paste0("output/species_level_harvests",asuf," ",spgp,".pdf"),width = 8,height = 10)
  for(pp in 1:length(spplts_list)){
    plt = spplts_list[[pp]]
    for(j in 1:length(plt)){
    print(plt[[j]])
    }}
  dev.off()
  
  

  
  pdf(paste0("output/age sex",asuf," ",spgp,".pdf"),
      width = 8,
      height = 6)
  for(pp in 1:length(paxsy_list)){
    plt = paxsy_list[[pp]]
    for(j in 1:length(plt)){
      print(plt[[j]])
    }}
  dev.off()
  
  
  
  pdf(paste0("output/species proportions by period",asuf," ",spgp,".pdf"),
      width = 8,
      height = 6)
  for(pp in 1:length(psy_list)){
    plt = psy_list[[pp]]
    for(j in 1:length(plt)){
      print(plt[[j]])
    }}
  dev.off()
  
  
  

# plotting hunter effects if necessary ------------------------------------

  # 
  # pdf(paste0("output/hunter effects",asuf," ",spgp,".pdf"),
  #     width = 8,
  #     height = 6)
  # for(pp in 1:length(hunter_list)){
  #   plt = hunter_list[[pp]]
  #   for(j in 1:length(plt)){
  #     print(plt[[j]])
  #   }}
  # dev.off()
  # 
  # 
  # 
  
   
  
}#spgp (species group)
#######################################

