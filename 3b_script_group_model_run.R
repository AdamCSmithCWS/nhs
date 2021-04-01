
# Script to run simplified harvest model for other game birds -------------



Y <- 2019
FY = 1976
years <- FY:Y

names(years) <- paste(years)

library(jagsUI)
library(tidyverse)
library(doParallel)
library(foreach)



provs = c("AB","BC","SK","MB","ON","PQ","NS","PE","NB","NF","NT","YT")#,"NU") #All prov

# load(paste0("data/parts and harvest survey info",Y,".RData"))
# 
# provzone <- read.csv("data/Province and zone table.csv")


# MCMC loops --------------------------------------------------------------
mod.file = "models/group_model.R" # 

# Set up parallel stuff if desired
# n_cores <- length(provs)
# cluster <- makeCluster(n_cores, type = "PSOCK")
# registerDoParallel(cluster)


# fullrun <- foreach(pr = provs,
#                    .packages = c("jagsUI","tidyverse"),
#                    .inorder = FALSE,
#                    .errorhandling = "pass") %dopar%
#   {

for(pr in provs){



for(z in 1:3){
# fullrun <- foreach(z = zns,
#                    .packages = c("jagsUI","tidyverse"),
#                    .inorder = FALSE,
#                    .errorhandling = "pass") %dopar%
#   {
    
    
  
    if(file.exists(paste("data/data",pr,z,"other_save.RData",sep = "_"))){
    load(paste("data/data",pr,z,"other_save.RData",sep = "_"))

parms = c("NACTIVE_y",
          "NSUCC_yg",
          "nu_day",
          "sdhunter_day",
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


  out2 = try(jagsUI(data = jdat,
                    parameters.to.save = parms,
                    n.chains = 3,
                    n.burnin = burnInSteps,
                    n.thin = thinSteps,
                    n.iter = nIter,
                    parallel = T,
                    #modules = "glm",
                    model.file = mod.file),silent = F)

  

if(class(out2) != "try-error"){
  save(list = c("out2","jdat","grps"),
       file = paste("output/other harvest zip",pr,z,"alt mod.RData"))
  
rm(list = "out2")
}
}

  }#z

}#pr
stopCluster(cl = cluster)




  

