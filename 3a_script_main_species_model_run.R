

Y <- 2019
FY = 1976
years <- FY:Y

names(years) <- paste(years)


library(jagsUI)
library(tidyverse)
library(doParallel)
library(foreach)

provs = c("AB","BC","SK","MB","ON","PQ","NS","PE","NB","NF","NT","YT")#,"NU") #All prov


for(spgp in c("murre","duck","goose")){

  if(spgp == "goose"){
   provs2 <- provs
    }
  if(spgp == "duck"){
   provs2 <- provs
    }
  if(spgp == "murre"){
    provs2 = "NF"
   }

  mod.file = "models/species_harvest_model.R" # I think this should work for murres too
  

# Province and Zone loop --------------------------------------------------
  # n_cores <- length(provs2)
  # cluster <- makeCluster(n_cores, type = "PSOCK")
  # registerDoParallel(cluster)
  # 
  # 
  # 
  # fullrun <- foreach(pr = provs2,
  #                    .packages = c("jagsUI","tidyverse"),
  #                    .inorder = FALSE,
  #                    .errorhandling = "pass") %dopar%
  #   {
  #     
 for(pr in provs){ 
 
  for(z in 1:3){

if(file.exists(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))){
load(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))



#parameters to monitor
parms = c("NACTIVE_y",
          "NSUCC_y",
          "nu_day",
          "sdhunter_day",
          "mean_totkill_yc",
          "mean_totdays_yc",
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
          "psi",
          "pkill_py",
          "mut")


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
  


}
}
  }#z

}#pr
  stopCluster(cl = cluster)
  


}#spgp






