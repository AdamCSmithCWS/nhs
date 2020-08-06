
#############################3
## script to summarize the zone level estimates into the national and provincial estimates
library(jagsUI)
library(tidyverse)
library(ggmcmc)
library(tidybayes)
library(ggrepel)
library(ggforce)

### caste level summaries
### full summaries
### harvest, activity, total group-level and species-level
### age-sex summaries
### age-sex raw data for website - 




# load website published estimates ----------------------------------------



# load published estimates by zone prov and national ----------------------------------------

for(rr in c("by_zone","by_province","canada-wide")){
  tmp = read.csv(paste0("data/enp_nhs_a_",rr,"_20200805.csv"),stringsAsFactors = F)
  tmp1 = read.csv(paste0("data/enp_nhs_b_",rr,"_20200805.csv"),stringsAsFactors = F)
  tmp2 = read.csv(paste0("data/enp_nhs_c_",rr,"_20200805.csv"),stringsAsFactors = F)
  # names(tmp) <- c("var","name","prov","zone","resid","year","mean","sd")
  if(rr == "by_zone"){
    pubEsts_simple_all <- tmp
    pubEsts_species_all <- tmp1
    pubEsts_age_sex_all <- tmp2
  }else{
      pubEsts_simple_all <- bind_rows(pubEsts_simple_all,tmp)
      pubEsts_species_all <- bind_rows(pubEsts_species_all,tmp1)
      pubEsts_age_sex_all <- bind_rows(pubEsts_age_sex_all,tmp2)
  }
 
} 
  
  names(pubEsts_simple_all) <- c("var","name","prov","zone","resid","year","mean","sd")
pubEsts_simple_all$lci = ceiling(pubEsts_simple_all$mean-(1.96*pubEsts_simple_all$sd))
pubEsts_simple_all$uci = ceiling(pubEsts_simple_all$mean+(1.96*pubEsts_simple_all$sd))
pubEsts_simple_all[which(pubEsts_simple_all$lci < 0),"lci"] <- 0


names(pubEsts_species_all) <- c("sp","species","prov","zone","year","mean","sd")
pubEsts_species_all$lci = ceiling(pubEsts_species_all$mean-(1.96*pubEsts_species_all$sd))
pubEsts_species_all$uci = ceiling(pubEsts_species_all$mean+(1.96*pubEsts_species_all$sd))
pubEsts_species_all[which(pubEsts_species_all$lci < 0),"lci"] <- 0


names(pubEsts_age_sex_all) <- c("sp","species","prov","zone","year","age_ratio")


species_web_names = unique(pubEsts_species_all[,c("sp","species")])

var_names_sim <- unique(pubEsts_simple_all[,c("var","name")])
# write.csv(var_names_sim,"data/website_variable_names.csv",row.names = F)
# 
# write.csv(species_web_names,"data/website_species_variable_names.csv",row.names = F)

# load all output from species models and other models --------------------------

load(paste0("data/parts and harvest survey info",Y,".RData"))

provzone <- read.csv("data/Province and zone table.csv")


# Load other harvest regulations ------------------------------------------

others = c("COOTK","WOODK","SNIPK","DOVEK","PIGEK","CRANK","RAILK","MURRK")

regs_other <- list()
length(regs_other) <- length(others)
names(regs_other) <- others

for(spgp in others){ 
  tmp <- read.csv(file = paste0("data/reg_",spgp,".csv"))
  names(tmp)[which(names(tmp) == "QC")] <- "PQ"
  regs_other[[spgp]] <- tmp
}


# compile website file b --------------------------------------------------

sp_vars <- read.csv("data/website_species_variable_names_in.csv")
sim_vars <- read.csv("data/website_variable_names_in.csv")


gps <- c("duck",
           "goose",
           "murre",
           "other")

for(pr in provs){
  

# other data load ---------------------------------------------------------

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
  

  wkill = grps
  wsucc = paste0("SU",gsub("K",replacement = "",x = grps)) 

  
   for(z in 1:3){
     if(file.exists(paste("output/full harvest zip",pr,z,gp,"alt mod.RData"))){
       load(paste("output/full harvest zip",pr,z,"duck","alt mod.RData"))
     
           tmp_duck <- out2$samples %>% gather_draws(NACTIVE_y[y],
                                                days_y[y],
                                                NSUCC_y[y],
                                                kill_y[y]) 
           }
     
     if(file.exists(paste("output/full harvest zip",pr,z,"goose","alt mod.RData"))){
             load(paste("output/full harvest zip",pr,z,"goose","alt mod.RData"))
             
           tmp_goose <- out2$samples %>% gather_draws(NSUCC_y[y],
                                                     kill_y[y]) 
           
       
     }
     
     if(file.exists(paste("output/other harvest zip",pr,z,"alt mod.RData"))){
       load(paste("output/other harvest zip",pr,z,"alt mod.RData"))
       
       vnm <- sim_vars[which(sim_vars$source == "other"),]
       vnm <- rename(vnm,group = var)
       
       tmp_otherk <- out2$samples %>% gather_draws(NACTIVE_y[y],
                                                  days_y[y],
                                                  kill_yg[y,g])
  #harvests
      gpk <- data.frame(g = 1:ngroups,
                       group = grps)
      tmp_otherk <- full_join(tmp_otherk,gpk,by = "g")
      

      tmp_otherk <- left_join(tmp_otherk,vnm,by = "group")
      
  #days
      tmp_otherd <- out2$samples %>% gather_draws(days_yg[y,g])
      gpd <- data.frame(g = 1:ngroups,
                        group = paste0("DA",gsub(grps,pattern = "K",replacement = "")))
      
      tmp_otherd <- full_join(tmp_otherd,gpd,by = "g")
      tmp_otherd <- left_join(tmp_otherd,vnm,by = "group")
      
  #succ    
      tmp_others <- out2$samples %>% gather_draws(NSUCC_yg[y,g])
      gps <- data.frame(g = 1:ngroups,
                        group = paste0("SU",gsub(grps,pattern = "K",replacement = "")))
      
      tmp_others <- full_join(tmp_others,gps,by = "g")
      tmp_others <- left_join(tmp_others,vnm,by = "group")
      
      
      tmp_other <- bind_rows(tmp_otherk,
                             tmp_otherd,
                             tmp_others)
      
      ys <- data.frame(y = 1:jdat$nyears,
                       year = years) 
      
      tmp_other <- full_join(tmp_other,ys,by = "y")
      
      #### this tmp file can be added to the similar regional ones
      #### then the group and year variables will facilitate a full 
      #### tidy summary to generate the national and provincial estimates
      
      }
       
       
       
    
   }
   
 } 
  
  
}




# table output and generation ---------------------------------------------

############# consider if this is necessary. since everything is graphed.
############# figure out which tables should be produced and in what format








