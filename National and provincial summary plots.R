
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

Y <- 2019
FY = 1976
years <- FY:Y

names(years) <- paste(years)



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
pubEsts_simple_all[which(is.na(pubEsts_simple_all$prov)),"prov"] <- "Canada"

names(pubEsts_species_all) <- c("sp","species","prov","zone","year","mean","sd")
pubEsts_species_all$lci = ceiling(pubEsts_species_all$mean-(1.96*pubEsts_species_all$sd))
pubEsts_species_all$uci = ceiling(pubEsts_species_all$mean+(1.96*pubEsts_species_all$sd))
pubEsts_species_all[which(pubEsts_species_all$lci < 0),"lci"] <- 0
pubEsts_species_all[which(is.na(pubEsts_species_all$prov)),"prov"] <- "Canada"


names(pubEsts_age_sex_all) <- c("sp","species","prov","zone","year","age_ratio")
pubEsts_age_sex_all[which(is.na(pubEsts_age_sex_all$prov)),"prov"] <- "Canada"


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

sim_vars <- read.csv("data/website_variable_names_in.csv")
sp_vars <- read.csv("data/website_species_variable_names_in.csv")



load("national_provincial_summaries.RData")



zone_sums_b <- tmp2 %>% 
  group_by(var,prov,zone,year,.draw) %>% 
  summarise(sum = sum(.value)) %>% 
  group_by(var,prov,zone,year) %>% 
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = quantile(sum,0.025,names = FALSE,na.rm = T),
            uci = quantile(sum,0.975,names = FALSE,na.rm = T))




prov_sums_b <- tmp %>% 
  group_by(var,prov,year,.draw) %>% 
  summarise(sum = sum(.value)) %>% 
  group_by(var,prov,year) %>% 
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = quantile(sum,0.025,names = FALSE,na.rm = T),
            uci = quantile(sum,0.975,names = FALSE,na.rm = T))


nat_sums_b <- tmp %>% 
  group_by(var,year,.draw) %>% 
  summarise(sum = sum(.value)) %>% 
  group_by(var,year) %>% 
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = quantile(sum,0.025,names = FALSE,na.rm = T),
            uci = quantile(sum,0.975,names = FALSE,na.rm = T))


zone_sums_a <- tmp_sp %>% 
  group_by(AOU,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>% 
  group_by(AOU,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

prov_sums_a <- tmp_sp %>% 
  group_by(AOU,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>% 
  group_by(AOU,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))


nat_sums_a <- tmp_sp %>% 
  group_by(AOU,year,.draw) %>%
  summarise(sum = sum(.value)) %>% 
  group_by(AOU,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))



save(list = c("nat_sums_a",
              "prov_sums_a",
              "zone_sums_a",
              "nat_sums_b",
              "prov_sums_b",
              "zone_sums_b"),
     file = "data/Posterior_summaries.RData")

nat_sums_a$model <- "new"
nat_sums_a$prov <- "Canada"
prov_sums_a$model <- "new"



# compile published and  new estimates ------------------------------------
pubEsts_species_all$model = "old"
pubEsts_simple_all$model = "old"
pubEsts_age_sex_all$model = "old"

names(pubEsts_species_all) <- c("AOU","species","province","zone","year","mean","sd","lci","uci","model")

provs = unique((provzone[,c("prov","province")]))
provs = data.frame(prov = as.character(provs$prov),
               province = as.character(provs$province),stringsAsFactors = F)

adl <- nrow(provs)+1
provs[adl,"prov"] <- "Canada"
provs[adl,"province"] <- "Canada"


prov_sums_a <- left_join(prov_sums_a,provs,by = "prov")
nat_sums_a <- left_join(nat_sums_a,provs,by = "prov")

sums_a <- bind_rows(nat_sums_a,prov_sums_a)
names(species_web_names) <- c("AOU","species")
sums_a <- left_join(sums_a,species_web_names)



both_a <- bind_rows(sums_a,pubEsts_species_all)

ttt = comp_plot_species(dat = both_a)

pdf(file = "species_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_species(dat = both_a,reg = "Canada",sp = sp_vars[which(sp_vars$source == "duck"),"species"])
pdf(file = "National_duck_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()


ttt = comp_plot_species(dat = both_a,reg = "Canada",sp = sp_vars[which(sp_vars$source == "goose"),"species"])
pdf(file = "National_goose_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()



ttt = comp_plot_species_CV(dat = both_a)

pdf(file = "species_summaries_CV.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_species_CV(dat = both_a,reg = "Canada",sp = sp_vars[which(sp_vars$source == "duck"),"species"])

pdf(file = "national_duck_summaries_CV.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()


ttt = comp_plot_species_CV(dat = both_a,reg = "Canada",sp = sp_vars[which(sp_vars$source == "goose"),"species"])

pdf(file = "national_goose_summaries_CV.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

# compile b tables --------------------------------------------------------


# table output and generation ---------------------------------------------

############# consider if this is necessary. since everything is graphed.
############# figure out which tables should be produced and in what format








