
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


pubEsts_age_sex_all <- pubEsts_age_sex_all[which(pubEsts_age_sex_all$year > 1975),]
pubEsts_species_all <- pubEsts_species_all[which(pubEsts_species_all$year > 1975),]
pubEsts_simple_all <- pubEsts_simple_all[which(pubEsts_simple_all$year > 1975),]



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


sim_vars <- read.csv("data/website_variable_names_in.csv")
sp_vars <- read.csv("data/website_species_variable_names_in.csv")






# plotting ----------------------------------------------------------------

load("data/Posterior_summaries.RData")
source("functions/comparison_simple.R")
source("functions/comparison_by_species.R")
source("functions/comparison_CV_by_species.R")

load("data/allkill.RData")



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



# A tables ----------------------------------------------------------------


nat_sums_a$model <- "new"
nat_sums_a$prov <- "Canada"
prov_sums_a$model <- "new"
zone_sums_a$model <- "new"

prov_sums_a <- left_join(prov_sums_a,provs,by = "prov")
nat_sums_a <- left_join(nat_sums_a,provs,by = "prov")
zone_sums_a <- left_join(zone_sums_a,provs,by = "prov")


sums_a <- bind_rows(nat_sums_a,prov_sums_a)
names(species_web_names) <- c("AOU","species")
sums_a <- left_join(sums_a,species_web_names)
zone_sums_a <- left_join(zone_sums_a,species_web_names)


both_a <- bind_rows(sums_a,pubEsts_species_all[which(is.na(pubEsts_species_all$zone)),])
zone_both_a <- bind_rows(zone_sums_a,pubEsts_species_all[which(!is.na(pubEsts_species_all$zone)),])

### not totally sure why there are na values in the species columns...
both_a <- both_a[which(!is.na(both_a$species)),]
zone_both_a <- zone_both_a[which(!is.na(zone_both_a$species)),]


ttt = comp_plot_species(dat = zone_both_a)

pdf(file = "Figures/species_summaries_by_zone.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()




ttt = comp_plot_species(dat = both_a)

pdf(file = "Figures/species_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_species(dat = both_a,reg = "Canada",sp = sp_vars[which(sp_vars$source == "duck"),"species"])
pdf(file = "Figures/National_duck_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_species(dat = both_a,reg = "Canada",sp = sp_vars[which(sp_vars$group == "diving_ducks"),"species"])
pdf(file = "Figures/National_diving_duck_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_species(dat = both_a,reg = "Canada",
                        sp = sp_vars[which(sp_vars$group == "sea_ducks"),"species"])
pdf(file = "Figures/National_sea_duck_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_species(dat = both_a,reg = "Canada",
                        sp = sp_vars[which(sp_vars$group == "puddle_ducks"),"species"])
pdf(file = "Figures/National_puddle_duck_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()


ttt = comp_plot_species(dat = both_a,reg = "Canada",sp = sp_vars[which(sp_vars$source == "goose"),"species"])
pdf(file = "Figures/National_goose_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()



ttt = comp_plot_species_CV(dat = both_a)

pdf(file = "Figures/species_summaries_CV.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_species_CV(dat = both_a,reg = "Canada",sp = sp_vars[which(sp_vars$source == "duck"),"species"])

pdf(file = "Figures/national_duck_summaries_CV.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()


ttt = comp_plot_species_CV(dat = both_a,reg = "Canada",sp = sp_vars[which(sp_vars$source == "goose"),"species"])

pdf(file = "Figures/national_goose_summaries_CV.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()


# B tables ----------------------------------------------------------------




nat_sums_b$model <- "new"
nat_sums_b$prov <- "Canada"
prov_sums_b$model <- "new"

prov_sums_b <- left_join(prov_sums_b,provs,by = "prov")
nat_sums_b <- left_join(nat_sums_b,provs,by = "prov")

sums_b <- bind_rows(nat_sums_b,prov_sums_b)

names(pubEsts_simple_all) <- c("var","name","province","zone","resid","year","mean","sd","lci","uci","model")
pubEsts_simple_all1 = filter(pubEsts_simple_all,is.na(zone))
vrs = unique(pubEsts_simple_all1[,c("var","name")])

sums_b <- left_join(sums_b,vrs,by = "var")
both_b <- bind_rows(sums_b,pubEsts_simple_all1)


ttt = comp_plot_simple(dat = both_b,
                       var_sel = c("ACTIWF",
                                   "DAYWF",
                                   "SUTODU",
                                   "SUTOGO",
                                   "TODUK",
                                   "TOGOK",
                                   "POTNTL",
                                   "SUTOWF",
                                   "TOWFK"))

pdf(file = "Figures/regional_Waterfowl_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()


ttt = comp_plot_simple(dat = both_b,
                       reg = "Canada",
                       var_sel = c("ACTIWF",
                                   "DAYWF",
                                   "SUTODU",
                                   "SUTOGO",
                                   "TODUK",
                                   "TOGOK",
                                   "POTNTL",
                                   "SUTOWF",
                                   "TOWFK"))

pdf(file = "Figures/national_Waterfowl_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()


##3 drop snipe and rail data from pre-1991
both_b2 <- both_b[-which(both_b$var %in% c("RAILK","SNIPK","SURAIL",
                                           "SUSNIP","DARAIL",
                                           "DASNIP") & 
                           both_b$year < 1992),]
ttt = comp_plot_simple(dat = both_b2,
                       reg = "Canada",
                       var_sel = c("ACTIOT",
                                   "COOTK",
                                   "CRANK",
                                    "DOVEK",
                                   "MURRK",
                                   "PIGEK",
                                   "RAILK",
                                   "SNIPK",
                                   "WOODK",
                                   "TOOTK"))

pdf(file = "Figures/national_NonWaterfowl_harvest_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_simple(dat = both_b2,
                       reg = "Canada",
                       var_sel = c("SUCOOT",
"SUCRAN",
"SUDOVE",
"SUMURR",
"SUPIGE",
"SURAIL",
"SUSNIP",
"SUWOOD",
"SUTOOT"))

pdf(file = "Figures/national_NonWaterfowl_hunter_success_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()




ttt = comp_plot_simple(dat = both_b2,
                       reg = "Canada",
                       var_sel = c("DACOOT",
                                   "DACRAN",
                                   "DADOVE",
                                   "DAMURR",
                                   "DAPIGE",
                                   "DARAIL",
                                   "DASNIP",
                                   "DAWOOD",
                                   "DAYOT"))

pdf(file = "Figures/national_NonWaterfowl_days_summaries.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()





# C tables ----------------------------------------------------------------

names(pubEsts_age_sex_all) <- c("AOU","species","province","zone","year","mean","model")

nat_sums_c$model <- "new"
nat_sums_c$prov <- "Canada"
prov_sums_c$model <- "new"
zone_sums_c$model <- "new"

prov_sums_c <- left_join(prov_sums_c,provs,by = "prov")
nat_sums_c <- left_join(nat_sums_c,provs,by = "prov")
zone_sums_c <- left_join(zone_sums_c,provs,by = "prov")

sums_c <- bind_rows(nat_sums_c,prov_sums_c)
names(species_web_names) <- c("AOU","species")
sums_c <- left_join(sums_c,species_web_names)
zone_sums_c <- left_join(zone_sums_c,species_web_names)



both_c <- bind_rows(sums_c,pubEsts_age_sex_all[which(is.na(pubEsts_age_sex_all$zone)),])
zone_both_c <- bind_rows(zone_sums_c,pubEsts_age_sex_all[which(!is.na(pubEsts_age_sex_all$zone)),])

### not totally sure why there are na values in the species columns...
both_c <- both_c[which(!is.na(both_c$species)),]
zone_both_c <- zone_both_c[which(!is.na(zone_both_c$species)),]


ttt = comp_plot_species(dat = zone_both_c)

pdf(file = "Figures/species_age_ratios_by_zone.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()




ttt = comp_plot_species(dat = both_c)

pdf(file = "Figures/species_age_ratios.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_species(dat = both_c,reg = "Canada",sp = sp_vars[which(sp_vars$source == "duck"),"species"])
pdf(file = "Figures/National_duck_age_ratios.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()





ttt = comp_plot_species(dat = both_c,reg = "Canada",sp = sp_vars[which(sp_vars$group == "diving_ducks"),"species"],
                        add_samplesize = FALSE,
                        add_nwings = TRUE,
                        nwing_scale = 0.1)


pdf(file = "Figures/National_diving_duck_age_ratios.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_species(dat = both_c,reg = "Canada",
                        sp = sp_vars[which(sp_vars$group == "sea_ducks"),"species"],
                        add_samplesize = FALSE,
                        add_nwings = TRUE,
                        nwing_scale = 0.1)
pdf(file = "Figures/National_sea_duck_age_ratios.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()

ttt = comp_plot_species(dat = both_c,reg = "Canada",
                        sp = sp_vars[which(sp_vars$group == "puddle_ducks"),"species"],
                        add_samplesize = FALSE,
                        add_nwings = TRUE,
                        nwing_scale = 0.1)
pdf(file = "Figures/National_puddle_duck_age_ratios.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()




ttt = comp_plot_species(dat = both_c,reg = "Canada",sp = sp_vars[which(sp_vars$source == "goose"),"species"])
pdf(file = "Figures/National_goose_age_ratios.pdf",width = 8.5,height = 11)
for(pp in 1:length(ttt)){
  print(ttt[[pp]])
}
dev.off()






# C alternate tables - sex ratio ----------------------------------------------------------------


nat_sums_c2$model <- "new"
nat_sums_c2$prov <- "Canada"
prov_sums_c2$model <- "new"
zone_sums_c2$model <- "new"

prov_sums_c2 <- left_join(prov_sums_c2,provs,by = "prov")
nat_sums_c2 <- left_join(nat_sums_c2,provs,by = "prov")
zone_sums_c2 <- left_join(zone_sums_c2,provs,by = "prov")

sums_c2 <- bind_rows(nat_sums_c2,prov_sums_c2)
names(species_web_names) <- c("AOU","species")
sums_c2 <- left_join(sums_c2,species_web_names)
zone_sums_c2 <- left_join(zone_sums_c2,species_web_names)



both_c <- bind_rows(sums_c,pubEsts_age_sex_all[which(is.na(pubEsts_age_sex_all$zone)),])
zone_both_c <- bind_rows(zone_sums_c,pubEsts_age_sex_all[which(!is.na(pubEsts_age_sex_all$zone)),])

### not totally sure why there are na values in the species columns...
both_c <- both_c[which(!is.na(both_c$species)),]
zone_both_c <- zone_both_c[which(!is.na(zone_both_c$species)),]




# table output and generation ---------------------------------------------

############# consider if this is necessary. since everything is graphed.
############# figure out which tables should be produced and in what format








