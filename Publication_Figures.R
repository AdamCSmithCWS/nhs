# publication figures


library(tidyverse)
library(ggmcmc)
library(ggrepel)
library(ggforce)
library(patchwork)

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



sim_vars <- read.csv("data/website_variable_names_in.csv")
sp_vars <- read.csv("data/website_species_variable_names_in.csv")




load("data/Posterior_summaries.RData")

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

# B tables ----------------------------------------------------------------




nat_sums_b$model <- "new"
nat_sums_b$prov <- "Canada"
prov_sums_b$model <- "new"
zone_sums_b$model <- "new"

prov_sums_b <- left_join(prov_sums_b,provs,by = "prov")
nat_sums_b <- left_join(nat_sums_b,provs,by = "prov")
zone_sums_b <- left_join(zone_sums_b,provs,by = "prov")

sums_b <- bind_rows(nat_sums_b,prov_sums_b)
zone_sums_b <- left_join(zone_sums_b,species_web_names)

names(pubEsts_simple_all) <- c("var","name","province","zone","resid","year","mean","sd","lci","uci","model")
pubEsts_simple_all1 = filter(pubEsts_simple_all,is.na(zone))
vrs = unique(pubEsts_simple_all1[,c("var","name")])

sums_b <- left_join(sums_b,vrs,by = "var")
both_b <- bind_rows(sums_b,pubEsts_simple_all1)
zone_both_b <- bind_rows(zone_sums_b,pubEsts_simple_all[which(!is.na(pubEsts_simple_all$zone)),])



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

sums_c <- filter(sums_c,BAGE == "I") #just the immature summaries to replicate the age ratios on the website
zone_sums_c <- filter(zone_sums_c,BAGE == "I")
zone_sums_c2 <- mutate(zone_sums_c,
                       mean = mean/(1-mean),
                       median = median/(1-median),
                       lci = lci/(1-lci),
                       uci = uci/(1-uci),
                       .keep = "all")


both_c <- bind_rows(sums_c,pubEsts_age_sex_all[which(is.na(pubEsts_age_sex_all$zone)),])
zone_both_c <- bind_rows(zone_sums_c2,pubEsts_age_sex_all[which(!is.na(pubEsts_age_sex_all$zone)),])

### not totally sure why there are na values in the species columns...
both_c <- both_c[which(!is.na(both_c$species)),]
zone_both_c <- zone_both_c[which(!is.na(zone_both_c$species)),]




###########################################################
# plotting ----------------------------------------------------------------
load("data/allkill.RData")
allkill <- allkill[which(allkill$PRHUNT %in% provs$prov[1:10]),]
# Figure 1 - Four example general harvest estimates ---------------------------------------

# Mallard harvest in SK 3
# CAGO small harvest in MB 1
# BSCO in NF 2
# NOPI in SK 3 -changing precision and sample sizes

source("Functions/selected_general_plot_function.R")

p1 = plot_sel_general(dat = both_b,
                      g = "TODUK",
                      p = "Canada",
                      z = NULL,
                      spgp = "duck",
                      labs_inc = TRUE,
                      lbl_y = c(1982,1995))
p2 = plot_sel_general(dat = both_b,
                      g = "TOGOK",
                      p = "Canada",
                      z = NULL,
                      spgp = "goose")
p3 = plot_sel_general(dat = both_b,
                      g = "SUTOGO",
                      p = "Canada",
                      z = NULL,
                      spgp = "goose")
p4 = plot_sel_general(dat = both_b,
                      g = "SUTODU",
                      p = "Canada",
                      z = NULL,
                      spgp = "duck")

pdf("Figures/Figure 1.pdf",
    width = 180/25,
    height = 180/25)
print(p1+p2+p3+p4)
dev.off()


p2 = plot_sel_general(dat = zone_both_b,
                      g = "TOGOK",
                      p = "ON",
                      z = 2,
                      spgp = "goose")
# Figure 2 - Four example species harvest estimates ---------------------------------------

# Mallard harvest in SK 3
# CAGO small harvest in MB 1
# BSCO in NF 2
# NOPI in SK 3 -changing precision and sample sizes

source("Functions/selected_zone_plot_function.R")

p1 = plot_sel_sp(dat = zone_both_a,
                             sp = "Mallard",
                             p = "SK",
                             z = 3,
                             spgp = "duck")
p2 = plot_sel_sp(dat = zone_both_a,
                 sp = "Canada Goose: small races",
                 p = "MB",
                 z = 1,
                 spgp = "goose")
p3 = plot_sel_sp(dat = zone_both_a,
                 sp = "Black Scoter",
                 p = "NF",
                 z = 2,
                 spgp = "duck",
                 labs_inc = TRUE,
                 lbl_y = c(2010,1995))
p4 = plot_sel_sp(dat = zone_both_a,
                 sp = "Northern Pintail",
                 p = "SK",
                 z = 3,
                 spgp = "duck")

pdf("Figures/Figure 2.pdf",
    width = 180/25,
    height = 180/25)
print(p1+p2+p3+p4)
dev.off()
