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

names(pubEsts_simple_all) <- c("var","name","province","zone","resid","year","mean","sd","lci","uci","model")
pubEsts_simple_all1 = filter(pubEsts_simple_all,is.na(zone))
vrs = unique(pubEsts_simple_all1[,c("var","name")])

zone_sums_b <- left_join(zone_sums_b,vrs,by = "var")
sums_b <- left_join(sums_b,vrs,by = "var")
both_b <- bind_rows(sums_b,pubEsts_simple_all1)
zone_both_b <- bind_rows(zone_sums_b,pubEsts_simple_all[which(!is.na(pubEsts_simple_all$zone)),])

# 
# 
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


both_b$name <- gsub(both_b$name,pattern = " (total)",
                    replacement = "", fixed = TRUE)

both_b$name <- gsub(both_b$name,pattern = "ucks",
                    replacement = "uck", fixed = TRUE)

both_b$name <- gsub(both_b$name,pattern = "eese",
                    replacement = "oose", fixed = TRUE)


###########################################################
# plotting ----------------------------------------------------------------
load("data/allkill.RData")
allkill <- allkill[which(allkill$PRHUNT %in% provs$prov[1:12]),]



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
print(p1+p4+p2+p3)
dev.off()


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




# Figure 3 - Six example CVs of national species harvest estimates ---------------------------------------

source("Functions/comparison_CV_by_species.R")

p1 = comp_plot_species_CV(dat = both_a,
                 sp = c("Mallard",
                        "American Black Duck",
                        "Common Eider",
                        "Black Scoter",
                 "Lesser Snow Goose: white phase",
                 "Canada Goose"),
                 reg = "Canada",
                 labs_inc = T,
                 lbl_y = c(2000,1989),
                 lab_sp = "American Black Duck")


pdf("Figures/Figure 3.pdf",
    width = 180/25,
    height = 180/25)
print(p1)
dev.off()



# Figure 4 uncommon species including Species of Conservation Concern --------------------------------

# national Harlequin Duck
# national King Eider
# national Common Eider
# national Brant
# Eastern Barrow's Goldeneye
# Newfoundland and Labrador Surf Scoter
source("Functions/comparison_by_species.R")



p1 = comp_plot_species(dat = both_a,
                       sp = c("Harlequin Duck",
                              "Barrow's Goldeneye",
                              "King Eider",
                              "Common Eider",
                              "Brant",
                              "Surf Scoter"),
                       reg = "Canada",
                       labs_inc = T,
                       lbl_y = c(2014,1986),
                       lab_sp = "Harlequin Duck",
                       add_samplesize = FALSE,
                       add_nwings = TRUE,
                       samplesize_scale = 1,
                       nwing_scale = 1,
                       title_base = "Harvest",
                       add_n_labs = FALSE)


pdf("Figures/Figure 4.pdf",
    width = 180/25,
    height = 180/25)
print(p1)
dev.off()




# Figure 5 - age ratio examples -------------------------------------------




p1 = comp_plot_species(dat = both_c,
                       sp = c("Wood Duck",
                              "American Black Duck",
                              "Greater Scaup",
                              "Canvasback",
                              "Canada Goose: small races",
                              "Ross' Goose"),
                       reg = "Canada",
                       labs_inc = T,
                       lbl_y = c(1999,2014),
                       lab_sp = "Ross' Goose",
                       add_samplesize = FALSE,
                       add_nwings = TRUE,
                       samplesize_scale = 1,
                       nwing_scale = 0.1,
                       title_base = "Age Ratio (Immatures/Adults)")


pdf("Figures/Figure 5.pdf",
    width = 180/25,
    height = 180/25)
print(p1)
dev.off()






# Figure 6 Mallard age ratios ---------------------------------------------

p1 = comp_plot_species(dat = both_c,
                       sp = "Mallard",
                       reg = "Ontario",
                       z = NULL,
                       add_samplesize = FALSE,
                       add_nwings = TRUE,
                       samplesize_scale = 0.01,
                       nwing_scale = 0.01,
                       title_base = "",
                       labs_inc = FALSE,
                       lbl_y = c(1990,1995),
                       lab_sp = NULL,
                       unit = "Age Ratio",
                       add_n_labs = TRUE,
                       startYear = NULL,
                       facet_scales = "free",
                       yup = 7.5)

p2 = comp_plot_species(dat = both_c,
                       sp = "Mallard",
                       reg = "Saskatchewan",
                       z = NULL,
                       add_samplesize = FALSE,
                       add_nwings = TRUE,
                       samplesize_scale = 0.01,
                       nwing_scale = 0.01,
                       title_base = "",
                       labs_inc = FALSE,
                       lbl_y = c(1990,1995),
                       lab_sp = NULL,
                       unit = "Age Ratio",
                       add_n_labs = TRUE,
                       startYear = NULL,
                       facet_scales = "free",
                       yup = 7.5)


# p3 = comp_plot_species(dat = both_c,
#                        sp = "Mallard",
#                        reg = "Alberta",
#                        z = NULL,
#                        add_samplesize = FALSE,
#                        add_nwings = TRUE,
#                        samplesize_scale = 0.01,
#                        nwing_scale = 0.01,
#                        title_base = "",
#                        labs_inc = FALSE,
#                        lbl_y = c(1990,1995),
#                        lab_sp = NULL,
#                        unit = "Age Ratio",
#                        add_n_labs = TRUE,
#                        startYear = NULL,
#                        facet_scales = "free",
#                        yup = 7.5)
# 
# p4 = comp_plot_species(dat = both_c,
#                        sp = "Mallard",
#                        reg = "Manitoba",
#                        z = NULL,
#                        add_samplesize = FALSE,
#                        add_nwings = TRUE,
#                        samplesize_scale = 0.01,
#                        nwing_scale = 0.01,
#                        title_base = "",
#                        labs_inc = FALSE,
#                        lbl_y = c(1990,1995),
#                        lab_sp = NULL,
#                        unit = "Age Ratio",
#                        add_n_labs = TRUE,
#                        startYear = NULL,
#                        facet_scales = "free",
#                        yup = 7.5)
p5 = comp_plot_species(dat = both_c,
                       sp = "Mallard",
                       reg = "Canada",
                       z = NULL,
                       add_samplesize = FALSE,
                       add_nwings = TRUE,
                       samplesize_scale = 0.01,
                       nwing_scale = 0.01,
                       title_base = "",
                       labs_inc = TRUE,
                       lbl_y = c(1997,1977),
                       lab_sp = NULL,
                       unit = "Age Ratio",
                       add_n_labs = TRUE,
                       startYear = NULL,
                       facet_scales = "free")

source("functions/parts_by_harvest_means.R")
p6 = parts_by_harvest(sp = "Mallard",reg = c("Saskatchewan","Ontario"))






pdf("Figures/Figure 6.pdf",
    width = 180/25,
    height = 180/25)
print((p1[[1]]+p2[[1]])/
        (p5[[1]]+p6))

dev.off()




# Figure 7 - age and sex specific harvests --------------------------------
source("Functions/Demographics_plot.R")

nat_sums_ag$province = "Canada"
plot_ag <- left_join(nat_sums_ag,sp_vars,by = c("AOU" = "sp"))
sp_plots = sp_vars[which(sp_vars$source == "goose"),"species"]
sp_plots = sp_plots[-which(grepl(sp_plots,pattern = "Brant"))]
tmp = demogr_plot(plot_ag,
                    sp = sp_plots,
                  add_nwings = TRUE,
                  nwing_scale = 0.02,
                  labs_inc = TRUE,
                  lab_sp = "Canada Goose")


pdf("Figures/Figure 7.pdf",
    width = 180/25,
    height = 180/25)
print(tmp)

dev.off()




nat_sums_asxy$province = "Canada"
plot_asxy <- left_join(nat_sums_asxy,sp_vars,by = c("AOU" = "sp"))
sp_plots = sp_vars[which(sp_vars$group == "puddle_ducks"),"species"]
sp_plots = sp_plots[c(1,2,10,11)]

tmp = demogr_plot(plot_asxy,
                  reg = "Canada",
                  sp = sp_plots,
                  age = FALSE,
                  sex = FALSE,
                  lbl_y = c(1982,1985,1998,2006),
                  both = TRUE,
                  add_nwings = TRUE,
                  nwing_scale = 0.02,
                  labs_inc = TRUE,
                  lab_sp = "American Black Duck")


pdf("Figures/Figure 7alt.pdf",
    width = 180/25,
    height = 180/25)
print(tmp)

dev.off()



sp_plots = sp_vars[which(sp_vars$group == "sea_ducks"),"species"]
sp_plots = sp_plots[c(4,6,1,2)]

tmp = demogr_plot(plot_asxy,
                  reg = "Canada",
                  sp = sp_plots,
                  age = FALSE,
                  sex = FALSE,
                  lbl_y = c(2005,1998,1985,1988),
                  both = TRUE,
                  add_nwings = TRUE,
                  nwing_scale = 0.1,
                  labs_inc = TRUE,
                  lab_sp = "Common Goldeneye")


pdf("Figures/Figure 7alt2.pdf",
    width = 180/25,
    height = 180/25)
print(tmp)

dev.off()



nat_sums_sx$province = "Canada"
nat_sums_sx <- left_join(nat_sums_sx,sp_vars,by = c("AOU" = "sp"))
sp_plots = sp_vars[which(sp_vars$group == "puddle_ducks"),"species"]
sp_plots = sp_plots[c(1,2,10,11)]

tmp = demogr_plot(nat_sums_sx,
                  reg = "Canada",
                  sp = sp_plots,
                  age = FALSE,
                  sex = TRUE,
                  lbl_y = c(1982,1985,1998,2006),
                  both = FALSE,
                  add_nwings = TRUE,
                  nwing_scale = 0.01,
                  labs_inc = TRUE,
                  lab_sp = sp_plots[1])


pdf("Figures/Figure 7alt3.pdf",
    width = 180/25,
    height = 180/25)
print(tmp)

dev.off()




sp_plots = sp_vars[which(sp_vars$group == "puddle_ducks"),"species"]
sp_plots = sp_plots[c(1,2,10,11)]
tmp = demogr_plot(plot_ag,
                  reg = "Canada",
                  sp = sp_plots,
                  age = TRUE,
                  sex = FALSE,
                  lbl_y = c(1982,1998),
                  both = FALSE,
                  add_nwings = TRUE,
                  nwing_scale = 0.01,
                  labs_inc = TRUE,
                  lab_sp = sp_plots[2])


pdf("Figures/Figure 7alt4.pdf",
    width = 180/25,
    height = 180/25)
print(tmp)

dev.off()

# 
# # FIgure X Murres not used ---------------------------------------------------------------
# 
# 
# 
# p1 = comp_plot_species(dat = both_a,
#                        sp = c("Thick-billed Murre",
#                               "Common Murre"),
#                        reg = "Canada",
#                        labs_inc = T,
#                        lbl_y = c(2015,2017),
#                        lab_sp = "Common Murre",
#                        add_samplesize = FALSE,
#                        add_nwings = TRUE,
#                        samplesize_scale = 1,
#                        nwing_scale = 0.05,
#                        title_base = "Harvest",
#                        add_n_labs = TRUE,
#                        startYear = 2014,
#                        facet_scales = "fixed")
# p2 = plot_sel_general(dat = both_b,
#                       g = "MURRK",
#                       p = "Canada",
#                       z = NULL,
#                       # labs_inc = TRUE,
#                       # lbl_y = c(1982,1995),
#                       spgp = "murre",
#                       startYear = 2014)
# ### explore the species proportions by period in 2015 adn 2016. 
# ### in these years, the Common Murre parts were less common in periods 4,5,6
# ### how much of hte harvest occurred during those periods in those years?
# 
# 
# ## add 4th plot to show the species proportions
# # p3 = ???
# 
# 
# # Figure Not Used - caste level estimates  -------------------------------------------
# ## not included because the relative precision of estimates from the two models doesn't vary much by caste
# 
# harsum = read.csv("data/harsum76_18.csv")
# load("kill_caste_summary.RData")
# 
# 
# harsum$variance = harsum$var
# harsum$var = paste0(harsum$species,"_Caste")
# harsum$mean = harsum$harvest
# harsum$lci = harsum$harvest-(1.96*harsum$se)
# harsum$lci[which(harsum$lci < 0)] <- 0
# harsum$uci = harsum$harvest+(1.96*harsum$se)
# harsum2 <- harsum %>% 
#   select(var,prov,zone,year,caste,mean,lci,uci) %>% 
#   filter(var %in% unique(zone_kill_caste$var),
#          caste %in% c("D","B","A","E"),
#          year > 1975)
# #above filter drops murrk data because variable that should be MURRK_Caste in zone_kill_caste is called TOMURK_Caste
# #done because Murre harvest info is not the focus of the paper, but should be fixed and explored
# harsum2$model = "old"
# harsum2$caste <- factor(harsum2$caste,
#                                 ordered = T,
#                                 levels = c("D","B","A","E"))
# 
# zone_kill_caste$model = "new"
# zone_kill_caste$caste <- factor(zone_kill_caste$caste,
#                                 ordered = T,
#                                 levels = c("D","B","A","E")) 
# zone_caste <- bind_rows(zone_kill_caste,
#                         harsum2)
# 
# zone_caste <- left_join(zone_caste,provs,by = "prov")
# 
# 
# source("Functions/selected_zone_caste_plot_function.R")
# 
# pdf("output/duck_caste_estimates.pdf")
# for(prv in provs$prov[1:12]){
#   
# p1 <- vector(mode = "list",length = 4)
# names(p1) <- c("D","B","A","E")
# 
# for(cc in c("D","B","A","E")){
# try(p1[[cc]] <- plot_sel_sp_caste(dat = zone_caste,
#                        v = "TODUK_Caste",
#                        p = prv,
#                        z = 1,
#                        c = cc,
#                        spgp = "duck"))
#   
# 
# }
# 
# print((p1[[1]]+p1[[2]])/(p1[[3]]+p1[[4]]))
# rm(list = "p1")
# }
# dev.off()
# 
# 
# 
# pdf("output/goose_caste_estimates.pdf")
# for(prv in provs$prov[1:12]){
#   
#   p1 <- vector(mode = "list",length = 4)
#   names(p1) <- c("D","B","A","E")
#   
#   for(cc in c("D","B","A","E")){
#     try(p1[[cc]] <- plot_sel_sp_caste(dat = zone_caste,
#                                       v = "TOGOK_Caste",
#                                       p = prv,
#                                       z = 1,
#                                       c = cc,
#                                       spgp = "goose"))
#     
#     
#   }
#   
#   print((p1[[1]]+p1[[2]])/(p1[[3]]+p1[[4]]))
#   rm(list = "p1")
# }
# dev.off()
# 
# 
# 
# # 
# # pdf("Figures/Figure 5.pdf",
# #     width = 180/25,
# #     height = 180/25)
# # print(p1)
# # dev.off()
# # 
# 



