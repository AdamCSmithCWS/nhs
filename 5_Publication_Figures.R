# publication figures


library(tidyverse)
library(ggmcmc)
library(ggrepel)
library(ggforce)
library(patchwork)
library(sf)
library(RColorBrewer)
library(ggthemes)
library(ggspatial)


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






# load all output from species models and other models --------------------------

#load(paste0("data/parts and harvest survey info",Y,".RData"))

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

sp_case <- sp_vars[,c("species","species_lower_case",
                      "species_sentence_case")]
### load saved results from 
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
names(species_web_names)[1] <- c("AOU")
sums_a <- left_join(sums_a,species_web_names)
zone_sums_a <- left_join(zone_sums_a,species_web_names)


both_a <- bind_rows(sums_a,pubEsts_species_all[which(is.na(pubEsts_species_all$zone)),])
zone_both_a <- bind_rows(zone_sums_a,pubEsts_species_all[which(!is.na(pubEsts_species_all$zone)),])

### not totally sure why there are na values in the species columns...
both_a <- both_a[which(!is.na(both_a$species)),]
zone_both_a <- zone_both_a[which(!is.na(zone_both_a$species)),]
### adding the lower-case versions of species names for JWM..sigh...
both_a <- left_join(both_a,sp_case,by = "species")


zone_both_a <- left_join(zone_both_a,sp_case,by = "species")



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
names(species_web_names)[1] <- c("AOU")
sums_c <- left_join(sums_c,species_web_names)
zone_sums_c <- left_join(zone_sums_c,species_web_names)



both_c <- bind_rows(sums_c,pubEsts_age_sex_all[which(is.na(pubEsts_age_sex_all$zone)),])
zone_both_c <- bind_rows(zone_sums_c,pubEsts_age_sex_all[which(!is.na(pubEsts_age_sex_all$zone)),])

### not totally sure why there are na values in the species columns...
both_c <- both_c[which(!is.na(both_c$species)),]
zone_both_c <- zone_both_c[which(!is.na(zone_both_c$species)),]

### adding the lower-case versions of species names for JWM..sigh...
both_c <- left_join(both_c,sp_case,by = "species")


zone_both_c <- left_join(zone_both_c,sp_case,by = "species")


both_b$name <- gsub(both_b$name,pattern = " (total)",
                    replacement = "", fixed = TRUE)

both_b$name <- gsub(both_b$name,pattern = "ucks",
                    replacement = "uck", fixed = TRUE)

both_b$name <- gsub(both_b$name,pattern = "eese",
                    replacement = "oose", fixed = TRUE)



## raw species parts data no permit or location information
outscse <- read.csv("data/outscse_spec_comp_survey_data.csv")




allkill <- read.csv("data/allkill.csv") ## raw data summary of survey responses for plotting





###########################################################
# plotting ----------------------------------------------------------------

species_web_names <- left_join(species_web_names,sp_case,by = "species")



# Figure 1 Map of regions -------------------------------------------------



## load map
base_map = st_read(dsn = "input_map",
                   layer = "Harvest_Survey_Zones_2017")

prov_swap = function(x){
  if(x %in% c("NF","PQ")){
    if(x == "NF"){x2 <- "NL"}
    if(x == "PQ"){x2 <- "QC"}
  }else{
    x2 <- x
  }
  
  return(x2)
}
base_map2 = base_map %>% group_by(Zonename) %>% 
  mutate(prov2 = prov_swap(PROV),
         labl = paste(prov2,ZONE,sep = ""))
#plot(base_map)
labs = st_coordinates(st_centroid(base_map2))

labls = data.frame(x = labs[,"X"],
                   y = labs[,"Y"],
                   labl = base_map2$labl,
                   col = base_map2$labl)


fg1 = ggplot()+
  geom_sf(data = base_map2,alpha = 1,size = 0.1,fill = grey(0.95))+ #aes(fill = labl)
  geom_label_repel(data = labls,aes(x = x, y = y,label = labl),#,colour = labl),, vjust = "inward"
                   fill = alpha(c("white"),0.8),
                   # nudge_y = -1000,
                   # nudge_x = -1000,
                   
                   min.segment.length = 0.1,
                   segment.size = 0.2,
                   segment.color = grey(0.2),
                   fontface = "bold",
                   family = "Times",
                   size = 2,
                   label.size = 0.01,
                   label.padding = 0.13)+
  # scale_colour_viridis_d(aesthetics = c("fill","colour"),
  #                        begin = 0.1,end = 0.9)+
  xlab("")+
  ylab("")+
  theme_light()+
    theme(text = element_text(family = "Times"),
          axis.title=element_blank(), axis.text=element_text(size = 5), axis.ticks=element_blank(),
          legend.position = "none")+
  scale_x_continuous(breaks = c(-120,-100,-80,-60))+
  annotation_north_arrow(aes(which_north = "true",location = "bl"),
                         style = north_arrow_minimal(),
                         height = unit(0.75,"cm"),
                         width = unit(0.35,"cm"))+
  annotation_scale(style = "tick",location = "tr")

print(fg1)


pdf("Figures/Figure 1.pdf",
    width = 85/25,
    height = 80/25)
print(fg1)
dev.off()








# Figure 2 - Four example general harvest estimates ---------------------------------------

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
                      spgp = "goose",
                      xlb = "Year")
p3 = plot_sel_general(dat = both_b,
                      g = "SUTOGO",
                      p = "Canada",
                      z = NULL,
                      spgp = "goose",
                      xlb = "Year")
p4 = plot_sel_general(dat = both_b,
                      g = "SUTODU",
                      p = "Canada",
                      z = NULL,
                      spgp = "duck")

pdf("Figures/Figure 2.pdf",
    width = 180/25,
    height = 180/25)
print(p1+p4+p2+p3)
dev.off()

# pdf("Figures/Supplemental_general_estimates.pdf")
# for(j in unique(both_b$var)){
#   spgpt <- sim_vars[which(sim_vars$var == j),"source"]
#   if(spgpt == "other"){next}
#   tmp = plot_sel_general(dat = both_b,
#                         g = j,
#                         p = "Canada",
#                         z = NULL,
#                         spgp = spgpt)
#   print(tmp)
# }
# dev.off()

# Figure 3 - All duck harvest estimate comparison 2009 - 2018 ---------------------------------------
# calculations of %diff in harvest between models -------------------------
p_dif = function(x,y){
  y[which(y == 0)] <- NA
  d = mean(log(x/y))
  return(d)
}


harv_comp <- both_a %>% 
  filter(province == "Canada",
         is.na(zone),
         year > 2008,
         species_sentence_case != "Eurasian green-winged teal",
         (AOU > 1000 & AOU < 1690)) 

spmeans = harv_comp %>% filter(model == "old") %>% 
  group_by(species_sentence_case) %>% 
  summarise(mean_harv = mean(mean,na.rm = TRUE)) %>% 
  left_join(.,sp_vars,by = "species_sentence_case") %>% 
  arrange(.,source,mean_harv) %>% 
  mutate(sp_f = factor(species_sentence_case,levels = species_sentence_case,ordered = TRUE)) %>% 
  select(species_sentence_case,mean_harv,sp_f,source)


# both_a[which(both_a$province == "Canada" & is.na(both_a$zone) & both_a$year > 2010 &
#                       both_a$species_sentence_case != "Eurasian green-winged teal" 
#                       & (both_a$AOU > 1000 & both_a$AOU < 1690)
#                          ),]

diff_harv = harv_comp %>% ungroup() %>% 
  select(species_sentence_case,model,year,mean) %>% 
  pivot_wider(.,names_from = model,
              values_from = mean)  %>%
  group_by(species_sentence_case,year) %>% 
  mutate(mean_diff = p_dif(new,old)) %>% 
  group_by(species_sentence_case) %>% 
  summarise(mean = mean(mean_diff,na.rm = TRUE),
            lqrt = quantile(mean_diff,0.0,na.rm = TRUE),
            uqrt = quantile(mean_diff,1,na.rm = TRUE),
            nn = length(which(!is.na(mean_diff)))) %>% 
  left_join(.,spmeans,by = "species_sentence_case") %>% 
  arrange(sp_f)



ch_lab_loc = log(c(0.5,0.75,1,1.33,2,4))
ch_labs = paste(round((exp(ch_lab_loc)-1)*100),"%")
ch_labs[which(ch_labs == "0 %")] <- ""

diff_plot = ggplot(data = diff_harv,aes(x = sp_f,y = mean))+
  geom_point()+
  geom_errorbar(aes(ymin = lqrt,ymax = uqrt,width = 0),alpha = 0.3)+
  geom_abline(slope = 0,intercept = 0)+
  scale_y_continuous(breaks = ch_lab_loc,labels = ch_labs)+
 # scale_colour_viridis_c(aesthetics = "colour",end = 0.8,direction = -1)+
  ylab("")+
  xlab("")+
  coord_flip(ylim = c(ch_lab_loc[1],ch_lab_loc[length(ch_lab_loc)]))+
  theme_classic()+
  theme(text = element_text(family = "Times"),
        legend.position = "none",
        axis.text = element_text(size = 8))

print(diff_plot)



pdf("Figures/Figure 3.pdf",
    width = 85/25,
    height = 180/25)
print(diff_plot)

dev.off()



# Figure 4 - Four example species harvest estimates -----------------------



# Mallard harvest in SK 3
# CAGO small harvest in MB 1
# BSCO in NF 2
# NOPI in SK 3 -changing precision and sample sizes

source("Functions/selected_zone_plot_function.R")

p1 = plot_sel_sp(dat = zone_both_a,
                             sp = "Mallard",
                             p = "SK",
                             z = 3,
                             spgp = "duck",
                 ylb = "Harvest")
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
                 lbl_y = c(2010,1995),
                 xlb = "Year",
                 ylb = "Harvest")
p4 = plot_sel_sp(dat = zone_both_a,
                 sp = "Northern Pintail",
                 p = "SK",
                 z = 3,
                 spgp = "duck",
                 xlb = "Year")

pdf("Figures/Figure 4.pdf",
    width = 180/25,
    height = 180/25)
print(p1+p2+p3+p4)
dev.off()

# figure for readme
png("Figures/Figure 4.png",
    width = 480*2,
    height = 480*2,
    res = 150)
print(p1+p2+p3+p4)
dev.off()


# Figure 6 - Six example CVs of national species harvest estimates ---------------------------------------

source("Functions/comparison_CV_by_species.R")

p1 = comp_plot_species_CV(dat = both_a,
                 sp = c("Mallard",
                        "American Black Duck",
                        "Common Eider",
                        "Black Scoter",
                 "Greater Snow Goose",
                 "Canada Goose"),
                 reg = "Canada",
                 labs_inc = T,
                 lbl_y = c(2001,1986),
                 add_nwings = TRUE,
                 nwing_scale = 0.005,
                 lab_sp = "Black Scoter",
                 xlb = "Year")


pdf("Figures/Figure 6.pdf",
    width = 180/25,
    height = 180/25)
print(p1)
dev.off()



# Figure 5 conservation concern species --------------------------------

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
                       nwing_scale = 0.1,
                       unit = "Harvest",
                       add_n_labs = FALSE)


pdf("Figures/Figure 5.pdf",
    width = 180/25,
    height = 180/25)
print(p1)
dev.off()




# Figure 7 - age ratio examples -------------------------------------------




p1 = comp_plot_species(dat = both_c,
                       sp = c("Wood Duck",
                              "American Black Duck",
                              "Greater Scaup",
                              "Canvasback",
                              "Canada Goose: small",
                              "Ross' Goose"),
                       reg = "Canada",
                       labs_inc = T,
                       lbl_y = c(1983,2010),
                       lab_sp = "Greater Scaup",
                       add_samplesize = FALSE,
                       add_nwings = TRUE,
                       samplesize_scale = 1,
                       nwing_scale = 0.02,
                       unit = "Age ratio (immatures/adults)",
                       #title_base = "Age ratio (immatures/adults)",
                       add_n_labs = FALSE)


pdf("Figures/Figure 7.pdf",
    width = 180/25,
    height = 180/25)
print(p1)
dev.off()


 



# Figure 8 Mallard age ratios ---------------------------------------------

p1 = comp_plot_species(dat = both_c,
                       sp = "Mallard",
                       reg = "Ontario",
                       z = NULL,
                       add_samplesize = FALSE,
                       add_nwings = TRUE,
                       samplesize_scale = 0.01,
                       nwing_scale = 0.005,
                       title_base = "Ontario",
                       labs_inc = FALSE,
                       lbl_y = c(1990,1995),
                       lab_sp = NULL,
                       unit = "Age ratio (immatures/adults)",
                       add_n_labs = FALSE,
                       startYear = NULL,
                       facet_scales = "free",
                       yup = 7.5,
                       xlb = "")

p2 = comp_plot_species(dat = both_c,
                       sp = "Mallard",
                       reg = "Saskatchewan",
                       z = NULL,
                       add_samplesize = FALSE,
                       add_nwings = TRUE,
                       nwing_scale = 0.005,
                       title_base = "Saskatchewan",
                       labs_inc = FALSE,
                       lbl_y = c(1990,1995),
                       lab_sp = NULL,
                       unit = "Age ratio (immatures/adults)",
                       add_n_labs = FALSE,
                       startYear = NULL,
                       facet_scales = "free",
                       yup = 7.5,
                       xlb = "Year")


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
                       nwing_scale = 0.005,
                       title_base = "Canada",
                       labs_inc = TRUE,
                       lbl_y = c(1997,1977),
                       lab_sp = NULL,
                       unit = "Age ratio (immatures/adults)",
                       add_n_labs = FALSE,
                       startYear = NULL,
                       facet_scales = "free",
                       xlb = "Year")

source("functions/parts_by_harvest_means.R")
p6 = parts_by_harvest(sp = "Mallard",reg = c("Saskatchewan","Ontario"))






pdf("Figures/Figure 8.pdf",
    width = 180/25,
    height = 180/25)
print((p1[[1]]+p2[[1]])/
        (p5[[1]]+p6))

dev.off()





# 
# diff_plot_p = ggplot(data = diff_harv_p,aes(x = species_sentence_case,y = mean))+
#   geom_point()+
#   geom_errorbar(aes(ymin = lqrt,ymax = uqrt,width = 0),alpha = 0.3)+
#   facet_wrap(~province,scales = "free_x")+
#   coord_flip()
# 
# print(diff_plot_p)
# 
# 
# 
# 
# harv_comp_p <- both_a[which(both_a$province != "Canada" & is.na(both_a$zone) & both_a$year > 2010 &
#                               both_a$species_sentence_case != "Eurasian green-winged teal"  
#                             & (both_a$AOU > 1000 & both_a$AOU < 1690)
# ),]
# 
# 
# 
#  
# diff_harv_p = harv_comp_p %>% select(species_sentence_case,AOU,model,year,mean,province) %>% 
#   pivot_wider(.,names_from = model,
#               values_from = mean)  %>%
#   group_by(species_sentence_case,AOU,province,year) %>% 
#   mutate(mean_diff = p_dif(new,old))  %>% 
#   group_by(species_sentence_case,) %>% 
#   summarise(mean = mean(mean_diff,na.rm = TRUE),
#             lqrt = quantile(mean_diff,0.05,na.rm = TRUE),
#             uqrt = quantile(mean_diff,0.95,na.rm = TRUE))
# 
# 
# diff_plot_p = ggplot(data = diff_harv_p,aes(x = species_sentence_case,y = mean))+
#   geom_point()+
#   geom_errorbar(aes(ymin = lqrt,ymax = uqrt,width = 0),alpha = 0.3)+
#   #facet_wrap(~province,scales = "free_x")+
#   coord_flip()
# 
# print(diff_plot_p)
# 
# 
# diff_harv_p$mean

# include Ducks, Geese, and Murres
# sort according to groups and then according to total harvest

### make this into a new plot to discuss the redistribution of harvest from common species to less common species
# 
# diff_harv_all = harv_comp %>% ungroup() %>% 
#   select(species_sentence_case,model,year,mean) %>% 
#   pivot_wider(.,names_from = model,
#               values_from = mean)  %>%
#   group_by(species_sentence_case,year) %>% 
#   mutate(diff = p_dif(new,old)) %>% 
#   left_join(.,spmeans,by = "species_sentence_case") %>% 
#   arrange(sp_f)
# 
# 
# 
# diff_plot_alt = ggplot(data = diff_harv_all,aes(x = sp_f,y = diff,colour = source))+
#   geom_point(position = position_jitter(width = 0.2),alpha = 0.2)+
#   #geom_errorbar(aes(ymin = lqrt,ymax = uqrt,width = 0),alpha = 0.3)+
#   geom_abline(slope = 0,intercept = 0)+
#   scale_y_continuous(breaks = ch_lab_loc,labels = ch_labs)+
#   scale_colour_viridis_d(aesthetics = "colour",end = 0.8)+
#   ylab("")+
#   xlab("")+
#   coord_flip(ylim = c(ch_lab_loc[1],ch_lab_loc[length(ch_lab_loc)]))+
#   theme_classic()+
#   theme(text = element_text(family = "Times"),
#         legend.position = "none",
#         axis.text = element_text(size = 8))
# 
# print(diff_plot_alt)
# 
# 
# 
# pdf("Figures/Figure new alt.pdf",
#     width = 85/25,
#     height = 180/25)
# print(diff_plot_alt)
# 
# dev.off()




median(diff_harv$mean)
range(diff_harv$mean)
quantile(diff_harv$mean,c(0.1,0.9))

length(which(abs(diff_harv$mean) < 10))/length(diff_harv$mean)
length(which(abs(diff_harv$mean) < 10))
length(diff_harv$mean)
diff_harv[which(abs(diff_harv$mean) > 10),]

# Calculations of CV compared across years --------------------------------

cv_a2 <- both_a[which(both_a$province == "Canada" & both_a$year > 2009 &
                        both_a$species_sentence_case != "Eurasian green-winged teal"),]
  cv_a2[,"CV"] <- (((cv_a2$uci-cv_a2$lci)/(qnorm(0.975)*2))/cv_a2$mean)*100

w_lci0 = which(cv_a2$lci == 0)
#cv_a2[w_lci0,"CV"] <- (cv_a2[w_lci0,"sd"])/cv_a2[w_lci0,"mean"]


mean_cv = cv_a2 %>% group_by(species_sentence_case,AOU,model) %>% 
  summarise(mean = mean(CV,na.rm = TRUE),
            lqrt = quantile(CV,0.25,na.rm = TRUE),
            uqrt = quantile(CV,0.75,na.rm = TRUE))


cv_comp = ggplot(data = mean_cv)+
  geom_pointrange(aes(x = species_sentence_case,y = mean,ymin = lqrt,ymax = uqrt,colour = model))+
  coord_flip()
print(cv_comp)

### number of species with mean annual CVs < 10% in the last decade 
nsp_LT10 <- mean_cv %>% group_by(species_sentence_case,model) %>% 
  summarise(lt10 = ifelse(mean < 10,TRUE,FALSE) ) %>% 
  group_by(model) %>% 
  summarise(nsp_LT10 = length(which(lt10)))
nsp_LT10


diff_cv = cv_a2 %>% select(species_sentence_case,AOU,model,year,CV) %>% 
  pivot_wider(.,names_from = model,
              values_from = CV)  %>% 
  mutate(CV_diff = new-old) %>% 
  group_by(species_sentence_case,AOU) %>% 
  summarise(mean = mean(CV_diff,na.rm = TRUE),
            lqrt = quantile(CV_diff,0.25,na.rm = TRUE),
            uqrt = quantile(CV_diff,0.75,na.rm = TRUE))

# cv_diff = ggplot(data = diff_cv)+
#   geom_pointrange(aes(x = species_sentence_case,y = median,ymin = lqrt,ymax = uqrt))+
#   coord_flip()
# print(cv_diff)

mean(diff_cv$mean)
range(diff_cv$mean)
length(which(diff_cv$mean < 0))
length(which(diff_cv$mean > 0))


# 
# # Figure Alt - age and sex specific harvests --------------------------------
# source("Functions/Demographics_plot.R")
# 
# nat_sums_ag$province = "Canada"
# plot_ag <- left_join(nat_sums_ag,sp_vars,by = c("AOU" = "sp"))
# sp_plots = sp_vars[which(sp_vars$source == "goose"),"species"]
# sp_plots = sp_plots[-which(grepl(sp_plots,pattern = "Brant"))]
# tmp = demogr_plot(plot_ag,
#                     sp = sp_plots,
#                   add_nwings = TRUE,
#                   nwing_scale = 0.02,
#                   labs_inc = TRUE,
#                   lab_sp = "Canada Goose")
# 
# 
# pdf("Figures/Figure alt1.pdf",
#     width = 180/25,
#     height = 180/25)
# print(tmp)
# 
# dev.off()
# 
# 
# 
# 
# nat_sums_asxy$province = "Canada"
# plot_asxy <- left_join(nat_sums_asxy,sp_vars,by = c("AOU" = "sp"))
# sp_plots = sp_vars[which(sp_vars$group == "puddle_ducks"),"species"]
# sp_plots = sp_plots[c(1,2,10,11)]
# 
# tmp = demogr_plot(plot_asxy,
#                   reg = "Canada",
#                   sp = sp_plots,
#                   age = FALSE,
#                   sex = FALSE,
#                   lbl_y = c(1982,1985,1998,2006),
#                   both = TRUE,
#                   add_nwings = TRUE,
#                   nwing_scale = 0.02,
#                   labs_inc = TRUE,
#                   lab_sp = "American Black Duck")
# 
# 
# pdf("Figures/Figure alt2.pdf",
#     width = 180/25,
#     height = 180/25)
# print(tmp)
# 
# dev.off()
# 
# 
# 
# sp_plots = sp_vars[which(sp_vars$group == "sea_ducks"),"species"]
# sp_plots = sp_plots[c(4,6,1,2)]
# 
# tmp = demogr_plot(plot_asxy,
#                   reg = "Canada",
#                   sp = sp_plots,
#                   age = FALSE,
#                   sex = FALSE,
#                   lbl_y = c(2005,1998,1985,1988),
#                   both = TRUE,
#                   add_nwings = TRUE,
#                   nwing_scale = 0.1,
#                   labs_inc = TRUE,
#                   lab_sp = "Common Goldeneye")
# 
# 
# pdf("Figures/Figure alt3.pdf",
#     width = 180/25,
#     height = 180/25)
# print(tmp)
# 
# dev.off()
# 
# 
# 
# nat_sums_sx$province = "Canada"
# nat_sums_sx <- left_join(nat_sums_sx,sp_vars,by = c("AOU" = "sp"))
# sp_plots = sp_vars[which(sp_vars$group == "puddle_ducks"),"species"]
# sp_plots = sp_plots[c(1,2,10,11)]
# 
# tmp = demogr_plot(nat_sums_sx,
#                   reg = "Canada",
#                   sp = sp_plots,
#                   age = FALSE,
#                   sex = TRUE,
#                   lbl_y = c(1982,1985,1998,2006),
#                   both = FALSE,
#                   add_nwings = TRUE,
#                   nwing_scale = 0.01,
#                   labs_inc = TRUE,
#                   lab_sp = sp_plots[1])
# 
# 
# pdf("Figures/Figure alt4.pdf",
#     width = 180/25,
#     height = 180/25)
# print(tmp)
# 
# dev.off()
# 
# 
# 
# 
# sp_plots = sp_vars[which(sp_vars$group == "puddle_ducks"),"species"]
# sp_plots = sp_plots[c(1,2,10,11)]
# tmp = demogr_plot(plot_ag,
#                   reg = "Canada",
#                   sp = sp_plots,
#                   age = TRUE,
#                   sex = FALSE,
#                   lbl_y = c(1982,1998),
#                   both = FALSE,
#                   add_nwings = TRUE,
#                   nwing_scale = 0.01,
#                   labs_inc = TRUE,
#                   lab_sp = sp_plots[2])
# 
# 
# pdf("Figures/Figure alt5.pdf",
#     width = 180/25,
#     height = 180/25)
# print(tmp)
# 
# dev.off()
# 


