### 

# possible demog_groups = c("Adult","Immature","Female","Male",
# "Adult-Female","Adult-Male",
# "Immature-Female","Immature-Male")


demogr_plot <- function(dat = plot_ag,
                              sp = NULL,
                              reg = "Canada",
                              z = NULL,
                              add_nwings = FALSE,
                              nwing_scale = 1,
                              title_base = "Harvest",
                              labs_inc = FALSE,
                              lbl_y = c(1990,1995,2000,2005),
                              lab_sp = NULL,
                              unit = NULL,
                              add_n_labs = TRUE,
                              startYear = NULL,
                              facet_scales = "free",
                              yup = NA,
                        age = TRUE,
                        sex = FALSE,
                        both = FALSE){
  
  
  
  demog_groups = data.frame(age = c(rep(TRUE,2),rep(FALSE,6)),
                           sex = c(rep(FALSE,2),rep(TRUE,2),
                                   rep(FALSE,4)),
                           both = c(rep(FALSE,4),rep(TRUE,4)),
                           BAGE = c("A","I",
                                    NA,NA,
                                    "A","A",
                                    "I","I"),
                           BSEX = c(NA,NA,
                                    "F","M",
                                    "F","M",
                                    "F","M"),
                           demog_group = c("Adult","Immature",
                                           "Female","Male",
                                           "Adult-Female","Adult-Male",
                                           "Immature-Female","Immature-Male"))
  if(age){
    demog_groups <- demog_groups %>% filter(age == TRUE) %>% select(BAGE,demog_group)
    dat <- left_join(dat,demog_groups,by = "BAGE")
  }
  
  if(sex){
    demog_groups <- demog_groups %>% filter(sex == TRUE) %>% select(BSEX,demog_group)
    dat <- left_join(dat,demog_groups,by = "BSEX")
  }
  
  if(both){
    demog_groups <- demog_groups %>% filter(both == TRUE) %>% select(BAGE,BSEX,demog_group)
  dat <- left_join(dat,demog_groups,by = c("BAGE","BSEX"))
}

  
  if(is.null(unit)){unit <- title_base}
  if(!is.null(reg)){
  reg2 <- provs[which(provs$province == reg),"prov"]

  
  
  # add n wings -------------------------------------------------------------
  
  if(add_nwings){
    if(reg == "Canada"){
      nws <- outscse
      nwingsa = select(.data = nws,
                       YEAR,AOU) %>% 
        rename(year = YEAR) %>% 
        group_by(AOU) %>% 
        slice_sample(prop = nwing_scale)
      
    }else{
      if(is.null(z)){
        nws <- outscse[which(outscse$PRHUNT == reg2 & !is.na(outscse$YEAR)),]
        nwingsa = select(.data = nws,
                         YEAR,AOU,PRHUNT) %>% 
          rename(year = YEAR,
                 prov = PRHUNT) %>% 
          left_join(provzone[,c("prov","province")],by = c("prov")) %>% 
          group_by(AOU,province) %>% 
          slice_sample(prop = nwing_scale)
        
      }else{
        nws <- outscse[which(outscse$PRHUNT == reg2 & outscse$ZOHUNT == z & !is.na(outscse$YEAR)),]
        nwingsa = select(.data = nws,
                         YEAR,AOU,PRHUNT,ZOHUNT) %>% 
          rename(year = YEAR,
                 prov = PRHUNT,
                 zone = ZOHUNT) %>% 
          left_join(provzone[,c("prov","zone","province")],by = c("prov","zone")) %>% 
          group_by(AOU,province,zone) %>% 
          slice_sample(prop = nwing_scale)
      }
      
      
    }
    nwings <- left_join(nwingsa,species_web_names,by = "AOU")
    
    if(!is.null(sp)){
      nwings <- filter(nwings,species %in% sp)
    }
    

  }
  ## end n wings
  
  
  }


  if(!is.null(sp)){
    dat <- filter(dat,species %in% sp)
  }
  if(!is.null(reg)){
    dat <- filter(dat,province %in% reg)
  }
by_zone = FALSE
  if(!is.null(z)){
    dat <- filter(dat,zone %in% z)
    if(length(unique(dat$zone)) > 1){
      by_zone = TRUE
    }
  }
  
  

    
    

  
  dat$demog <- factor(dat$demog_group,levels = c("Adult","Immature","Female","Male",
                                                 "Adult-Female","Adult-Male",
                                                 "Immature-Female","Immature-Male"), ordered = T)
  
  source("Functions/demographics_palette.R")  
  
  
  if(!is.null(reg)){
    nreg <- length(reg)
    outggs <- vector(mode = "list",length = nreg)
    
    sps = unique(dat$species)
    nspecies <- length(sps)
    
    
    if(by_zone){
      my_facets <- facet_wrap(facets = ~species+zone,ncol = ceiling(sqrt(nspecies)),scales = facet_scales)
      
    }else{
      my_facets <- facet_wrap(facets = ~species,ncol = ceiling(sqrt(nspecies)),scales = facet_scales)
      if(nspecies == 1){
        my_facets <- NULL
      }
    }
    for(ppn in 1:length(reg)){
      pp = reg[ppn]
      datp <- filter(dat,province == pp)
      
      if(labs_inc){
        if(age){
        lbs = datp[which((datp$demog == "Adult" & datp$year == lbl_y[1] |
                            datp$demog == "Immature" & datp$year == lbl_y[2])),]
        }
        if(sex){
          lbs = datp[which((datp$demog == "Female" & datp$year == lbl_y[1] |
                              datp$demog == "Male" & datp$year == lbl_y[2])),]
          
        }
        if(both){
          lbs = datp[which((datp$demog == "Adult-Female" & datp$year == lbl_y[1] |
                              datp$demog == "Adult-Male" & datp$year == lbl_y[2] |
                              datp$demog == "Immature-Female" & datp$year == lbl_y[3]|
                              datp$demog == "Immature-Male" & datp$year == lbl_y[4])),]
          
        }
        if(!is.null(lab_sp)){
          lbs[which(lbs$species != lab_sp),"year"] <- NA
          lbs[which(lbs$species != lab_sp),"mean"] <- NA
          
        }
        
         lbs$lbl <- paste0(toupper(lbs$demog),"")
      }
      
      if(!is.null(startYear)){datp <- filter(datp,year >= startYear)}
      
      
      #}
      outgg = ggplot(data = datp,aes(x = year,y = mean,group = demog,fill = demog))+
        geom_point(aes(colour = demog),size = 0.5)+
        geom_line(aes(colour = demog))+
        labs(x = "",title = paste(pp,title_base))+
        ylab(unit)+
        geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,yup))+
        my_col+
        theme_classic()+
        theme(legend.position = "none",
              strip.text = element_text(size = 7))+
        my_facets
      #print(outgg)
      

      
      if(add_nwings){
        if(!is.null(startYear)){nwings <- filter(nwings,year >= startYear)}
        
        outgg <- outgg + ggplot2::geom_dotplot(data = nwings,mapping = ggplot2::aes(x = year),drop = TRUE,binaxis = "x", stackdir = "up",method = "histodot",binwidth = 1,width = 0.2,inherit.aes = FALSE,fill = grDevices::grey(0.6),colour = grDevices::grey(0.6),alpha = 0.2,dotsize = 0.3)
         
          if(add_n_labs){ 
            outgg <- outgg + annotate(geom = "text",x = max(c(1990,startYear)),y = 0,label = paste0("Each grey dot represents ",round(1/nwing_scale,0)," parts"),colour = grey(0.4),size = 2, hjust = 0)
          }
      }
      
      if(labs_inc){
        outgg <- outgg + geom_text_repel(data = lbs,aes(x = year,y = mean,label = lbl,group = demog,colour = demog),
                                         nudge_y = max(lbs$mean,na.rm = T)*0.4,nudge_x = diff(range(dat$year))*0.1,size = 3)
      }
      
      outggs[[ppn]] <- outgg
      
    }
  }
  
 
#   if(is.null(reg)){
#     
#     sps = unique(dat$species)
#     nspecies <- length(sps)
#     outggs <- vector(mode = "list",length = nspecies)
#     
#     if(by_zone){
#       my_facets <- facet_wrap(facets = ~province+zone,ncol = 3,scales = facet_scales)
#     }else{
#       my_facets <- facet_wrap(facets = ~province,ncol = 3,scales = facet_scales)
#     if(nspecies == 1){
#       my_facets <- NULL
#     }
#     }
#     
# for(ppn in 1:nspecies){
#   pp = sps[ppn]
#   datp <- filter(dat,species == pp)
#   if(!is.null(startYear)){datp <- filter(datp,year >= startYear)}
#   
#   outgg = ggplot(data = datp,aes(x = year,y = mean,group = demog,fill = demog))+
#     geom_point(aes(colour = demog),size = 0.5)+
#     geom_line(aes(colour = demog))+
#     labs(x = "",title = paste0(pp," Harvest (mean and 95 CI)"))+
#     geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
#     scale_y_continuous(limits = c(0,yup))+
#     my_col+
#     theme_classic()+
#     theme(legend.position = "none")+
#     my_facets
#   
#   #print(outgg)
#   
#   outggs[[ppn]] <- outgg
# }
#   }
  return(outggs)
}


