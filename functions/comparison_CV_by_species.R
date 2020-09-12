### 


comp_plot_species_CV <- function(dat = both_a,
                              sp = NULL,
                              reg = NULL,
                              z = NULL,
                              labs_inc = FALSE,
                              lbl_y = c(1990,1995),
                              lab_sp = NULL){
  
  
  if(!is.null(sp)){
    dat <- filter(dat,species %in% sp)
  }
  if(!is.null(reg)){
    dat <- filter(dat,province %in% reg)
  }
  if(!is.null(z)){
    dat <- filter(dat,zone %in% z)
  }
  if(length(unique(dat$zone)) > 1){
    by_zone = TRUE
  }else{by_zone = FALSE}
  
  
  sps = unique(dat$species)
  nspecies <- length(sps)
  outggs <- vector(mode = "list",length = nspecies)

  
  dat$mod <- factor(dat$model,levels = c("old","new"), ordered = T)
  
  source("Functions/palette.R")  
  
  dat[,"CV"] <- ((dat$uci-dat$lci)/(qnorm(0.975)*2))/dat$mean
  
  w_lci0 = which(dat$lci == 0)
  dat[w_lci0,"CV"] <- (dat[w_lci0,"sd"])/dat[w_lci0,"mean"]
  
  

  
  if(!is.null(reg)){
    nreg <- length(reg)
    outggs <- vector(mode = "list",length = nreg)
    if(by_zone){
      my_facets <- facet_wrap(facets = ~species+zone,ncol = 3,scales = "free")
      
    }else{
      my_facets <- facet_wrap(facets = ~species,ncol = 3,scales = "free")
      
    }
    for(ppn in 1:length(reg)){
      pp = reg[ppn]
      datp <- filter(dat,province == pp)
      if(labs_inc){
        if(!is.null(lab_sp)){
        lbs = datp[which((datp$model == "new" & datp$year == lbl_y[1] |
                          datp$model == "old" & datp$year == lbl_y[2]) &
                           datp$species == lab_sp),]
        }else{
          lbs = datp[which((datp$model == "new" & datp$year == lbl_y[1] |
                              datp$model == "old" & datp$year == lbl_y[2])),]
        }
        
        lbs$lbl <- paste(toupper(lbs$model),"model")
      }
      
      outgg = ggplot(data = datp,aes(x = year,y = CV,group = mod,fill = mod))+
        geom_point(aes(colour = mod),size = 0.5)+
        geom_line(aes(colour = mod))+
        ylab("Coefficient of Variation (SD/mean)")+
        labs(title = paste0("Variance of ",pp," harvest estimates by year"))+
        #geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,NA))+
        my_col+
        theme_classic()+
        theme(legend.position = "none",
              strip.text = element_text(size = 7))+
        my_facets
      
      if(labs_inc){
        outgg <- outgg+geom_text_repel(data = lbs,aes(label = lbl,colour = mod),
                                       nudge_y = c(0.05,0),nudge_x = c(6,6),min.segment.length = 0,size = 3)
      }
      #print(outgg)
      
      outggs[[ppn]] <- outgg
      
    }
  }
  
    if(is.null(reg)){
      if(by_zone){
        my_facets <- facet_wrap(facets = ~province+zone,ncol = 3,scales = "free")
      }else{
        my_facets <- facet_wrap(facets = ~province,ncol = 3,scales = "free")
      }
    for(ppn in 1:nspecies){
  pp = sps[ppn]
  datp <- filter(dat,species == pp)
  outgg = ggplot(data = datp,aes(x = year,y = CV,group = mod,fill = mod))+
    geom_point(aes(colour = mod),size = 0.5)+
    geom_line(aes(colour = mod))+
    labs(title = paste0(pp," CV of Harvest (mean and 95 CI)"))+
    #geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
    scale_y_continuous(limits = c(0,NA))+
    my_col+
    theme_classic()+
    my_facets
  
  #print(outgg)
  
  outggs[[ppn]] <- outgg
}
}

  return(outggs)
}


