### 


comp_plot_species_CV <- function(dat = both_a,
                              sp = NULL,
                              reg = NULL,
                              z = NULL){
  
  
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
  }
  
  
  sps = unique(dat$species)
  nspecies <- length(sps)
  outggs <- vector(mode = "list",length = nspecies)

  
  dat$mod <- factor(dat$model,levels = c("old","new"), ordered = T)
  
  my_col <-  scale_color_viridis_d(aesthetics = c("colour","fill"), begin = 0.3,end = 0.9,option = "B",direction = -1)
  
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


