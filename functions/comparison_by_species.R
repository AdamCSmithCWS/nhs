### 


comp_plot_species <- function(dat = both_a,
                              sp = NULL,
                              reg = NULL,
                              z = NULL,
                              add_samplesize = FALSE,
                              add_nwings = FALSE,
                              samplesize_scale = 1,
                              nwing_scale = 1,
                              title_base = "",
                              labs_inc = FALSE,
                              lbl_y = c(1990,1995),
                              lab_sp = NULL,
                              unit = NULL,
                              add_n_labs = TRUE,
                              startYear = NULL,
                              facet_scales = "free",
                              yup = NA,
                              xlb = "Year"){
  
  
  
  if(is.null(unit)){unit <- title_base}
  if(!is.null(reg)){
  reg2 <- provs[which(provs$province == reg),"prov"]
  if(add_samplesize){
    if(reg == "Canada"){
      allk <- allkill[which(allkill$ACTIVEWF == "Y" & !is.na(allkill$YEAR)),]
    }else{
      if(is.null(z)){
      allk <- allkill[which(allkill$ACTIVEWF == "Y" & allkill$PRHUNT == reg2 & !is.na(allkill$YEAR)),]
      }else{
        allk <- allkill[which(allkill$ACTIVEWF == "Y" & allkill$PRHUNT == reg2 & allkill$ZOHUNT == z & !is.na(allkill$YEAR)),]
        
      }
    }
    
    
    
    ssa = data.frame(year = allk$YEAR,
                        nresp = 1)
    
    ss <- ssa %>% 
      select(year) %>% 
      group_by(year) %>% 
      slice_sample(prop = samplesize_scale)
    
    
    tt = table(ssa$year)
    tt = tt[which(tt > 1)]
    
   
    ss_lab = data.frame(np = paste(min(tt),"HQS responses in",names(tt)[which.min(tt)]),
                           year = as.integer(names(tt)[which.min(tt)]),
                           mean = 0)
    if(ss_lab$year > 1985){
      xndg = -10
    }else{
      xndg = 10
    }
    
  }
  ## end sample size setup
  
  
  
  # add n wings -------------------------------------------------------------
  
  if(add_nwings){
    
    # npy = jdat$nparts_sy[ssp,]
    # min_p <- min(npy[which(npy > 0)])
    # nwing = data.frame(year = rep(as.integer(names(jdat$nhunter_y[1])),each = ceiling(jdat$nparts_sy[ssp,1]/dots_x)),
    #                    nresp = rep(1,each = ceiling(jdat$nparts_sy[ssp,1]/dots_x)))
    # for(y in 2:length(jdat$nhunter_y)){
    #   tmp = data.frame(year = rep(as.integer(names(jdat$nhunter_y[y])),each = ceiling(jdat$nparts_sy[ssp,y]/dots_x)),
    #                    nresp = rep(1,each = ceiling(jdat$nparts_sy[ssp,y]/dots_x)))
    #   nwing <- bind_rows(nwing,tmp)
    #   
    # }
    # 
    # tt = table(nwing$year)
    # tt = tt[which(tt > 0)]
    # 
    # nwing_lab = data.frame(np = paste(min_p,"parts in",names(tt)[which.min(tt)]),
    #                        year = as.integer(names(tt)[which.min(tt)]),
    #                        mean = 0)
    
    
    if(reg == "Canada"){
      nws <- outscse
      nwingsa = select(.data = nws,
                       YEAR,AOU) %>% 
        rename(year = YEAR) %>% 
        group_by(AOU,year) %>% 
        summarise(nw = ceiling(n()*nwing_scale))%>% 
        slice(rep(seq_len(n()), nw)) %>% 
        select(-nw)
      
    }else{
      if(is.null(z)){
        nws <- outscse[which(outscse$PRHUNT == reg2 & !is.na(outscse$YEAR)),]
        nwingsa = select(.data = nws,
                         YEAR,AOU,PRHUNT) %>% 
          rename(year = YEAR,
                 prov = PRHUNT) %>% 
          left_join(provzone[,c("prov","province")],by = c("prov")) %>% 
          group_by(AOU,province,year) %>% 
          summarise(nw = ceiling(n()*nwing_scale))%>% 
          slice(rep(seq_len(n()), nw)) %>% 
          select(-nw)
        
      }else{
        nws <- outscse[which(outscse$PRHUNT == reg2 & outscse$ZOHUNT == z & !is.na(outscse$YEAR)),]
        nwingsa = select(.data = nws,
                         YEAR,AOU,PRHUNT,ZOHUNT) %>% 
          rename(year = YEAR,
                 prov = PRHUNT,
                 zone = ZOHUNT) %>% 
          left_join(provzone[,c("prov","zone","province")],by = c("prov","zone")) %>% 
          group_by(AOU,province,zone,year) %>% 
          summarise(nw = ceiling(n()*nwing_scale))%>% 
          slice(rep(seq_len(n()), nw)) %>% 
          select(-nw)
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
  if(!is.null(z)){
    dat <- filter(dat,zone %in% z)
  }
  
  
  if(length(unique(dat$zone)) > 1){
    by_zone = TRUE
  }else{by_zone = FALSE}
    
    

  
  dat$mod <- factor(dat$model,levels = c("old","new"), ordered = T)
  
  source("Functions/palette.R")  
  
  
  if(!is.null(reg)){
    nreg <- length(reg)
    outggs <- vector(mode = "list",length = nreg)
    
    sps = unique(dat$species)
    nspecies <- length(sps)
    
    
    if(by_zone){
      my_facets <- facet_wrap(facets = ~species_sentence_case+zone,ncol = 3,scales = facet_scales)
      
    }else{
      my_facets <- facet_wrap(facets = ~species_sentence_case,ncol = 3,scales = facet_scales)
      if(nspecies == 1){
        my_facets <- NULL
      }
    }
    for(ppn in 1:length(reg)){
      pp = reg[ppn]
      datp <- filter(dat,province == pp)
      
      if(labs_inc){
        lbs = datp[which((datp$model == "new" & datp$year == lbl_y[1] |
                            datp$model == "old" & datp$year == lbl_y[2])),]
        if(!is.null(lab_sp)){
          lbs[which(lbs$species != lab_sp),"year"] <- NA
          lbs[which(lbs$species != lab_sp),"mean"] <- NA
          
        }
        
        lbs$lbl <- paste(str_to_sentence(lbs$model),"model")
      }
      
      if(!is.null(startYear)){datp <- filter(datp,year >= startYear)}
      
      
      #}
      outgg = ggplot(data = datp,aes(x = year,y = mean,group = mod,fill = mod))+
        geom_point(aes(colour = mod),size = 0.5)+
        geom_line(aes(colour = mod))+
        labs(title = paste(title_base))+
        ylab(unit)+
        xlab(xlb)+
        geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,yup),labels = scales::comma)+
        my_col+
        theme_classic()+
        theme(text = element_text(family = "Times"),
              legend.position = "none",
              strip.text = element_text(size = 10),
              axis.text = element_text(size = 8))+
        my_facets
      #print(outgg)
      
      if(add_samplesize){
        if(!is.null(startYear)){ss <- filter(ss,year >= startYear)}
        
        outgg <- outgg + ggplot2::geom_dotplot(data = ss,mapping = ggplot2::aes(x = year),drop = TRUE,binaxis = "x", stackdir = "up",method = "histodot",binwidth = 1,width = 0.2,inherit.aes = FALSE,fill = grDevices::grey(0.6),colour = grDevices::grey(0.6),alpha = 0.2,dotsize = 0.3)+
          annotate(geom = "text",x = max(c(1990,startYear)),y = 0,label = paste0("Each grey dot represents ",round(1/samplesize_scale,0)," responses"),colour = grey(0.4),size = 2, hjust = 0)
        
      }
      
      if(add_nwings){
        
        
        
        
        if(!is.null(startYear)){nwings <- filter(nwings,year >= startYear)}
        
        outgg <- outgg + ggplot2::geom_dotplot(data = nwings,mapping = ggplot2::aes(x = year),drop = TRUE,binaxis = "x", stackdir = "up",method = "histodot",binwidth = 1,width = 0.2,inherit.aes = FALSE,fill = grDevices::grey(0.5),colour = grDevices::grey(0.3),alpha = 0.2,dotsize = 0.6)
         
          if(add_n_labs){ 
            outgg <- outgg + annotate(geom = "text",x = max(c(1990,startYear)),y = 0,label = paste0("Each grey dot represents ",round(1/nwing_scale,0)," parts"),colour = grey(0.4),size = 2, hjust = 0)
          }
      }
      
      if(labs_inc){
        outgg <- outgg + geom_text_repel(data = lbs,aes(x = year,y = mean,label = lbl,group = mod,colour = mod),
                                         nudge_y = max(lbs$mean,na.rm = T)*0.4,nudge_x = diff(range(dat$year))*0.1,size = 3,
                                         family = "Times")
      }
      
      outggs[[ppn]] <- outgg
      
    }
  }
  
 
  if(is.null(reg)){
    
    sps = unique(dat$species)
    nspecies <- length(sps)
    outggs <- vector(mode = "list",length = nspecies)
    
    if(by_zone){
      my_facets <- facet_wrap(facets = ~province+zone,ncol = 3,scales = facet_scales)
    }else{
      my_facets <- facet_wrap(facets = ~province,ncol = 3,scales = facet_scales)
    if(nspecies == 1){
      my_facets <- NULL
    }
    }
    
for(ppn in 1:nspecies){
  pp = sps[ppn]
  datp <- filter(dat,species == pp)
  if(!is.null(startYear)){datp <- filter(datp,year >= startYear)}
  
  outgg = ggplot(data = datp,aes(x = year,y = mean,group = mod,fill = mod))+
    geom_point(aes(colour = mod),size = 0.5)+
    geom_line(aes(colour = mod))+
    labs(x = "",title = paste0(pp," Harvest (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
    scale_y_continuous(limits = c(0,yup),labels = scales::comma)+
    my_col+
    theme_classic()+
    theme(legend.position = "none")+
    my_facets
  
  #print(outgg)
  
  outggs[[ppn]] <- outgg
}
  }
  return(outggs)
}


