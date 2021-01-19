### 


parts_by_harvest <- function(dat = both_a,
                              sp = NULL,
                              reg = NULL,
                              z = NULL,
                              add_nwings = TRUE,
                              samplesize_scale = 1,
                              nwing_scale = 1,
                              title_base = "Parts per Harvest",
                              labs_inc = FALSE,
                              lbl_y = c(1990,1995),
                              lab_sp = NULL,
                              unit = NULL,
                              add_n_labs = TRUE,
                              startYear = NULL,
                              facet_scales = "fixed",
                             prov_means = TRUE){
  
  
  
  
  dat <- filter(dat,model == "new")
  
  if(!is.null(reg)){
  reg2 <- provs[which(provs$province %in% reg),"prov"]
  

  
  # add n wings -------------------------------------------------------------
  
  if(add_nwings){
    if(reg[1] == "Canada"){
      nws <- outscse
      nwingsa = select(.data = nws,
                       YEAR,AOU) %>% 
        rename(year = YEAR) %>% 
        group_by(year,AOU) %>%
        summarise(nwings = n())
      
    }else{
      if(is.null(z)){
        nws <- outscse[which(outscse$PRHUNT %in% reg2 & !is.na(outscse$YEAR)),]
        nwingsa = select(.data = nws,
                         YEAR,AOU,PRHUNT) %>% 
          rename(year = YEAR,
                 prov = PRHUNT) %>% 
          left_join(provzone[,c("prov","province")],by = c("prov")) %>% 
          group_by(year,AOU,province) %>% 
          summarise(nwings = n())
        
      }else{
        nws <- outscse[which(outscse$PRHUNT %in% reg2 & outscse$ZOHUNT == z & !is.na(outscse$YEAR)),]
        nwingsa = select(.data = nws,
                         YEAR,AOU,PRHUNT,ZOHUNT) %>% 
          rename(year = YEAR,
                 prov = PRHUNT,
                 zone = ZOHUNT) %>% 
          left_join(provzone[,c("prov","zone","province")],by = c("prov","zone")) %>% 
          group_by(year,AOU,province,zone) %>%
          summarise(nwings = n())
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
    
    dat <- left_join(dat,nwings) %>% 
      mutate(w_by_harvest = nwings/mean)

  
    
    
    
    
    
    
    
    if(prov_means){
      
      dat1 <- dat %>% group_by(province,AOU,species) %>% 
        summarise(mw_by_harvest = mean(w_by_harvest),
                  lci = quantile(w_by_harvest,0.05),
                  uci = quantile(w_by_harvest,0.95))
      
      outggs = ggplot(data = dat1,aes(x = province,y = mw_by_harvest))+
        geom_pointrange(aes(ymin = lci,ymax = uci))+
        labs(x= "",title = paste(title_base))+
        ylab(unit)+
        #geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,NA))+
        #my_col+
        theme_classic()+
        theme(legend.position = "none",
              strip.text = element_text(size = 7))
      
      
    }else{
    

  if(!is.null(reg)){
    nreg <- length(reg)
  

    
    if(by_zone){
      my_facets <- facet_wrap(facets = ~province+zone,ncol = 3,scales = facet_scales)
      
    }else{
      my_facets <- facet_wrap(facets = ~province,ncol = 3,scales = facet_scales)
     }

    
          if(!is.null(startYear)){dat <- filter(dat,year >= startYear)}
      
      
      #}
      outggs = ggplot(data = dat,aes(x = year,y = w_by_harvest))+
        geom_point(size = 0.5)+
        geom_line()+
        labs(x= "",title = paste(title_base))+
        ylab(unit)+
        #geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,NA))+
        #my_col+
        theme_classic()+
        theme(legend.position = "none",
              strip.text = element_text(size = 7))+
        my_facets
      #print(outgg)
      
}

      # if(labs_inc){
      #   outgg <- outgg + geom_text_repel(data = lbs,aes(x = year,y = mean,label = lbl,group = mod,colour = mod),
      #                                    nudge_y = max(lbs$mean,na.rm = T)*0.4,nudge_x = diff(range(dat$year))*0.1,size = 3)
      # }
 
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
#   outgg = ggplot(data = datp,aes(x = year,y = mean,group = mod,fill = mod))+
#     geom_point(aes(colour = mod),size = 0.5)+
#     geom_line(aes(colour = mod))+
#     labs(title = paste0(pp," Harvest (mean and 95 CI)"))+
#     geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
#     scale_y_continuous(limits = c(0,NA))+
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


