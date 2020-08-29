### 


comp_plot_simple <- function(dat = both_b,
                              reg = NULL,
                              z = NULL,
                             var_sel = NULL){
  
  
  if(!is.null(var_sel)){
    dat <- filter(dat,var %in% var_sel)
  }
  if(!is.null(reg)){
    dat <- filter(dat,province %in% reg)
  }
  if(!is.null(z)){
    dat <- filter(dat,zone %in% z)
  }
  

  
  dat$mod <- factor(dat$model,levels = c("old","new"), ordered = T)
  
  my_col <-  scale_color_viridis_d(aesthetics = c("colour","fill"), begin = 0.3,end = 0.9,option = "B",direction = -1)
  
  
  if(!is.null(reg)){
    nreg <- length(reg)
    outggs <- vector(mode = "list",length = nreg)
    
    for(ppn in 1:length(reg)){
      pp = reg[ppn]
      datp <- filter(dat,province == pp)
      outgg = ggplot(data = datp,aes(x = year,y = mean,group = mod,fill = mod))+
        geom_point(aes(colour = mod),size = 0.5)+
        geom_line(aes(colour = mod))+
        labs(title = paste0(pp," Simple estimates (mean and 95 CI)"))+
        geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,NA))+
        my_col+
        theme_classic()+
        facet_wrap(facets = ~name,ncol = 3,scales = "free")
      
      #print(outgg)
      
      outggs[[ppn]] <- outgg
      
    }
  }
  
 
  if(is.null(reg)){
    nms = unique(dat$name)
    nnms = length(nms)
    outggs <- vector(mode = "list",length = nnms)
    
for(ppn in 1:nnms){
  pp = nms[ppn]
  datp <- filter(dat,name == pp)
  outgg = ggplot(data = datp,aes(x = year,y = mean,group = mod,fill = mod))+
    geom_point(aes(colour = mod),size = 0.5)+
    geom_line(aes(colour = mod))+
    labs(title = paste0(pp," (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
    scale_y_continuous(limits = c(0,NA))+
    my_col+
    theme_classic()+
    facet_wrap(facets = ~province,ncol = 3,scales = "free")
  
  print(outgg)
  
  outggs[[ppn]] <- outgg
}
  }
  return(outggs)
}


