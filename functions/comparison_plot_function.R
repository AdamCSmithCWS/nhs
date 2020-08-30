### 


comp_plot_species <- function(dat = both_a,
                              sp = NULL,
                              reg = NULL,
                              z = NA){
  
  
  if(!is.null(sp)){
    dat <- filter(dat,species %in% sp)
  }
  if(!is.null(reg)){
    dat <- filter(dat,province %in% reg)
  }
  if(!is.null(z)){
    dat <- filter(dat,zone %in% z)
  }
  
  sps = unique(dat$species)
  nspecies <- length(sps)
  outggs <- vector(mode = "list",length = nspecies)

  
  dat$mod <- factor(dat$model,levels = c("old","new"), ordered = T)
  
  source("Functions/palette.R")  
  
  
for(ppn in 1:nspecies){
  pp = sps[ppn]
  datp <- filter(dat,species == pp)
  outgg = ggplot(data = datp,aes(x = year,y = mean,group = mod,fill = mod))+
    geom_point(aes(colour = mod),size = 0.5)+
    geom_line(aes(colour = mod))+
    labs(title = paste0(pp," Harvest (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
    scale_y_continuous(limits = c(0,NA))+
    my_col+
    theme_classic()+
    facet_wrap(facets = ~province,ncol = 3,scales = "free")
  
  print(outgg)
  
  outggs[[ppn]] <- outgg
}
  
  return(outggs)
}


