### 


comp_plot_species <- function(group = spgp,
                             var = "kill_ys",
                             prov = "",
                             zone = "",
                             M = out2,
                             nspecies = jdat$nspecies){
  
  
  
  
  dsum = as.data.frame(M$summary)
  names(dsum)[3:7] <- c("lci","lqrt","med","uqrt","uci")
  dsum$Parameter = row.names(dsum)
  d1 = filter(dsum,grepl(Parameter,pattern = paste0(var,"["),fixed = T))
  d1$yr = jags_dim(var = var,dat = d1)
  d1$sp = jags_dim(var = var,dat = d1,dim = 2)
  d1$year = d1$yr+(Y-(jdat$nyears))
  
  spls = sp.save.out[which(sp.save.out$PRHUNT == prov & sp.save.out$ZOHUNT == zone),]
  d1 = merge(d1,spls,by.x = "sp",by.y = "spn")
  d1 = merge(d1,provzone,by.x = c("PRHUNT","ZOHUNT"),by.y = c("prov","zone"))
  d1 = merge(d1,species_web_names,by.x = "AOU",by.y = "sp",all.x = T)
  d1$mod = "New"
  
  d2 = pubEsts_species[which(pubEsts_species$prov == unique(d1$province) & pubEsts_species$zone == zone),]
  names(d2) <- c("AOU","species","province","ZOHUNT","year","mean","sd","lci","uci")
  d2$mod = "Old"
  d2 = filter(d2,AOU %in% aou.spgp)
  
  dd = suppressWarnings(bind_rows(d1,d2))
  outggs <- list()
  length(outggs) <- ceiling(nspecies/9)
  
  
  dd$mod <- factor(dd$mod,levels = c("Old","New"), ordered = T)
  dd <- dd[which(dd$year >= FY),]
  my_col <-  scale_color_viridis_d(aesthetics = c("colour","fill"), begin = 0.3,end = 0.9,option = "B",direction = -1)
  
  
for(pp in 1:(ceiling(nspecies/9))){
  outgg = ggplot(data = dd,aes(x = year,y = mean,group = mod,fill = mod))+
    geom_point(aes(colour = mod),size = 0.5)+
    geom_line(aes(colour = mod))+
    labs(title = paste0("species level harvest ",prov," zn",zone," (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
    scale_y_continuous(limits = c(0,NA))+
    my_col+
    theme_classic()+
    facet_wrap_paginate(facets = ~AOU,nrow = 2,ncol = 2,scales = "free",page = pp)
  outggs[[pp]] <- outgg
}
  
  return(outggs)
}


