### 


comp_plot_axsy <- function(group = spgp,
                             var = "axcomp_axsy",
                             prov = "",
                             zone = "",
                             M = out2,
                             nspecies = jdat$nspecies,
                          nparts_sy = jdat$nparts_sy,
                          ndemog = jdat$ndemog,
                          ds = demog,
                          nyears = jdat$nyears,
                          spls = sp.save.out[which(sp.save.out$PRHUNT == pr & sp.save.out$ZOHUNT == z),],
                          raw = jdat$w_axsy){
  
  
  
  
  dsum = as.data.frame(M$summary)
  names(dsum)[3:7] <- c("lci","lqrt","med","uqrt","uci")
  dsum$Parameter = row.names(dsum)
  
  d1 = filter(dsum,grepl(Parameter,pattern = paste0(var,"["),fixed = T))
  
  d1$d = jags_dim(var = var,dat = d1,dim = 1)
  d1$sp = jags_dim(var = var,dat = d1,dim = 2)
  d1$yr = jags_dim(var = var,dat = d1,dim = 3)
  d1$year = d1$yr+(Y-(nyears))
  d1 = merge(d1,spls,by.x = "sp",by.y = "spn")
  
  ds$d = 1:ndemog
  d1 = merge(d1,ds,by = "d")
  d1$demo = paste(d1$BAGE,d1$BSEX,sep = "_")
  
  dd = d1
  outggs <- list()
  length(outggs) <- ceiling(nspecies/9)
  
  
  
  for(i in 1:nrow(dd)){
      ss = dd[i,"sp"]
     yy = dd[i,"yr"]
    
    dd[i,"nparts"] <- sum(nparts_sy[ss,yy])
    
    d = dd[i,"d"]
    dd[i,"raw_count"] <- raw[d,ss,yy]
    dd[i,"raw_pd"] <- raw[d,ss,yy]/sum(raw[,ss,yy])
    
  }
  
  
  

# part counts to plot ---------------------------------------------------

  
  ulim = max(dd$uci)
  ddb = dd
  for(ss in 1:nspecies){
    ws = which(ddb$sp == ss)
  ddb[ws,"partsplot"] <- (ddb[ws,"nparts"]/max(ddb[ws,"nparts"]))*(ulim/2)
  }
  
  ddb = unique(ddb[,c("AOU","year","nparts","partsplot")])
  ddbmx = tapply(ddb$partsplot,ddb$AOU,max)
  wm = NULL
  ddbmn = tapply(ddb$partsplot,ddb$AOU,min)
  wmn = NULL
  
  for(j in 1:length(ddbmx)){
    wm[j] <- which(ddb$partsplot == ddbmx[j] & ddb$AOU == names(ddbmx)[j])[1]
    wmn[j] <- which(ddb$partsplot == ddbmn[j] & ddb$AOU == names(ddbmn)[j])[1]
  }
  ddbm = ddb[c(wm,wmn),]
  
  outggs <- list()
  length(outggs) <- nspecies
############## end parts plot counts
  
for(pp in 1:nspecies){
  outgg = ggplot(data = dd,aes(x = year,y = mean,group = demo,fill = demo))+
    geom_bar(data = ddb,inherit.aes = FALSE,aes(x = year,y = partsplot),fill = grey(0.7),alpha = 1,stat = "identity",width = 0.2)+
    #geom_point(aes(colour = demo),size = 0.5)+
    geom_line(aes(colour = demo))+
    geom_point(aes(x = year,y = raw_pd,colour = demo))+
    labs(title = paste0("Age and sex proportions ",prov," zn",zone," (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.1)+
    scale_y_continuous(limits = c(0,NA))+
    scale_color_viridis_d(aesthetics = c("colour","fill"),end = 0.7)+
    theme_classic()+
    geom_text_repel(data = ddbm,inherit.aes = FALSE,aes(x = year,y = partsplot,label = nparts),size = 3,colour = grey(0.2),alpha = 0.75,nudge_y = ulim*-0.1)+
    facet_wrap_paginate(facets = ~AOU+demo,nrow = 2,ncol = 2,scales = "fixed",page = pp)
  outggs[[pp]] <- outgg
}
  
  return(outggs)
}


