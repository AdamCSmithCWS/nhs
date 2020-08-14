### 


comp_plot_psy <- function(group = spgp,
                             var = "pcomp_psy",
                             prov = "",
                             zone = "",
                             M = out2,
                             nspecies = jdat$nspecies,
                          nperiods = jdat$nperiods,
                          nparts_sy = jdat$nparts_sy){
  
  
  
  
  dsum = as.data.frame(M$summary)
  names(dsum)[3:7] <- c("lci","lqrt","med","uqrt","uci")
  dsum$Parameter = row.names(dsum)
  
  d1 = filter(dsum,grepl(Parameter,pattern = paste0(var,"["),fixed = T))
  
  d1$per = jags_dim(var = var,dat = d1)
  d1$per_f = factor(d1$per)
  d1$yr = jags_dim(var = var,dat = d1,dim = 3)
  d1$sp = jags_dim(var = var,dat = d1,dim = 2)
  d1$year = d1$yr+(Y-(jdat$nyears))
  
  spls = sp.save.out[which(sp.save.out$PRHUNT == prov & sp.save.out$ZOHUNT == zone),]
  d1 = merge(d1,spls,by.x = "sp",by.y = "spn")
  d1 = merge(d1,provzone,by.x = c("PRHUNT","ZOHUNT"),by.y = c("prov","zone"))
  d1 = merge(d1,species_web_names,by.x = "AOU",by.y = "sp",all.x = T)
  d1$mod = "New"
  

  dd = d1
  
  
  for(i in 1:nrow(dd)){
    ss = dd[i,"sp"]
    yy = dd[i,"yr"]
    
    dd[i,"nparts"] <- sum(nparts_sy[ss,yy])
    
    # d = dd[i,"d"]
    # dd[i,"raw_count"] <- raw[d,ss,yy]
    # dd[i,"raw_pd"] <- raw[d,ss,yy]/sum(raw[,ss,yy])
    
  }
  
  

  dd <- dd[which(dd$year >= FY),]
  my_col <-  scale_color_viridis_d(aesthetics = c("colour","fill"), begin = 0.3,end = 0.9,option = "B",direction = -1)
  
  
  # part counts to plot ---------------------------------------------------
  
  
  ddb = dd
  for(ss in 1:nspecies){
    ws = which(ddb$sp == ss)
    ulim = max(ddb[ws,"uci"])
    
    ddb[ws,"partsplot"] <- (ddb[ws,"nparts"]/max(ddb[ws,"nparts"]))*(ulim/2)
    if(max(ddb[ws,"nparts"]) == 0){
      ddb[ws,"partsplot"] <- 0
      
    }
    ddb[ws,"partsplot"] <- (ddb[ws,"nparts"]/max(ddb[ws,"nparts"]))*(ulim/2)
  }
  
  ddb = unique(ddb[,c("AOU","year","nparts","partsplot")])
  ddbmx = tapply(ddb$partsplot,ddb$AOU,max)
  if(any(is.na(ddbmx))){ddbmx[which(is.na(ddbmx))] <- 0}
  wm = NULL
  ddbmn = tapply(ddb$partsplot,ddb$AOU,min)
  if(any(is.na(ddbmn))){ddbmn[which(is.na(ddbmn))] <- 0}
  wmn = NULL
  
  for(j in 1:length(ddbmx)){
    wm[j] <- which(ddb$partsplot == ddbmx[j] & ddb$AOU == names(ddbmx)[j])[1]
    wmn[j] <- which(ddb$partsplot == ddbmn[j] & ddb$AOU == names(ddbmn)[j])[1]
  }
  ddbm = ddb[c(wm,wmn),]
  

    outggs <- list()
    length(outggs) <- ceiling(nspecies/4)

  ############## end parts plot counts
  
  for(pp in 1:length(outggs)){
    outgg = ggplot(data = dd,aes(x = year,y = mean,group = per_f,fill = per_f))+
      geom_bar(data = ddb,inherit.aes = FALSE,aes(x = year,y = partsplot),fill = grey(0.7),alpha = 1,stat = "identity",width = 0.2)+
      #geom_point(aes(colour = per_f),size = 0.5)+
      geom_line(aes(colour = per_f))+
      labs(title = paste0("Species proportions in SCS by period ",prov," zn",zone," (mean and 95 CI)"))+
      geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.03)+
      scale_y_continuous(limits = c(0,NA))+
      my_col+
      theme_classic()+
      geom_text_repel(data = ddbm,inherit.aes = FALSE,aes(x = year,y = partsplot,label = nparts),size = 3,colour = grey(0.2),alpha = 0.75,position = position_dodge(width = 0.5))+
      facet_wrap_paginate(facets = ~AOU,nrow = 2,ncol = 2,scales = "free",page = pp)
    outggs[[pp]] <- outgg
  }
  
  return(outggs)
}


  
  
  
  
  
  


