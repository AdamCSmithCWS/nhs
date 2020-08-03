### 


comp_plot_simple_other <- function(prov = "",
                             zone = "",
                             M = out2){
  
  
  
  outggl <- vector("list",length = 5)
  my_col <-  scale_color_viridis_d(aesthetics = c("colour","fill"), begin = 0.3,end = 0.9,option = "B",direction = -1)
  
  for(i in c(1:5)){
  
    var <- var_pair$new[[i]]
    oldvar <- var_pair$old[[i]]
    grpnms <- var_pair$newgrps[[i]]
    if(all(is.na(oldvar))){
      oldvar <- var_pair$old[[i-1]] 
    }
    if(i %in% c(1:3,5)){
      
      dsum = as.data.frame(M$summary)
      names(dsum)[3:7] <- c("lci","lqrt","med","uqrt","uci")
      dsum$Parameter = row.names(dsum)
      d1 = filter(dsum,grepl(Parameter,pattern = paste0(var,"["),fixed = T))
      d1$yr = jags_dim(var = var,dat = d1)
      d1$grp = jags_dim(var = var,dat = d1,dim = 2)
      d1$group = grpnms[d1$grp]
      d1$year = d1$yr+(Y-(jdat$nyears))
      d1$prov = prov
      d1$zone = zone
      d1$mod = "New"
      
      
      province = unique(provzone[which(provzone$prov == prov),"province"])

      d2 = pubEsts_simple[which(pubEsts_simple$var %in% oldvar &
                                  pubEsts_simple$prov == province & pubEsts_simple$zone == zone),]
    
      if(nrow(d2) == 0){
        dd = d1
      }else{
        d2$mod = "Old"
        d2$group = d2$var
        dd = bind_rows(d2,d1)
      }
      
      
      dd$mod <- factor(dd$mod,levels = c("Old","New"), ordered = T)
      dd <- dd[which(dd$year >= FY),]
      labls <- NULL
      for(g in 1:length(grpnms)){
      tmp <- filter(dd,dd$yr == fyear[g] & dd$group == grpnms[g] )
      labls <- rbind(labls,tmp)
      }
      
if(length(oldvar) > 1){
  nf = ceiling(length(oldvar)/2)
  outgg = ggplot(data = dd,aes(x = year,y = mean,group = mod,fill = mod))+
    geom_point(aes(colour = mod))+
    labs(title = paste0(var,prov," zn",zone," (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
    scale_y_continuous(limits = c(0,NA))+
    my_col+
    ggrepel::geom_text_repel(data = labls,aes(colour = mod),inherit.aes = TRUE,label = group)+
    facet_wrap(facets = ~group,ncol = nf,nrow = 2,scales = "free")+
    theme_classic()
}else{
  outgg = ggplot(data = dd,aes(x = year,y = mean,group = group,fill = mod))+
    geom_point(aes(colour = mod))+
    labs(title = paste0(var," vs ",oldvar," ",prov," zn",zone," (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
    scale_y_continuous(limits = c(0,NA))+
    my_col+
    ggrepel::geom_text_repel(data = labls,aes(colour = mod),inherit.aes = TRUE,label = group)+
    theme_classic()  
}
      
      
      
      
      
    }else{
   
   
  wd1 = paste0(var,"[",1:nyears,"]")  
  d1 = as.data.frame(M$summary[wd1,])
  names(d1)[3:7] <- c("lci","lqrt","med","uqrt","uci")
  d1$varbl = row.names(d1)
  d1$var = var
  d1$yr = 1:nyears
  d1$year = years
  d1$prov = prov
  d1$zone = zone
  d1$mod = "New"
  
 
  
  province = unique(provzone[which(provzone$prov == prov),"province"])
  d2 = pubEsts_simple[which(pubEsts_simple$var == oldvar &
                              pubEsts_simple$prov == province & pubEsts_simple$zone == zone),]
  if(nrow(d2) == 0){
    dd = d1
  }else{
  d2$mod = "Old"
  dd = bind_rows(d2,d1)
  }
  
  
  dd$mod <- factor(dd$mod,levels = c("Old","New"), ordered = T)
  dd <- dd[which(dd$year >= FY),]
  
  
  if(any(!is.finite(dd$sd))){
  outgg = ggplot(data = dd,aes(x = year,y = med,group = mod,fill = mod))+
    geom_point(aes(colour = mod))+
    labs(title = paste0("Median and quartiles",var," vs ",oldvar," ",prov," zn",zone," (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uqrt,ymin = lqrt),alpha = 0.2)+
    scale_y_continuous(limits = c(0,NA))+
    my_col+
    theme_classic()
  }else{
  outgg = ggplot(data = dd,aes(x = year,y = mean,group = mod,fill = mod))+
    geom_point(aes(colour = mod))+
    labs(title = paste0(var," vs ",oldvar," ",prov," zn",zone," (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
    scale_y_continuous(limits = c(0,NA))+
    my_col+
    theme_classic()
    }
  
  }
  
outggl[[i]] <- outgg 
}

  
  return(outggl)
  
}

