### 


comp_plot_simple <- function(group = spgp,
                             var = "NACTIVE_y",
                             prov = "",
                             zone = "",
                             M = out2){
  
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
  
  oldvar = var_pair[which(var_pair[,"new"] == var),group]
  
  province = unique(provzone[which(provzone$prov == prov),"province"])
  d2 = pubEsts_simple[which(pubEsts_simple$var == oldvar &
                              pubEsts_simple$prov == province & pubEsts_simple$zone == zone),]
  if(nrow(d2) == 0){
    dd = d1
  }else{
  d2$mod = "Old"
  dd = bind_rows(d1,d2)
  }
  
  if(any(!is.finite(dd$sd))){
  outgg = ggplot(data = dd,aes(x = year,y = med,group = mod,fill = mod))+
    geom_point(aes(colour = mod))+
    labs(title = paste0("Median and quartiles",var," vs ",oldvar," ",prov," zn",zone," (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uqrt,ymin = lqrt),alpha = 0.2)+
    scale_y_continuous(limits = c(0,NA))+
    scale_color_viridis_d(aesthetics = c("colour","fill"), end = 0.7)+
    theme_classic()
  }else{
  outgg = ggplot(data = dd,aes(x = year,y = mean,group = mod,fill = mod))+
    geom_point(aes(colour = mod))+
    labs(title = paste0(var," vs ",oldvar," ",prov," zn",zone," (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
    scale_y_continuous(limits = c(0,NA))+
    scale_color_viridis_d(aesthetics = c("colour","fill"), end = 0.7)+
    theme_classic()
    }
  
  
  return(outgg)
}


