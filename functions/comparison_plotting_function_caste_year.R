### 


comp_plot_caste_year <- function(group = spgp,
                             var = "cst",
                             prov = "",
                             zone = "",
                             M = out2,
                             castes = jdat$castes ){
  
  
  
  
  dsum = as.data.frame(M$summary)
  names(dsum)[3:7] <- c("lci","lqrt","med","uqrt","uci")
  dsum$Parameter = row.names(dsum)
  d1 = filter(dsum,grepl(Parameter,pattern = paste0(var,"["),fixed = T))
  d1$caste = jags_dim(var = var,dat = d1)
  d1$yr = jags_dim(var = var,dat = d1,dim = 2)
  d1$year = d1$yr+(Y-(jdat$nyears))
  
  d1$mod = "Cst"
  var2 = paste0(var,"_day")
  d2 = filter(dsum,grepl(Parameter,pattern = paste0(var2,"["),fixed = T))
  d2$caste = jags_dim(var = var2,dat = d2)
  d2$yr = jags_dim(var = var2,dat = d2,dim = 2)
  d2$year = d2$yr+(Y-(jdat$nyears))
  
  d2$mod = "Cst_day"
  
  dd = bind_rows(d1,d2)

  dd$mod <- factor(dd$mod,levels = c("Old","New"), ordered = T)
  dd <- dd[which(dd$year >= FY),]
  my_col <-  scale_color_viridis_d(aesthetics = c("colour","fill"), begin = 0.3,end = 0.9,option = "B",direction = -1)
  
  outgg = ggplot(data = dd,aes(x = year,y = mean,group = mod,fill = mod))+
    geom_point(aes(colour = mod),size = 0.5)+
    geom_line(aes(colour = mod))+
    labs(title = paste0("caste specific effects ",prov," zn",zone," (mean and 95 CI)"))+
    geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
    #scale_y_continuous(limits = c(0,NA))+
    my_col+
    theme_classic()+
    facet_wrap(facets = ~caste,nrow = 2,ncol = 2,scales = "free")


  
  return(outgg)
}


