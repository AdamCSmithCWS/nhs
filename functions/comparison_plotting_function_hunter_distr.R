### 


comp_plot_hunter <- function(group = spgp,
                             var = "hntr",
                             prov = "",
                             zone = "",
                             M = out3,
                             castes = jdat$castes ){
  
  
  
  
  dsum = as.data.frame(M$summary)
  names(dsum)[3:7] <- c("lci","lqrt","med","uqrt","uci")
  dsum$Parameter = row.names(dsum)
  d1 = filter(dsum,grepl(Parameter,pattern = paste0(var,"["),fixed = T))
  d1$caste = factor(jags_dim(var = var,dat = d1))
  d1$yr = jags_dim(var = var,dat = d1,dim = 2)
  d1$hunter = jags_dim(var = var,dat = d1,dim = 3)
  d1$year = d1$yr+(Y-(jdat$nyears))
  

  dd = d1
outggs <- list()
length(outggs) <- ceiling(nyears/12)
  for(pp in 1:length(outggs)){
  outgg = ggplot(data = dd,aes(x = med,group = caste))+
    geom_freqpoly(aes(colour = caste),size = 0.5)+
    labs(title = paste0("caste specific hunter effects ",prov," zn",zone," (mean and 95 CI)"))+
    scale_color_viridis_d(aesthetics = c("colour"), end = 0.7)+
    theme_classic()+
    facet_wrap_paginate(facets = ~year,nrow = 4,ncol = 3,scales = "free",page = pp)

  outggs[[pp]] <- outgg
}#

  
  return(outggs)
}


