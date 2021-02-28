### 


plot_simple_b<- function(dat = both_b,
                        startYear = 1976){
  
  require(ggforce)
     dat <- filter(dat,
                  year > (startYear - 1))
    
     
     
  dat$mod <- factor(dat$model,levels = c("old","new"), ordered = T)
  
source("Functions/palette.R")  

npg = floor(nrow(unique(dat[,c("province","name")]))/9)
  rem = nrow(unique(dat[,c("province","name")]))-(npg*9)
  if(rem > 1){
  outgg = vector("list",length = npg+1)
}else{
  outgg = vector("list",length = npg)
}
      for(i in 1:npg){
        outgg[[i]] = ggplot(data = dat,aes(x = year,y = mean,fill = mod))+
        geom_point(aes(colour = mod),size = 0.5)+
        geom_line(aes(colour = mod))+
        ylab("")+
        #labs(title = ttle)+
        geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
        my_col+
        # geom_text_repel(data = nwing_lab,aes(x = year,y = mean,label = np),
        #                 inherit.aes = FALSE,nudge_y = max(dat$mean)*-0.03,nudge_x = xndg,
        #                 colour = grey(0.7),min.segment.length = 0,size = 3)+
        theme_classic()+
        theme(legend.position = "right",
              title = element_text(size = 9))+
        coord_cartesian(xlim = c(startYear, 2019))+
        facet_wrap_paginate(~province+name,ncol = 3,nrow = 3,scales = "free",page = i)
      
      }

  if(rem > 1){
    nc = floor(sqrt(rem))
    nr = 1+ceiling((rem-nc)/nc)
    outgg[[npg+1]] = ggplot(data = dat,aes(x = year,y = mean,fill = mod))+
      geom_point(aes(colour = mod),size = 0.5)+
      geom_line(aes(colour = mod))+
      ylab("")+
      #labs(title = ttle)+
      geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
      scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
      my_col+
      # geom_text_repel(data = nwing_lab,aes(x = year,y = mean,label = np),
      #                 inherit.aes = FALSE,nudge_y = max(dat$mean)*-0.03,nudge_x = xndg,
      #                 colour = grey(0.7),min.segment.length = 0,size = 3)+
      theme_classic()+
      theme(legend.position = "right",
            title = element_text(size = 9))+
      coord_cartesian(xlim = c(startYear, 2019))+
      facet_wrap_paginate(~province+name,ncol = nc,nrow = nr,scales = "free",page = i)
  }
        
 
  return(outgg)
}


