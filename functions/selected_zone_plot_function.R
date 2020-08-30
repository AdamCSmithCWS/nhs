### 


plot_sel_sp <- function(dat = zone_both_a,
                              sp = "Mallard",
                              p = "SK",
                              z = 1){
  
  
  pr = as.character(unique(dat[which(dat$prov == p),"province"]))

    dat <- filter(dat,species %in% sp,
                  province %in% pr,
                  zone %in% z)

  
  dat$mod <- factor(dat$model,levels = c("old","new"), ordered = T)
  
  my_col <-  scale_color_viridis_d(aesthetics = c("colour","fill"), begin = 0.3,end = 0.9,option = "B",direction = -1)
  
  


      outgg = ggplot(data = datp,aes(x = year,y = mean,group = mod,fill = mod))+
        geom_point(aes(colour = mod),size = 0.5)+
        geom_line(aes(colour = mod))+
        labs(title = paste0(pp," Harvest (mean and 95 CI)"))+
        geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,NA))+
        my_col+
        theme_classic()
 
  return(outgg)
}


