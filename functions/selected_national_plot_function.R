### 


plot_sel_national <- function(dat = both_b,
                              g = "TODUK",
                        p = "Canada",
                        z = NULL,
                        spgp = "duck",
                        labs_inc = FALSE,
                        lbl_y = c(1990,1995)){
  
  
  pr = as.character(unique(dat[which(dat$prov == p),"province"]))

     dat <- filter(dat,
                   var %in% g,
                  province %in% pr,
                  year > 1975)
    
      if(!is.null(z)){
       dat <- filter(dat,zone %in% z)
       load(paste("data/data",p,spgp,"save.RData",sep = "_"))
       
       
       nresp = data.frame(year = rep(as.integer(names(jdat$nhunter_y))[1],each = jdat$nhunter_y[1]),
                          nresp = rep(1,each = jdat$nhunter_y[1]))
       for(y in 2:length(jdat$nhunter_y)){
         tmp = data.frame(year = rep(as.integer(names(jdat$nhunter_y))[y],each = jdat$nhunter_y[y]),
                          nresp = rep(1,each = jdat$nhunter_y[y]))
         nresp <- bind_rows(nresp,tmp)
       }
       
       
       
       tt = table(nresp$year)
       tt = tt[which(tt > 1)]
       
       nwing_lab = data.frame(np = paste(min(tt),"parts in",names(tt)[which.min(tt)]),
                              year = as.integer(names(tt)[which.min(tt)]),
                              mean = 0)
       if(nwing_lab$year > 1985){
         xndg = -10
       }else{
         xndg = 10
       }
       
      }else{ #if not a zone-based plot
       
        nresp = data.frame(year = rep(as.integer(names(jdat$nhunter_y))[1],each = jdat$nhunter_y[1]),
                           nresp = rep(1,each = jdat$nhunter_y[1]))
        for(y in 2:length(jdat$nhunter_y)){
          tmp = data.frame(year = rep(as.integer(names(jdat$nhunter_y))[y],each = jdat$nhunter_y[y]),
                           nresp = rep(1,each = jdat$nhunter_y[y]))
          nresp <- bind_rows(nresp,tmp)
        }
        
        
        
        tt = table(nresp$year)
        tt = tt[which(tt > 1)]
        
        nwing_lab = data.frame(np = paste(min(tt),"parts in",names(tt)[which.min(tt)]),
                               year = as.integer(names(tt)[which.min(tt)]),
                               mean = 0)
        if(nwing_lab$year > 1985){
          xndg = -10
        }else{
          xndg = 10
        }
        
        
     }
     
     gp =  as.character(unique(dat[,"name"]))
     
 
  dat$mod <- factor(dat$model,levels = c("old","new"), ordered = T)
  
  source("Functions/palette.R")  
  

  
  if(labs_inc){
    lbs = dat[which(dat$model == "new" & dat$year == lbl_y[1] |
                      dat$model == "old" & dat$year == lbl_y[2]),]
    lbs$lbl <- paste(toupper(lbs$model),"model")
  }

      outgg = ggplot(data = dat,aes(x = year,y = mean,group = mod,fill = mod))+
        geom_dotplot(data = nwing,aes(x = year),inherit.aes = FALSE,binwidth = 1,colour = grey(0.5),fill = grey(0.5),alpha = 0.1,method = "histodot",dotsize = 0.2)+
        geom_point(aes(colour = mod),size = 0.5)+
        geom_line(aes(colour = mod))+
        ylab("")+
        labs(title = paste(p,"zone",z,sp))+
        geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
        my_col+
        geom_text_repel(data = nwing_lab,aes(x = year,y = mean,label = np),
                        inherit.aes = FALSE,nudge_y = max(dat$mean)*-0.03,nudge_x = xndg,
                        colour = grey(0.7),min.segment.length = 0,size = 3)+
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 9))
      
      if(labs_inc){
        outgg <- outgg+geom_text_repel(data = lbs,aes(label = lbl,colour = mod),
                                       nudge_y = max(dat$mean)*0.3,nudge_x = -4,min.segment.length = 0,size = 3)
      }
        
 
  return(outgg)
}


