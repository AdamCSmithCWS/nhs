### 


plot_sel_sp_caste <- function(dat = zone_caste,
                              v = "TODUK_Caste",
                              p = "SK",
                              z = 3,
                              c = "A",
                        spgp = "duck",
                        labs_inc = FALSE,
                        lbl_y = c(1990,1995)){
  
  
  pr = as.character(unique(dat[which(dat$prov == p),"province"]))

    dat <- filter(dat,
                  var %in% v,
                  province %in% pr,
                  zone %in% z,
                  caste %in% c,
                  year > 1975)
    
load(paste("data/data",p,z,spgp,"save.RData",sep = "_"))
  
  dat$mod <- factor(dat$model,levels = c("old","new"), ordered = T)
  
  wc <- which(levels(dat$caste) == c)
  
  
  source("Functions/palette.R")  
  
  
  nresp = data.frame(year = rep(as.integer(names(jdat$nhunter_y))[1],each = jdat$nhunter_cy[wc,1]),
                     nresp = rep(1,each = jdat$nhunter_cy[wc,1]))
for(y in 2:length(jdat$nhunter_y)){
  tmp = data.frame(year = rep(as.integer(names(jdat$nhunter_y))[y],each = jdat$nhunter_cy[wc,y]),
                     nresp = rep(1,each = jdat$nhunter_cy[wc,y]))
  nresp <- bind_rows(nresp,tmp)
}
 
  
  if(labs_inc){
    lbs = dat[which(dat$model == "new" & dat$year == lbl_y[1] |
                      dat$model == "old" & dat$year == lbl_y[2]),]
    lbs$lbl <- paste(toupper(lbs$model),"model")
  }

      outgg = ggplot(data = dat,aes(x = year,y = mean,group = mod,fill = mod))+
        geom_dotplot(data = nresp,aes(x = year),inherit.aes = FALSE,binwidth = 1,colour = grey(0.5),fill = grey(0.5),alpha = 0.1,method = "histodot",dotsize = 0.2)+
        geom_point(aes(colour = mod),size = 0.5)+
        geom_line(aes(colour = mod))+
        ylab("")+
        labs(title = paste(p,"zone",z,v,"caste",c))+
        geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
        my_col+
        theme_classic()+
        theme(legend.position = "none",
              title = element_text(size = 9))
      
      if(labs_inc){
        outgg <- outgg+geom_text_repel(data = lbs,aes(label = lbl,colour = mod),
                                       nudge_y = max(dat$mean)*0.3,nudge_x = -4,min.segment.length = 0,size = 3)
      }
        
 
  return(outgg)
}


