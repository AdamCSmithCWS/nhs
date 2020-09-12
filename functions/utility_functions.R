##3 utility functions

jags_dim <- function(dim = 1,
                     var = "",
                     cl = "Parameter",
                     dat = NULL){
  ##3 function to extract the indicator value of a multi-dimension jagsUI summary table
  require(stringr)
  

  pat = paste0("(?<=",var,"\\[")
  
  if(dim > 1){
  for(j in 1:(dim-1)){
      
    pat2 = paste0(pat,")[:digit:]+")
    cl2 = str_extract(dat[,cl],pattern = pat2)
    
    d = max(nchar(cl2))
  
    pat = paste0(pat,"[:digit:]{1,",d,"}[:punct:]")
  }
  }
 
  
  pat = paste0(pat,")[:digit:]+")
  dds = as.integer(str_extract(dat[,cl],pattern = pat))
  return(dds)
  
}

