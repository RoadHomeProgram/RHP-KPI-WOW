library(lubridate)

#concatonates two strings together
"%&%" = function(a,b) paste0(a,b)

#identifies if a given vector is empty
isEmpty<-function(vec){
  return(sum(is.na(vec) | vec == "") == length(vec))
}

timestamp<-gsub(' ','_',now())