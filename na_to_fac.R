## Change NA values of the features into "None", a new factor value 

na_to_fac=function(data,names){
  # libraries (change it into installing it if not installed later)
  library(magrittr)
  library(dplyr)
  # Split the data into data with NA value and data with no NA value 
  data_na=data%>%select(names)
  data_nna=data%>%select(-one_of(names))
  data_na[is.na(data_na)]="None"
  
  data=cbind(data_na,data_nna)
  return(data)
  
}





#data.table::fread("train.csv")%>%na_to_fac(names=c("PoolQC","MiscFeature"))

