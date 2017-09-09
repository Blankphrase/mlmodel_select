# Find all factor variables 

select_fac=function(data){
  library(magrittr)
  data=as.data.frame(data)
  
  data%>%sapply(class)%>%table()%>%print
  fac_index=sapply(data,class)== "factor"
  fac_index=fac_index%>%as.logical()
  # data[,!int_index]%>%sapply(class)%>%table
  
  data=data[,fac_index]
  
  data%>%sapply(class)%>%table%>%print
  return(data)
}