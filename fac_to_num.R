# convert factor variables to numerical

fac_to_num=function(data){
  
  data=data%>%select_fac()%>%sapply(as.integer)
  return(data)
  
}




