select_num=function(data){
  library(magrittr)
  data=as.data.frame(data)
  
  data%>%sapply(class)%>%table()%>%print
  int_index=sapply(data,class)== "integer"
  int_index=int_index%>%as.logical()
  num_index=sapply(data,class)== "numeric"
  num_index=num_index%>%as.logical()
  # data[,!int_index]%>%sapply(class)%>%table
  
  data1=data[,int_index]
  data2=data[,num_index]
  data=cbind(data1,data2)
  data%>%sapply(class)%>%table%>%print
  return(data)
}


