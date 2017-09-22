
# Input. data
# output. data
# The function transforms integer value to numeric value. 




int_to_num=function(data){
  data=as.data.frame(data)
  library(magrittr)
  column_names=colnames(data)
  
  sapply(data,class)%>%table()%>%print
  int_index=sapply(data,class)=="integer"
  int_index=int_index%>%as.logical()
  
  int_features=data[,int_index]
  
  
  int_features=int_features%>%sapply(as.numeric)
  int_features=as.data.frame(int_features)
  
  data[,!int_index]%>%sapply(class)%>%table
  
  data=cbind(data[,!int_index],int_features)
  colnames(data)=column_names
  data%>%sapply(class)%>%table%>%print
  return(data)
  
}

