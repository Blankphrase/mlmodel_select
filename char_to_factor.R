char_to_factor=function(data){
  library(magrittr)
  data=as.data.frame(data)
  
  sapply(data,class)%>%table()%>%print
  char_index=sapply(data,class)=="character"
  char_index=char_index%>%as.logical()
  
  char_features=data[,char_index]
  
  
  char_features=char_features%>%sapply(as.factor)
  char_features=as.data.frame(char_features)
  
  data[,!char_index]%>%sapply(class)%>%table
  
  data=cbind(data[,!char_index],char_features)
  
  data%>%sapply(class)%>%table%>%print
  return(data)
  
}

# Return original dataset if there is no character variables in the original dataset. 
