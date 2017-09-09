
miss_pct_df=function(data){
  
  # Define a function to calculate percentage of missing value in each feature
  miss_pct=function(data){
    pct=sum(is.na(data))/length(data)
  }
  
  # apply the function define above to every columns of the data set 
  missing=data%>%sapply(miss_pct)%>%as.data.frame()
  
  
  colnames(missing)="miss_pct"
  missing=data.frame(missing_pct=missing,names=row.names(missing)%>%as.character())
  
  # Select only the features with missing values and their missing value percentage
  missing=missing%>%filter(miss_pct>0)%>%arrange(desc(miss_pct))
  
  paste("There are ",  dim(missing)[1] ," features with missing values.",sep="")%>%print()

  
  return(missing)
  
}



