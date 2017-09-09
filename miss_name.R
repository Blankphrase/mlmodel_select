## A function to get the names and percentile of a number of features with missing values 

miss_name=function(data){
  # Define a function to calculate percentage of missing value in each feature
  miss_pct=function(data){
    pct=sum(is.na(data))/length(data)
  }
  
  # apply the function define above to every columns of the data set 
  missing=data%>%sapply(miss_pct)%>%as.data.frame()
  
  
  colnames(missing)="miss_pct"
  missing=data.frame(names=row.names(missing)%>%as.character(),missing_pct=missing)
  
  # Select only the features with missing values and their missing value percentage
  missing=missing%>%filter(miss_pct>0)%>%arrange(desc(miss_pct))
  
  
  return(missing)
}