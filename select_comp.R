




# Define a function to filter out variables with missing value more than p, which has a default value of 50%. 


## The function name is short for complete data with percentile p
select_comp=function(data,p=0.5){
  # Define a function to calculate percentage of missing value in each feature
  miss_pct=function(data){
    pct=sum(is.na(data))/length(data)
  }
  
  # apply the function define above to every columns of the data set 
  missing=sapply(data,miss_pct)%>%as.data.frame()
  
  names=row.names(missing)
  colnames(missing)="miss_pct"
  missing=data.frame(missing_pct=missing,names=row.names(missing)%>%as.character())
  
  # Select only the features with missing values and their missing value percentage
  missing=missing%>%filter(miss_pct>p)%>%arrange(desc(miss_pct))
  
  
  
  names=missing$names%>%as.character()
  
  data=data%>%select(-one_of(names) )
  return(data)
}
