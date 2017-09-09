

miss_pct_plot=function(data){
  
  
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
  
  # Plot the missing percentage
  library(ggplot2)
  plot=missing%>%ggplot(aes(x=reorder(names,-miss_pct),y=miss_pct))+
    geom_bar(stat = "identity",fill="blue")+
    theme(axis.text.x = element_text(angle = 60,hjust=1))+
    labs(x="Features with missing value", y="Percentage of missing value")
  
  return(plot)
}

# miss_pct_plot(boston_raw)

