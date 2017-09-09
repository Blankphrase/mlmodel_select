
# A function to calculate correlation for factor and numerical values.
# It will also change character values into factor values to calculate correlation and throw out any feature with missing value. 


correlation=function(data){
  
  ## Define fucntions: select_num, select_fac, select_comp, char_to_factor
  
  ############################################################
  
  
  # function for selecting only numerical variables 
  select_num=function(data){
    library(magrittr)
    data=as.data.frame(data)
    
    #data%>%sapply(class)%>%table()%>%print
    int_index=sapply(data,class)== "integer"
    int_index=int_index%>%as.logical()
    num_index=sapply(data,class)== "numeric"
    num_index=num_index%>%as.logical()
    # data[,!int_index]%>%sapply(class)%>%table
    
    data1=data[,int_index]
    data2=data[,num_index]
    data=cbind(data1,data2)
    #data%>%sapply(class)%>%table%>%print
    return(data)
  }
  
  ############################################################
  
  
  
  # select variables with no missing values 
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
  
  
  ############################################################
  
  # convert character to factor value 
  char_to_factor=function(data){
  data=as.data.frame(data)
  library(tidyverse)
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
  
  ############################################################
  
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
  
  
  
  #############################################################
  
  
  
  
  
  # Convert character to factor, select variables with no missing value by setting p=0, select only numerical variables 
  data1=data%>%char_to_factor()%>%select_comp(p=0)%>%select_num()
  # Convert character to factor, select variables with no missing value by setting p=0, select only factor variables, and convert it into integer values 
  data2=data%>%char_to_factor()%>%select_comp(p=0)%>%select_fac()%>%sapply(as.integer)
  # combine the numerical and factor variables 
  data=cbind(data1,data2)
  # calculate correlations 
  correlation=cor(data)
  #print("contains only numeric or interger for correlation calculation")
  # Return the correlation data 
  return(correlation)
}


## What if there is no factor variable or there is no numerical variable 
## How to select most correlated variables 
## add an echo variable 

