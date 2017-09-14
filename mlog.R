### Define some functions to use when we are doing ML spot check. Also prepare for easier stacking in the next step. 


### Need to save 1.predictions on training data 2.performance metrics on test data 3. time used in this model 4. model name + datasetname

## Need to have a list of model,predictions,time used, modelname+preprocessing information.  Extract from that list all information but the first. 


## There are basically two steps in this. 
## create a log file in the working environment to add all information to. 
library(foreach)
library(magrittr)

# input value is the name for the dataset:char value

# the name is short for machine learning log initialized. (learn from h2o.init())
mlog_init=function(name="ML"){
  
  
  ## create a log file to add all above information to if the file does not exist
  
  # create a file name based on dataset name to avoid possible overlap
  
  file_name=paste(name,format(Sys.time(), "%F_%T"),".log",sep="")
  
  file.create(file_name)
  
  # make the insanely long name a global variable. Different from the one I used in timeRecord functions. 
  extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use_for_ml_information_recording<<-file_name
  
  if(file.exists(file_name)){
    
    print("Fun time Machine learning log has been created")
    
  }else{
    print("unsuccessful in creating a time log file")
  }
  
}

# There are going to be issues if you are working on different datasets in the same working environment. It will not distinguish the prediction information from different datasets. 
# But if you are going to combine results from different dataset predictions, that is fine. 
####################################################################################################################
####################################################################################################################

# Necessary ml information to the log. 
# Must be used after creating the log file with mlog_init()

# The name is short for machine learning log write. 
mlog_w=function(ml_list,saveModel=FALSE){
  
  
  # save the model to the working directory if savemModel is TRUE, but the default is set to FALSE. 
  # Since an easier way to save models is to save the working environment to Rdata. Saves the code for loading.
  if(saveModel){
    saveRDS(ml_list$model,file=paste(ml_list$name,".rds",sep=""))
  }else{
    print("Did not save the model to log, only the predictions.")
  }
  
  
  # Append to file the current log file if the file is already there.  
  if(file.exists(extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use_for_ml_information_recording)){
    file_name=extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use_for_ml_information_recording
   
    values_write_to_log=c(method=ml_list$name,preProcess=ml_list$preProcess
                          ,performance_train=ml_list$performance_train,performance_test=ml_list$performance_test,
                          ml_list$elapsed
                          ,pred_train=ml_list$pred_train
                          ,pred_test=ml_list$pred_test
                          )
   #log_values<<-values_write_to_log
    write.table( values_write_to_log , file=file_name , append=TRUE)
  }else{
    print("Please use function mlog_init() to create a file first.")
  }
  return(ml_list)
}

######### A function to write target variable into the log file.


mlog_wtarget=function(data,seed=10,p=0.85,name=extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use_for_ml_information_recording){
  
  
  # record the time when this function begins. 
  t1=proc.time()
  # make all column names legit
  colnames(data)=make.names(colnames(data))
  # set a seed for reproducibility. 
  set.seed(seed)
  seed<<-seed
  # split the data
  index=createDataPartition(data$SalePrice,p=p,list=FALSE)
  train=data[index,]
  # train_y=train$SalePrice%>%as.vector()%>%as.numeric()%>%log1p()
  # train_x=train%>%select(-SalePrice)
  test=data[-index,]
  
  # Append to file the current log file if the file is already there.  
  if( file.exists(extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use_for_ml_information_recording) ){  
    
    file_name=extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use_for_ml_information_recording
    
    t2=proc.time()
    
    time_elapsed=(t2-t1)/60
    values_write_to_log=c(method="target",preProcess="None"
                          ,performance_train=0,performance_test=0
                          ,time_elapsed
                          ,pred_train=log1p(train$SalePrice)
                          ,pred_test=log1p(test$SalePrice)
    )
    #log_values<<-values_write_to_log
    write.table( values_write_to_log , file=file_name , append=TRUE)
  }else{
    print("Please use function mlog_init() to create a file first.")
  }
  
  
}




#####################################################################

## A function to read in the data and clean it into easy to use and read format. 
########## how to read the data into R and convert it into a cleaned format. 
mlog_r=function(name=extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use_for_ml_information_recording){
  
  # read in the data
  dataset=read.table(file=name,skip=1,na.strings = "x",fill=TRUE,stringsAsFactors=FALSE)
  
  # remove the NA value 
  dataset=dataset[complete.cases(dataset),]
  
  
  ## The data read in needs more cleaning process. 
  
  # What kind of format do we want? Just place the data horizontally. 
  
  #find the number of models calculated 
  num_models=(dataset[,1]=="method")%>%sum
  
  # calculate the length of each model information. 
  length_of_record=dim(dataset)[1]/num_models
  
  # Need to check that length_of_record is not zero. 
  
  if(length_of_record!=0){
    
    # for all models, only the second column in the data is enough. 
    library(foreach)
    dataset2=foreach(i=1:num_models,.combine = cbind)%do%{
      ml_information=dataset[(1+(i-1)*length_of_record):(i*length_of_record),2]
      return(ml_information)
    }
    
    # Give the names in the first column to the new cleaned version of dataset as row.names
    row.names(dataset2)=dataset[1:length_of_record,1]
    
    
    
  }else if(length_of_record==0){
    print("No machine learning algorithm information stored in it.")
  }else{
    print("No valid value for length_of_record. Maybe there is no dataset loaded.")
  }
  
  return(dataset2)
}

# Define two differnt functions to extract predictions for train and test data

# these two functions depends on mlog_r() function. 
# They take name of the file as input. And return the prediction for train or test data. 

mlog_rtrain=function(name=extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use_for_ml_information_recording){
  # read in data use mlog_r() function as convert it to dataframe to keep data structure consistent. 
  dataset=mlog_r(name=name)%>%as.data.frame()
  # find the index that contains pred_test, which is the predictions for train data. 
  index_train=grep("pred_train",row.names(dataset))
  # find the data type of each column
  dataset%>%apply(2,class)%>%print()
  # change the character into numeric values. 
  data_train=dataset[index_train,]%>%apply(2,as.numeric)
  # test to see if all variables have been converted to numeric. 
  data_train%>%apply(2,class)%>%print()
  # add column names for the train data predictions 
  column_names=dataset[1,]%>%as.matrix()%>%as.vector()
  colnames(data_train)=column_names
  return(data_train)
}


mlog_rtest=function(name=extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use_for_ml_information_recording){
  # read in data use mlog_r() function as convert it to dataframe to keep data structure consistent. 
  dataset=mlog_r(name=name)%>%as.data.frame()
  # find the index that contains pred_test, which is the predictions for test data. 
  index_test=grep("pred_test",row.names(dataset))
  # find the data type of each column
  dataset%>%apply(2,class)%>%print()
  # change the character into numeric values. 
  data_test=dataset[index_test,]%>%apply(2,as.numeric)
  # test to see if all variables have been converted to numeric. 
  data_test%>%apply(2,class)%>%print()
  # add column names for the test data predictions 
  column_names=dataset[1,]%>%as.matrix()%>%as.vector()
  colnames(data_test)=column_names
  return(data_test)
}


# a function to extract method,proProcess, time,train data performance and test data performance function. 

mlog_rsummary=function(name=extremely_long_name_i_do_not_think_anyone_would_be_sanely_to_use_for_ml_information_recording){
  
  dataset=mlog_r(name=name)
  dataset=dataset[c(1,2,3,4,7),]
  return(dataset)
}

# there is definitely better ways to write this. But I just like it this way. It is easier to maintain several functions than one extremely long function. 
# Plus, it is easier to remeber the name this way. 
################################################################################################################################