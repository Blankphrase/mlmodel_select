## Wrapper for auto-tune many ML algorithms supported by caret. 



## Load all the library dependencies here. 
library(foreach)
library(magrittr)
library(caret)
library(plyr)
library(dplyr)
# another package glue is not loaded here. Since it is only used for collapse the preProcess parameter.
# source the timeRecord function dependency here
source("https://raw.githubusercontent.com/edwardcooper/mlmodel_select/master/timeRecord_functions.R")



############################

# library(doParallel)
# if(detectCores()==4){core_num=detectCores()}else(core_num=detectCores()/2)
# cluster_name<<-makeCluster(core_num)
# registerDoParallel(cluster_name)       
# 
# 
# 
# stopCluster(cluster_name)
# rm(cluster_name)
# stopImplicitCluster()
# gc()

####################################################################################

## Goal: Auto-tune xgbTree model with different sampling methods and different metric. 

## When using grid search, there will be 3^tuneLength of the models being trained, plus eta is set to be 0.3
## When using random grid search, there will be tuneLength of models being trained, plus the eta is not set. 
## They are getting comparable results though. 
## Use Random whenever possible. 

ml_tune=function(data,target,sampling=NULL,metric="Accuracy",search = "random",k=10,tuneLength=2,method="xgbLinear",preProcess=NULL,summaryFunction=twoClassSummary){
  library(caret)
  # record the time
  timeRecordB()
  # change the trainControl for different metric. 
  switch(metric,
         Accuracy={
           ctrl_with_sampling<- trainControl(method = "repeatedcv",number = k, repeats = 1,sampling = sampling,search=search)
         },Kappa={
           ctrl_with_sampling<- trainControl(method = "repeatedcv",number = k, repeats = 1,sampling = sampling,search=search)
         },ROC={
           ctrl_with_sampling<- trainControl(method = "repeatedcv",number = k, repeats = 1,sampling = sampling,search=search,classProbs = TRUE,summaryFunction = summaryFunction)
         },Sens={
           ctrl_with_sampling<- trainControl(method = "repeatedcv",number = k, repeats = 1,sampling = sampling,search=search,classProbs = TRUE,summaryFunction = summaryFunction)
         },Spec={
           ctrl_with_sampling<- trainControl(method = "repeatedcv",number = k, repeats = 1,sampling = sampling,search=search,classProbs = TRUE,summaryFunction = summaryFunction)
         }
  )
  
  # random tuning grid     
  # xgbTree_tune_grid=
  
  # train the function 
  # consider change the input into x and y in the future.
  ml_with_sampling_preprocess=train(  x=data[,colnames(train_data)!=target]
                                      , y=data[,colnames(train_data)==target]
                                      , method=method
                                      , metric=metric
                                      , trControl=ctrl_with_sampling
                                      , tuneLength=tuneLength,preProcess=preProcess)
  # collapse the vector for preprocessing to a single character. 
  preProcess=glue::collapse(preProcess,sep=" ")
  # The output message paste together. 
  output_message=paste(method,sampling,metric,"tuneLength:",tuneLength,search,preProcess,"cv_num:",k,sep=" ")
  # output the model that just finished training. 
  output_message%>%message()
  #record the time use. 
  timeRecordB(output_message = output_message)
  gc()
  return(ml_with_sampling_preprocess)
}

# example use of ml_tune.
# ml_tune(data=train_data,target = "is_open")


# Add error handling to function ml_tune

ml_tune_tc=function(data,target,sampling=NULL,metric="Accuracy",search = "random",k=10,tuneLength=2,method="xgbLinear",preProcess=NULL,summaryFunction=twoClassSummary){
  out=tryCatch(
    ml_tune(data=data,target=target,sampling=sampling,preProcess=preProcess
            ,metric=metric
            ,tuneLength=tuneLength
            ,search=search
            ,method=method
            ,summaryFunction=summaryFunction
            ,k=k
    )
    
    ,error=function(e){
      #echo the error message 
      message(e)
      # echo the specific model
      message(paste(method,sampling,metric,tuneLength,search,preProcess,sep=" "))
      return(NA)
    }
    
  )
  return(out)
}

# train_data%>%ml_tune_tc(target="is_open")



# ### A function to auto-train and store models into a list. 
# 
ml_list=function(data,target,params,summaryFunction=twoClassSummary){
  timeRecordB()
  # print the total numbers of models to be trained.
  print(paste("Total training model(s):",sum(params[,"tuneLength"]),sep=" " ))
  params%>%print()
  library(foreach)
  library(magrittr)
  
  
  # just do not add the .combine=list sovles the strange list structure.
  model_list=foreach(i=1:nrow(params),.packages = c("caret","magrittr"))%do%{
    
    ### If there is sampling information in the params then give sampling that value, if sampling has a NULL character value, give it a NULL.
    if("sampling" %in% colnames(params) ){
      sampling=params[i,"sampling"]%>%as.character()
      if(sampling=="NULL"){sampling=NULL}
    }else{sampling=NULL}
    # Do the same for preprocess variable.
    if("preProcess" %in% colnames(params) ){
      preProcess=params[i,"preProcess"]%>%.[[1]]
      if(preProcess[1]=="NULL"){preProcess=NULL}
    }else{preProcess=NULL}
    
    method=params[i,"method"]%>%as.character()
    search=params[i,"search"]%>%as.character()
    tuneLength=params[i,"tuneLength"]%>%as.character() 
    metric=params[i,"metric"]%>%as.character()
    k=params[i,"k"]%>%as.numeric()
    # model training part.
    # add tryCatch for error handling. 
    
    ml_model_train=ml_tune_tc(data=data,target=target,sampling=sampling,preProcess=preProcess
                              ,metric=metric
                              ,tuneLength=tuneLength
                              ,search=search
                              ,k=k
                              ,method=method
                              ,summaryFunction = summaryFunction)
    
    
    #paste(method,metric,tuneLength,search,sampling,preProcess,sep=" ")%>%message()
    #print the number of models that have been trained.
    paste("Finished training: ",i,"/",nrow(params),sep="")%>%message()
    #save the model to the file.
    
    
    
    return(ml_model_train)
  }
  
  return(model_list)
}




## test ml_list function 
# 
# params_grid=expand.grid(sampling=c("up","down")
#                         ,metric=c("ROC")
#                         ,method=c("xgbLinear")
#                         ,search="random"
#                         ,tuneLength=1
#                         ,k=10)
# 
# ml_list(data=train_data,target = "is_open",params = params_grid,summaryFunction=twoClassSummary)
