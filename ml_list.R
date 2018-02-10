## Wrapper for auto-tune many ML algorithms supported by caret. 



## Load all the library dependencies here. 
library(foreach)
library(magrittr)
library(caret)
library(plyr);library(dplyr)
# another package glue is not loaded here. Since it is only used for collapse the preProcess parameter.
# source the timeRecord function dependency here
source("https://raw.githubusercontent.com/edwardcooper/mlmodel_select/master/timeRecord_functions.R")

## Goal: Auto-tune ml model with different sampling methods, different metric and etc.

## When using grid search, there will be N_hyper_params^tuneLength of the models being trained.
## When using random grid search, there will be tuneLength of models being trained, plus the eta is not set. 
## They are getting comparable results though. 
## Use Random whenever possible. 

ml_tune=function(data,target,sampling=NULL,metric="Accuracy",search = "random",k=10,tuneLength=2,repeats=1,method="xgbLinear",preProcess=NULL,summaryFunction=twoClassSummary,nthread=4){
  # load the machine learning library. 
  library(caret)
  # register parallel backend
  library(doParallel)
  
  
  # if the method name contains h2o then it is essential to initialize the h2o 
  if(grepl(pattern="h2o",method)){ 
    library(h2o)
    h2o.init(nthreads=nthread) }
  # set the number of cores to 1 for some algorithms. 
  if(method %in% c("OneR","LMT","mlpKerasDecay","mlpKerasDecayCost","mlpKerasDropout") ){
    nthread=1
  }
  cl=makeCluster(nthread)
  registerDoParallel(cl)
  
  # record the time
  timeRecordB()
  # change the trainControl for different metric. 
  switch(metric,
         Accuracy={
           ctrl_with_sampling<- trainControl(method = "repeatedcv",number = k, repeats = repeats,sampling = sampling,search=search)
         },Kappa={
           ctrl_with_sampling<- trainControl(method = "repeatedcv",number = k, repeats = repeats,sampling = sampling,search=search)
         },ROC={
           ctrl_with_sampling<- trainControl(method = "repeatedcv",number = k, repeats = repeats,sampling = sampling,search=search,classProbs = TRUE,summaryFunction = summaryFunction)
         },Sens={
           ctrl_with_sampling<- trainControl(method = "repeatedcv",number = k, repeats = repeats,sampling = sampling,search=search,classProbs = TRUE,summaryFunction = summaryFunction)
         },Spec={
           ctrl_with_sampling<- trainControl(method = "repeatedcv",number = k, repeats = repeats,sampling = sampling,search=search,classProbs = TRUE,summaryFunction = summaryFunction)
         }
  )
  
  # collapse the vector for preprocessing to a single character. 
  preProcess_message=glue::collapse(preProcess,sep=" ")
  # The output message paste together. 
  output_message=paste(method,sampling,metric,"tuneLength:",tuneLength,"search:",search,"preProcess:",preProcess_message,"cv_num:",k,"repeats:",repeats,sep=" ")
  #record the time use.
  timeRecordB()
  # train the function 
  # consider change the input into x and y in the future.
  ml_with_sampling_preprocess=train(  x=data[,colnames(data)!=target]
                                      , y=data[,colnames(data)==target]
                                      , method=method
                                      , metric=metric
                                      , trControl=ctrl_with_sampling
                                      , tuneLength=tuneLength,preProcess=preProcess)
 
  timeRecordB(output_message = output_message)
  

  # output the model that just finished training. 
  output_message%>%message()
  
  # wrap up the parallel connections. 
  if(grepl(pattern="h2o",method)){ 
   h2o.shutdown(prompt = FALSE) }
  stopCluster(cl)
  stopImplicitCluster()
  gc()
  return(ml_with_sampling_preprocess)
}

# example use of ml_tune.
# ml_tune(data=train_data,method="rf",target = "is_open")




# ### A function to auto-train and store models into a list. 

ml_list=function(data,target,params,summaryFunction=twoClassSummary,save_model=NULL){
  timeRecordB()
  # print the total numbers of models to be trained.
  print(paste("Total training model(s):",sum(params[,"tuneLength"]),sep=" " ))
  params%>%print()
  library(foreach)
  library(magrittr)
  
  
  # just do not add the .combine=list sovles the strange list structure.
  # remove the output if there is an error in training the model. .errorhandling = "remove". 
  # See https://cran.r-project.org/web/packages/foreach/foreach.pdf for details. 
  model_list=foreach(i=1:nrow(params),.packages = c("caret","magrittr"),.errorhandling = "remove")%do%{
    
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
    
    # give nthread a number of 4 if not specified. Since most desktop or more powerful laptops have four cores. 
    if("nthread" %in% colnames(params)){
      nthread=params[i,"nthread"]%>%as.numeric()
    }else{
      nthread=4
    }
    # give repeats a default number 1 if not specified. 
    if("repeats" %in% colnames(params)){
      repeats=params[i,"repeats"]%>%as.numeric()
    }else{
      repeats=1
    }
    method=params[i,"method"]%>%as.character()
    search=params[i,"search"]%>%as.character()
    tuneLength=params[i,"tuneLength"]%>%as.character() 
    metric=params[i,"metric"]%>%as.character()
    k=params[i,"k"]%>%as.numeric()
    
    
    # model training part.
    # add tryCatch for error handling. 
    
    ml_model_train=ml_tune(data=data,target=target,sampling=sampling,preProcess=preProcess
                              ,metric=metric
                              ,tuneLength=tuneLength
                              ,search=search
                              ,repeats = repeats
                              ,k=k
                              ,nthread=nthread
                              ,method=method
                              ,summaryFunction = summaryFunction)
    
    
    #paste(method,metric,tuneLength,search,sampling,preProcess,sep=" ")%>%message()
    #print the number of models that have been trained.
    paste("Finished training: ",i,"/",nrow(params),sep="")%>%message()
    
    
    # save each model to disk.
    # ======================================
    # change the preprocess vector into a single chr. 
    preProcess_message=glue::collapse(preProcess,sep="_")
    file_name=paste(i,method,sampling,preProcess_message,metric,sep="_")
    # if the save_model is not null, then save each model 
    if(!is.null(save_model)){ 
      # Use the save_model string as the name for the subdirectory to store each models.
      dir_path=paste("./",save_model,sep="")
      # If the dir_path does not exist, create a subdirectory. 
      if(!dir.exists(dir_path)){  dir.create(dir_path)  }
      # save the models to that subdirectory with file_name. 
      file_name=paste(dir_path,"/",file_name,sep="")
      # save the models 
      saveRDS(ml_model_train,file=paste(file_name,".rds",sep="")) 
    }
    # ====================================================
    return(ml_model_train)
  }
  
  # finally save the entire model list to disk. 
  if(!is.null(save_model)){ saveRDS(model_list,file=paste(save_model,".rds",sep="") ) }
  
  return(model_list)
}




## test ml_list function 
# 
# params_grid=expand.grid(sampling=c("up","down")
#                         ,metric=c("ROC","Accuracy","Kappa","Sens","Spec")
#                         ,preProcess=list(c("zv","nzv","center","scale"),c("center","scale"))
#                         ,method=c("glmnet","glm","bayesglm")
#                         ,search="random"
#                         ,tuneLength=10
#                         ,k=10,nthread=4)
# 
# ml_list(data=train_data,target = "is_open",params = params_grid,summaryFunction=twoClassSummary,save_model="test_models")

########################################################################################################################33

# Functions to do model selection

## plot the model performance for models with different metrics. 
ml_bwplot=function(models,metric=NULL){
  # get the metrics from the caret model list. 
  model_metrics=models%>%lapply( function(model_list){model_list$metric})%>%unlist
  # plot the model performance for metric-group ROC,Sens,Spec and/or Accuracy, Kappa. 
  # decide the number of plots a.k.a groups
  # if there are two groups then find their indexes and get the models for differnt group. 
  # if there is one group, then just plot it. 
  if(c("ROC","Sens","Spec") %in% model_metrics && c("Accuracy","Kappa") %in% model_metrics){
    # if the model selection does not depend on model correlation. a.k.a the cor_level is null. 
    if(is.null(metric)){
      models[model_metrics %in% c("ROC","Sens","Spec")]%>%resamples%>%bwplot%>%print
      
      models[model_metrics %in% c("Accuracy","Kappa")]%>%resamples%>%bwplot%>%print
    }else{
      models[model_metrics %in% c("ROC","Sens","Spec")]%>%resamples%>%bwplot(metric=metric)%>%print
      
      models[model_metrics %in% c("Accuracy","Kappa")]%>%resamples%>%bwplot(metric=metric)%>%print
    }
    
  }else{
    if(is.null(metric)){
      models%>%resamples%>%bwplot%>%print
    }else{
      models%>%resamples%>%bwplot(metric=metric)%>%print
    }
  }
  return(models)
}

# example use of ml_bwplot
# ml_bwplot(testmodels_churn)

#==============================================================================================================================================================


# A function to filter models based on performance metrics from cross-validation. 
ml_cv_filter=function(models,metric="ROC",mini=NULL,max=NULL,FUN=median){
  # get all the metrics in the model list
  model_metrics=models%>%lapply( function(model_list){model_list$metric})%>%unlist
  # if the metric is in the model_metrics, then select the models based on minimum value of the metric.
  # if(!metric %in% model_metrics){paste("There is no metric, ",metric,", in the model performance. "
  #                                     ,"Try using metrics:",glue::collapse(unique(model_metrics),sep=", ") )%>%stop() }
  
  
  # ==================================
  # define a function to filter models. 
  filter_model=function(model,metric,FUN,mini=NULL,max=NULL){
    # model%>%resamples%>%.$values%>%print
    if(!is.null(FUN)){
      # if the FUN variable is not null, then we will use that function to apply to the performance metrics dataframe. 
      # resample the models, select the performance metrics dataframe from the object,
      # then select the columns that contains the metric you selected in the function options.
      # then apply the function FUN to the list, and unlist the values and turn the vector into a TRUE or FALSE vector to select models in filtered_models.
      
      # value_for_switch is 1 when only mini variable is not null
      # value_for_switch is 2 when both mini and max variable are not null.
      # value_for_switch is 3 when only max variable is not null. 
      value_for_switch=as.numeric(is.null(mini))+as.numeric(!is.null(max))+1
      switch (value_for_switch
              ,{
                # for using only mini
                print("using mini")
                filter_values=(model%>%resamples%>%.$values%>%select(contains(metric))%>%apply(2,FUN)%>%unlist)>mini
              }
              ,{
                # for using max and mini
                print("using max and mini")
                filter_values_mini=(model%>%resamples%>%.$values%>%select(contains(metric))%>%apply(2,FUN)%>%unlist)>mini
                filter_values_max=(model%>%resamples%>%.$values%>%select(contains(metric))%>%apply(2,FUN)%>%unlist)<max
                filter_values=(filter_values_max & filter_values_mini)
              }
              ,{
                # for using only max
                print("using max")
                filter_values=(model%>%resamples%>%.$values%>%select(contains(metric))%>%apply(2,FUN)%>%unlist)<max
              }
      )
      model%>%resamples%>%.$values%>%select(contains(metric))%>%apply(2,FUN)%>%unlist%>%print
      filter_values%>%print
      filtered_model_from_metric=model[filter_values]
    }else{
      message("please give FUN variable a value like min or median")
    }
    
    return(filtered_model_from_metric)
  }
  # ==================================
  
  # select models based on which metric group the metric is in.  
  # if ROC is in that list, then it will return the second option.
  # if ROC is not in that list, then it will return the first option. a.k.a kappa and accuracy models
  switch(as.numeric(metric %in% c("ROC","Sens","Spec"))+1
         ,{
           filtered_models=models[model_metrics %in% c("Accuracy","Kappa")]%>%filter_model(metric=metric,FUN=FUN,mini=mini,max=max)
         }
         ,{
           filtered_models=models[model_metrics %in% c("ROC","Sens","Spec")]%>%filter_model(metric=metric,FUN=FUN,mini=mini,max=max)
         }
  )
  
  return(filtered_models)
}



# the supported metrics are ROC,Sens,Spec,Accuracy,Kappa. 
# example use to select models based on metric. 
# this select models that has a cv minimum of 0.75 median accuracy 
# testmodels_metric_filtered=testmodels_churn%>%ml_cv_filter(metric="Accuracy",mini=0.75,FUN=median)
# this select models that has a cv minimum accuracy of 0.75
# testmodels_metric_filtered=testmodels_churn%>%ml_cv_filter(metric="Accuracy",mini=0.75,FUN=min)
# this select models that has a cv minimum standard deviation of 0.01
# testmodels_metric_filtered=testmodels_churn%>%ml_cv_filter(metric="Accuracy",mini=0.75,FUN=sd)
# select models that has a cv minimum ROC median of 0.84 and a maximum ROC standard deviation of 0.017
# testmodels_metric_filtered=testmodels_churn%>%ml_cv_filter(metric="ROC",mini=0.84,FUN=median)%>%ml_cv_filter(metric="ROC",max=0.017,FUN=sd)
# select models that has a cv median ROC value between 0.84 and 0.84275. 
# testmodels_metric_filtered=testmodels_churn%>%filter_model(metric="ROC",mini=0.84,max=0.84275,FUN=median)
# you could use custom functions to calculate a statistic for a k-fold performance metric
# This function used the performance metrics after feed the model into resamples function in caret package. 
# You could get the same dataframe with model_list%>%resamples%>%.$values.


#==============================================================================================================================================================

# A function to remove models based on inter-model correlation. Mainly based on modelCor function in caret. 
# This function will also remove models that have NA value in resampled performance. a.k.a NA value in modelCor function output.
# get rid of the missing values
# select models based on cor_level. Get rid of the models with high correlation.
## Wrapper for auto-tune many ML algorithms supported by caret. 


# issue a warning if the model list in any step is empty. 
ml_cor_filter=function(models,cor_level=0.9){
  # a function to remove models that have NA performance value after resample. a.k.a. modelCor function produces NA values. 
  
  remove_models=function(models,cor_level){
    
    # suppress the warning from the modelCor, since we are trying to get rid of all models that have NA correlation value in modelCor.
    suppressWarnings(na_index<-(models%>%resamples%>%modelCor%>%is.na%>%apply(2,sum))==length(models[-1]))
    
    non_na_models=models[!na_index]
    paste("Number of NA model(s) removed:",(length(models)-length(non_na_models)) )%>%message
    
    high_cor_index=non_na_models%>%resamples%>%modelCor%>%findCorrelation(cutoff = cor_level)
    # the all models have a correlation level below cor_level, then you need to return the original list of models.  
    if(!length(high_cor_index) ){
      low_cor_models=non_na_models
    }else{
      low_cor_models=non_na_models[-high_cor_index]
    }
    
    paste("Number of high correlation model(s) removed:",(length(non_na_models)-length(low_cor_models)) )%>%message
    
    return(low_cor_models)
  }  
  
  # get the metrics from the caret model list. 
  model_metrics=models%>%lapply( function(model_list){model_list$metric})%>%unlist
  # plot the model performance for metric-group ROC,Sens,Spec and/or Accuracy, Kappa. 
  # decide the number of plots a.k.a groups
  # if there are two groups then find their indexes and get the models for differnt group. 
  # if there is one group, then just plot it. 
  if(c("ROC","Sens","Spec") %in% model_metrics && c("Accuracy","Kappa") %in% model_metrics){
    message("For ROC, Sens, and Spec metrics:")
    roc_models=models[model_metrics %in% c("ROC","Sens","Spec")]
    cleaned_roc_models=roc_models%>%remove_models(cor_level = cor_level)
    
    
    message("For Accuracy, Kappa metrics:")
    accu_models=models[model_metrics %in% c("Accuracy","Kappa")]
    cleaned_accu_models=accu_models%>%remove_models(cor_level = cor_level)
    
    cleaned_models=c(cleaned_roc_models,cleaned_accu_models)
  }else{
    cleaned_models=models%>%remove_models(cor_level = cor_level)
  }
  
  
  return (cleaned_models)
}

# example use 
# a maximum of correlation 0.9 between models. 
# low_cor_models_churn=testmodels_churn%>%ml_cor_filter(cor_level = 0.9)
# a maximum of correlation 0.7 between models. 
# low_cor_models_churn=testmodels_churn%>%ml_cor_filter(cor_level = 0.7)




#########################################################################################################################

#### The function below aims to make model stacking easier with pipeline by using the prediction matrix. 

prediction_matrix=function(base_model,data,target){
  # predict on the data provided. 
  base_prediction=foreach(j=1:length(base_model),.combine = cbind)%do%{
    result_predictions=base_model[[j]]%>%predict(data)
    return( result_predictions )
  }
  # change the matrix into data frame to avoid data type matching. 
  base_prediction=as.data.frame(base_prediction)
  base_prediction=base_prediction%>%apply(2,as.double)%>%as.data.frame()
  # combine the true label from the data
  base_prediction=cbind(base_prediction, true_label = data[,colnames(data)==target] )
  
  
  
  # base_prediction$true_label=as.factor(base_prediction$true_label)
  # base_prediction=as.data.frame(base_prediction)
  # print the summary of the prediction data. 
  base_prediction%>%sapply(class)%>%print() # This is the prediction data frame we get.
  return(base_prediction)
}


##################################################################################################################################
# function to check and install packages. 

# check and install missing package dependencies from a vector of model names.  
install_pkg_model_names=function(model_names){
  
  model_names=unique(model_names)
  pkg_names=foreach::foreach(i=seq_along(model_names),.combine=c)%do%{
    return(caret::getModelInfo()[[model_names[i]]]$library)
  }
  
  # check for the difference between required package and installed packages. 
  missing_pkgs=setdiff(unique(pkg_names),rownames(installed.packages()))
  if( length( missing_pkgs )>0    ){
    message(paste("Trying to install packages:",missing_pkgs,"\n"))
    if("bnclassify" %in% missing_pkgs){
      # need to install some bioconductor dependencies. 
      source("http://bioconductor.org/biocLite.R")
      biocLite(c("graph", "RBGL", "Rgraphviz"))
    }
    install.packages(missing_pkgs)
  }else{
    message("No missing package dependency.")
  }
}

# example use 
# install_pkg_model_names(c("xgbTree","deepboost"))
# install_pkg_model_names(params_grid$method)

# check and install missing pacakge dependencies from a list of trained caret models. 

install_pkg_model_list=function(models){
  model_libs=foreach::foreach(i=seq_along(models),.combine = c)%do%{
    return(models[[i]]$modelInfo$library)
  }
  install_pkg_model_names(model_libs)
}


# install_pkg_model_list(models=down_sampling_models)

# give models a meaningful names for comparison. 
assign_model_names=function(models){
  model_names=foreach::foreach(i=seq_along(models))%do%{
    method=models[[i]]$method
    metric=models[[i]]$metric
    preProcess=models[[i]]$preProcess$method%>%names
    sampling=models[[i]]$control$sampling$name
    preProcess=glue::collapse(preProcess,sep="_")
    model_name=paste(i,method,sampling,preProcess,metric,sep="_")
    return(model_name)
  }
  names(models)=model_names
  return(models)
}


# example use 
# down_sampling_models= assign_model_names(down_sampling_models)



model_list_load=function(path){
  current_path=getwd()
  setwd(path)
  path_command=paste("cd ",path,";ls")
  file_names=system(path_command,intern = TRUE)
  message(paste("Loading "),length(file_names)," models.")
  library(foreach)
  model_list=foreach(ih=seq_along(file_names))%do%{
    model=readRDS(file=file_names[i])
    message(paste("Finished loading model:",file_names[i],"\n",i,"/",length(file_names)))
    return(model)
  }
  
  setwd(current_path)
  return(model_list)

  }


# example use
# models=model_list_load(path="~/Dropbox/churn/down_sampling")
