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
  # register parallel backend package
  library(doParallel)
  
  
  # if the method name contains h2o then it is essential to initialize the h2o 
  if(grepl(pattern="h2o",method)){ 
    library(h2o)
    h2o.init(nthreads=nthread) }
  
  # set the number of cores to 1 for some algorithms. 
  if(method %in% c("OneR","LMT","mlpKerasDecay","mlpKerasDecayCost","mlpKerasDropout") ){
    nthread=1
  }
  # register the backend
  cl=makeCluster(nthread)
  registerDoParallel(cl)
  
  
  # implement different sampling methods. Only invoke the switch below if the sampling method is in the vector  c("ADAS","ANS","BLSMOTE","DBSMOTE","RSLS","SLS")
  if(sampling %in% c("ADAS","ANS","BLSMOTE","DBSMOTE","RSLS","SLS")){
    switch(sampling,
           ADAS={sampling <- list(name = "ADAS",
                                  func = function (x, y) {
                                    
                                    library(smotefamily)
                                    library(FNN)
                                    dat <- if (is.data.frame(x)) x else as.data.frame(x)
                                    
                                    dat$.y <- y
                                    
                                    dat <- ADAS(X=dat[, !grepl(".y", colnames(dat), fixed = TRUE)],target=dat$.y)
                                    dat=dat$data
                                    list(x = dat[,!colnames(dat)=="class"], 
                                         y = as.factor(dat[,colnames(dat)=="class"]))
                                    
                                  }
                                  ,first = TRUE)
           
           },
           ANS={
             sampling <- list(name = "ANS",
                              func = function (x, y) {
                                
                                library(smotefamily)
                                library(FNN)
                                dat <- if (is.data.frame(x)) x else as.data.frame(x)
                                
                                dat$.y <- y
                                
                                dat <- ANS(X=dat[, !grepl(".y", colnames(dat), fixed = TRUE)],target=dat$.y)
                                dat=dat$data
                                list(x = dat[,!colnames(dat)=="class"], 
                                     y = as.factor(dat[,colnames(dat)=="class"]))
                                
                              }
                              ,first = TRUE)
           },
           BLSMOTE={
             sampling<-list(name = "BLSMOTE",
                            func = function (x, y) {
                              
                              library(smotefamily)
                              library(FNN)
                              dat <- if (is.data.frame(x)) x else as.data.frame(x)
                              
                              dat$.y <- y
                              
                              dat <- BLSMOTE(X=dat[, !grepl(".y", colnames(dat), fixed = TRUE)],target=dat$.y)
                              dat=dat$data
                              list(x = dat[,!colnames(dat)=="class"], 
                                   y = as.factor(dat[,colnames(dat)=="class"]))
                              
                            }
                            ,first = TRUE)
           },
           DBSMOTE={
             sampling<-list(name = "DBSMOTE",
                            func = function (x, y) {
                              
                              library(smotefamily)
                              library(dbscan)
                              dat <- if (is.data.frame(x)) x else as.data.frame(x)
                              
                              dat$.y <- y
                              
                              dat <- DBSMOTE(X=dat[, !grepl(".y", colnames(dat), fixed = TRUE)],target=dat$.y)
                              dat=dat$data
                              list(x = dat[,!colnames(dat)=="class"], 
                                   y = as.factor(dat[,colnames(dat)=="class"]))
                              
                            }
                            ,first = TRUE)
           },
           RSLS={
             sampling<-list(name = "RSLS",
                            func = function (x, y) {
                              
                              library(smotefamily)
                              library(dbscan)
                              dat <- if (is.data.frame(x)) x else as.data.frame(x)
                              
                              dat$.y <- y
                              
                              dat <- RSLS(X=dat[, !grepl(".y", colnames(dat), fixed = TRUE)],target=dat$.y)
                              dat=dat$data
                              list(x = dat[,!colnames(dat)=="class"], 
                                   y = as.factor(dat[,colnames(dat)=="class"]))
                              
                            }
                            ,first = TRUE)
           },
           SLS={
             sampling<-list(name = "SLS",
                            func = function (x, y) {
                              
                              library(smotefamily)
                              library(dbscan)
                              dat <- if (is.data.frame(x)) x else as.data.frame(x)
                              
                              dat$.y <- y
                              
                              dat <- SLS(X=dat[, !grepl(".y", colnames(dat), fixed = TRUE)],target=dat$.y)
                              dat=dat$data
                              list(x = dat[,!colnames(dat)=="class"], 
                                   y = as.factor(dat[,colnames(dat)=="class"]))
                              
                            }
                            ,first = TRUE)
           }
           
    )
    
  }
  # end of the if for changing sampling method. 
  
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
  
  
  # train the function 
  # consider change the input into x and y in the future.
  ml_with_sampling_preprocess=train(  x=data[,colnames(data)!=target]
                                      , y=data[,colnames(data)==target]
                                      , method=method
                                      , metric=metric
                                      , trControl=ctrl_with_sampling
                                      , tuneLength=tuneLength,preProcess=preProcess)
  # collapse the vector for preprocessing to a single character. 
  preProcess=glue::collapse(preProcess,sep=" ")
  # The output message paste together. 
  output_message=paste(method,sampling[[1]],metric,"tuneLength:",tuneLength,"search:",search,"preProcess:",preProcess,"cv_num:",k,"repeats:",repeats,sep=" ")
  # output the model that just finished training. 
  output_message%>%message()
  #record the time use. 
  timeRecordB(output_message = output_message)
  
  
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
  print(paste("Total training model(s):",sum(params[,"tuneLength"]),sep=" "))
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
    preProcess=glue::collapse(preProcess,sep="_")
    file_name=paste(i,method,sampling,preProcess,metric,sep="_")
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



install_pkg_sampling=function(sampling){
  required_sampling_pkgs=list(down="caret",up="caret",smote="DMwR",rose="ROSE",ADAS=c("smotefamily","FNN")
                              ,ANS=c("smotefamily","FNN"),BLSMOTE=c("smotefamily","FNN"),DBSMOTE=c("smotefamily","dbscan")
                              ,RSLS=c("smotefamily","dbscan"),SLS=c("smotefamily","dbscan"))
  needed_pkgs=lapply(sampling,FUN=function(sampling){ required_sampling_pkgs[[sampling]] } )%>%unlist%>%unique()
  # check for the difference between required package and installed packages. 
  missing_pkgs=setdiff(needed_pkgs,rownames(installed.packages()))
  if( length( missing_pkgs )>0    ){
    message(paste("Trying to install packages:",missing_pkgs,"\n"))
    
    install.packages(missing_pkgs)
  }else{
    message("No missing package dependency.")
  }
  
}

# example use of install_pkg_sampling function. 
# install_pkg_sampling(c("down","ADAS","ANS","RSLS"))


# example use of ml_list function with sampling methods 


# params_grid2=expand.grid(sampling=c("down","ADAS","ANS","BLSMOTE","DBSMOTE","RSLS","SLS","smote","rose","up")
#                          ,metric=c("ROC")
#                          ,method=c("glm","glmnet","LogitBoost")
#                          ,preProcess=list(c("center","scale"))
#                          ,search="random"
#                          ,tuneLength=5
#                          ,k=10,nthread=2)
# install_pkg_sampling(params_grid2$sampling)
# 
# baseModels_churn2_down=ml_list(data=train_churn,target = "Churn",params = params_grid2,summaryFunction=twoClassSummary,save_model = "sampling_test")


