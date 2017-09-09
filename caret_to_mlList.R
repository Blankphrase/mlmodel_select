### How to convert an already trained caret model into ml_list that could use for stacking?

caret_to_mlList=function(data,model,seed=10,p=0.85){
    
    # record the time when this function begins. 
    t1=proc.time()
    # make all column names legit
    colnames(data)=make.names(colnames(data))
    # set a seed for reproducibility. 
    set.seed(seed)
    # split the data
    index=createDataPartition(data$SalePrice,p=p,list=FALSE)
    train=data[index,]
    # train_y=train$SalePrice%>%as.vector()%>%as.numeric()%>%log1p()
    # train_x=train%>%select(-SalePrice)
    test=data[-index,]
    # test_y=test$SalePrice%>%as.vector()%>%as.numeric()%>%log1p()
    # test_x=test%>%select(-SalePrice)
    
    # cl=makeCluster(detectCores()-1)
    # registerDoParallel(cl)
    
    
    
    # ## Adaptive resampling 
    # ctrl<- trainControl(method = "repeatedcv",
    #                     number = 10, repeats = 1,
    #                     adaptive = list(min = 5, alpha = 0.05, method = "gls", complete = TRUE),
    #                     search = "random")
    
    
    ML2=model
    
    
    
    # stopCluster(cl)
    # stopImplicitCluster()
    gc()
    
    
    
    # timeRecordB()
    
    # make predictions on botb train and test data.
    y_pred_test=ML2%>%predict(test)
    y_pred_train=ML2%>%predict(train)
    
    # save and print performance on both train and test data.
    performance_test=RMSE(y_pred=y_pred_test,y_true =log1p(test$SalePrice) )
    paste("Test data performance is ",performance_test,sep="")%>%print()
    performance_train=RMSE(y_pred=y_pred_train,y_true =log1p(train$SalePrice) )
    paste("Train data performance is ",performance_train,sep="")%>%print()
    
    
    
    #timeRecordR(unit="min")%>%print()
    
    varImp(ML2,scale=FALSE)%>%print()
    plot(x=y_pred_test,y=log1p(test$SalePrice)-y_pred_test)%>%print()
    cor(x=y_pred_test,y=log1p(test$SalePrice))%>%print()
    # record the time to do all the calculations and plotting 
    t2=proc.time()
    # change the preProcess value to a single character 
    preProcess_char=paste(preProcess,collapse = ",")
    
    ml_list=list(model=ML2,pred_train=y_pred_train,pred_test=y_pred_test, elapsed=(t2-t1)/60,name=method,preProcess= preProcess_char,performance_train=performance_train,performance_test=performance_test )
    return(ml_list)
}