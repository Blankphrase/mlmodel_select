
# import the depedencies of stack modeling 
source("https://raw.githubusercontent.com/edwardcooper/mlmodel_select/master/ml_tune.R")

# In he future, we need to write it in a way so that we could select and compare different based models. 
#####################################################
## Reproduce the stacking method from https://rasbt.github.io/mlxtend/user_guide/classifier/StackingClassifier/. 


# The goal of this function ml_stack.train is to do stack model training easily in R.
# The next function ml_stack.cv is to use corss validation to evaluate the stack model performance on unseen data. 



ml_stack_predict=function(data,target,params,base_model="NULL"){
  
  
  # If the base models are given, then do not re-train the base models. Just make prediction and train the meta models. 
  if(base_model=="NULL"){
    paste("Base models")%>%message()
    # No base model is given. Need to train the base models. 
    ## train the list of base models. 
    base_model=ml_list(data=data,target=target,params = params)
  
   }else{
     base_model=base_model
  }
  

    # predict on the data provided. 
    base_prediction=foreach(j=1:nrow(params),.combine = cbind)%do%{
      return( base_model[[j]]%>%predict(data)%>%as.character() )
    }
    
    # combine the true label from the data
    base_prediction=cbind(base_prediction, true_label=data[,colnames(data)==target]%>%as.character()  )
    base_prediction=as.data.frame(base_prediction)
    # print the summary of the prediction data. 
    base_prediction%>%summary()%>%print() # This is the prediction matrix we get.
  return(base_prediction)
}

params_grid=expand.grid(sampling=c("up","down")
                        ,metric=c("ROC","Kappa")
                        ,method=c("xgbLinear","xgbTree")
                        ,search="random"
                        ,tuneLength=1
                        ,k=2)

meta_models=ml_stack_predict(data=train_data,target="is_open",params = params_grid)

meta_models

## how to do it if we have the base models.

## how to predict using the meta models. 

## how to evaluate the performance of meta models with cv. 


