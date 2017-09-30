
# import the depedencies of stack modeling 
source("https://raw.githubusercontent.com/edwardcooper/mlmodel_select/master/ml_tune.R")

# In the future, we need to write it in a way so that we could select and compare different based models. 
#####################################################
## Reproduce the stacking method from https://rasbt.github.io/mlxtend/user_guide/classifier/StackingClassifier/. 


# The goal of this function ml_stack.train is to do stack model training easily in R.
# The next function ml_stack.cv is to use corss validation to evaluate the stack model performance on unseen data. 



prediction_matrix=function(base_model,data,target){
    # predict on the data provided. 
    base_prediction=foreach(j=1:length(base_model),.combine = cbind)%do%{
      result_predictions=base_model[[j]]%>%predict(data)
      return( result_predictions )
    }
    # change the matrix into data frame to avoid data type matching. 
    base_prediction=as.data.frame(base_prediction)
    # combine the true label from the data
    base_prediction=cbind(base_prediction, true_label = data[,colnames(data)==target] )
    
    # base_prediction=as.data.frame(base_prediction)
    # print the summary of the prediction data. 
    base_prediction%>%sapply(class)%>%print() # This is the prediction data frame we get.
  return(base_prediction)
}


source("https://raw.githubusercontent.com/edwardcooper/yelp_datamining/master/data_clean1.R")

#source("https://raw.githubusercontent.com/edwardcooper/mlmodel_select/master/int_to_num.R")


## how to do it if we have the base models.

models=readRDS("models.rds")

train_predict_matrix=prediction_matrix(base_model=models,data=train_data,target = "is_open")

train_predict_matrix%>%sapply(class)

train_predict_matrix%>%sapply(as.numeric)%>%as.data.frame()%>%sapply(class)

train_predict_matrix$true_label=as.factor(train_predict_matrix$true_label)

train_predict_matrix%>%sapply(class)
colnames(train_predict_matrix)


params_grid=expand.grid(sampling=c("up","down","smote","rose")
                        ,metric=c("ROC")
                        ,method=c("rf","glmnet")
                        ,search="random"
                        ,tuneLength=5
                        ,k=10)

meta_models=ml_list(data=train_predict_matrix,target="true_label",params=params_grid)

meta_models[[1]]%>%predict(train_predict_matrix)%>%confusionMatrix(train_predict_matrix$true_label)

## how to predict using the meta models. 
## Predict using base models first and combine the predictions and predict with the meta models. 
test_predict_matrix=prediction_matrix(base_model=models,data=test_data,target = "is_open")


meta_models[[8]]%>%predict(test_predict_matrix)%>%confusionMatrix(test_predict_matrix$true_label)
meta_models
## how to evaluate the performance of meta models with cv. 
## Once the above pipe line is done, we could do the cv easily. 


## How to do multi-layer stacking?