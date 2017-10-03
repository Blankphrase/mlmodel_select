
# import the functions for automatic ml model training.
source("https://raw.githubusercontent.com/edwardcooper/mlmodel_select/master/ml_list.R")

# In the future, we need to write it in a way so that we could select and compare different based models. 

#####################################################
## Reproduce the stacking method from https://rasbt.github.io/mlxtend/user_guide/classifier/StackingClassifier/. 



# import the data
source("https://raw.githubusercontent.com/edwardcooper/yelp_datamining/master/data_clean1.R")

#source("https://raw.githubusercontent.com/edwardcooper/mlmodel_select/master/int_to_num.R")

### First step, train the base models.
params_grid=expand.grid(sampling=c("up","down","smote","rose")
                        ,metric=c("ROC")
                        ,method=c("xgbTree")
                        ,search="random"
                        ,tuneLength=5
                        ,k=10, nthread=10)
# give rf method only 5 cores to do the calculation since more cores would leads to memory overflow. 
params_grid[params_grid[,"method"]=="rf","nthread"]=5
# train the meta models.
meta_models=ml_list(data=train_predict_matrix,target="true_label",params=params_grid)

## how to do it if we have the base models.

models=readRDS("models.rds")



# resample the models for visual comparison
models_for_compare=resamples(models)
bwplot(models_for_compare)


# make the prediction from the base models and combine them into a single dataframe. 
train_predict_matrix=prediction_matrix(base_model=models,data=train_data,target = "is_open")


# train_predict_matrix%>%sapply(class)

train_predict_matrix%>%head
colnames(train_predict_matrix)
# It is not a good idea to put all calculations into a single workflow. At each step of the calculations, you will need to save the results. 
# Do the data manipulation and cleaning for next step stacked models.

params_grid=expand.grid(sampling=c("up","down","smote","rose")
                        ,metric=c("ROC")
                        ,method=c("xgbTree")
                        ,search="random"
                        ,tuneLength=5
                        ,k=10, nthread=10)
# give rf method only 5 cores to do the calculation since more cores would leads to memory overflow. 
params_grid[params_grid[,"method"]=="rf","nthread"]=5
# train the meta models.
meta_models=ml_list(data=train_predict_matrix,target="true_label",params=params_grid)


resamples_meta_models=resamples(meta_models)
bwplot(resamples_meta_models)
## how to predict using the meta models. 
## Predict using base models first and combine the predictions and predict with the meta models. 
test_predict_matrix=prediction_matrix(base_model=models,data=test_data,target = "is_open")


meta_models[[5]]%>%predict(test_predict_matrix)%>%confusionMatrix(test_predict_matrix$true_label)
## how to evaluate the performance of meta models with cv. 
## Once the above pipe line is done, we could do the cv easily. 


## How to do multi-layer stacking? Just do the above for mutiple times. 