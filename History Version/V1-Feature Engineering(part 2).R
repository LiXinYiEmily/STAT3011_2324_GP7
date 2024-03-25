#' # Feature Engineering (part 2)
#' # Feature importance 
#' # Elastic Net 
library(caret)
library(doParallel)
library(foreach)
library(pROC)
library(coefplot)
library(tidyverse)
setwd("/Users/cailiying/Desktop/cuhk bsc/23:24/23:24 T2/STAT3011/Project2")
train <- read.csv("cs-training-processed-v1.csv")
train <- train %>% 
  mutate(across(c(seriousdlqin2yrs, age_group, category_pastdue, dependents_groups, rsll_groups, ocll_quantile_groups),~as.factor(.x)))
set.seed(666)
alpha<-seq(0.1,0.9,0.05)
train.X<-as.matrix(train[,2:ncol(train)])
train.Y<-train$seriousdlqin2yrs

# Create a cluster object and then register: 
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

# Find the best parameters
library(glmnet)
search<-foreach(i=alpha, .combine=rbind, .packages='glmnet') %dopar% {
  cv<-cv.glmnet(train.X, train.Y, family="binomial", nfold=10, type.measure="auc", paralle=TRUE, alpha=i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
# stop cluster
stopCluster(cl)
# best parameters
best<-search[search$cvm==min(search$cvm),]
best

# modelling with best parameters
elnet<-glmnet(train.X, train.Y, family="binomial", lambda = best$lambda.1se, alpha = best$alpha)
coef(elnet)

# variable importance
train.X<-train[,-which(names(train)=="seriousdlqin2yrs")]
train.Y<-as.numeric(train$seriousdlqin2yrs)-1
library(DALEXtra)
explainer_elnet <- explain_tidymodels(
  model=elnet,
  data=train.X,
  y=train.Y,
  label="Elastic Net",
  type = "classification"
)
vip_elnet_auc <- model_parts(explainer_elnet,loss_function = loss_one_minus_auc) #metric is 1-auc
plot(vip_elnet_auc)


#' # By using random forest
set.seed(3011)
library(caret)
library(ranger)
library(tidyverse)
library(e1071)
library(recipes)
library(workflows)
library(rsample)
library(tidymodels)
library(pROC)
library(dplyr)
library(doMC)

# train dataset
train <- read.csv("cs-training-processed-v1.csv")
train <- train %>% 
  mutate(across(c(seriousdlqin2yrs, age_group, category_pastdue, dependents_groups, rsll_groups, ocll_quantile_groups),~as.character(.x)))

# Specify the formula and data for modeling
ranger_recipe <- 
  recipe(seriousdlqin2yrs~., data = train) 
# Specify a random forest model for modeling
ranger_spec <- 
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("ranger") 
# Create a workflow that combines recipe with the modeling
ranger_workflow <-
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 
# Define data partitioning for evalution
ranger_rsample <- vfold_cv(train,v=5)

# Create a cluster object and then register: 
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

# Perform tuning to find the best parameters
ranger_tune <-
  tune_grid(ranger_workflow, resamples = ranger_rsample, 
            grid = 50, control=control_grid(verbose=TRUE))
# stop cluster
stopCluster(cl)
# best parameters
ranger_best<-select_best(ranger_tune,metric="roc_auc")
ranger_best

# Last Fit
last_ranger_spec <- 
  rand_forest(mtry=ranger_best$mtry,
              min_n =ranger_best$min_n,
              trees = ranger_best$trees) %>% 
  set_mode("classification") %>% 
  set_engine("ranger") 
last_ranger_workflow <- 
  ranger_workflow %>% 
  update_model(last_ranger_spec)
last_fit <- last_ranger_workflow %>% 
  fit(train)

# variable importance
train.X.rf<-train[,-which(names(train)=="seriousdlqin2yrs")]
train.Y.rf<-as.numeric(train$seriousdlqin2yrs)
library(DALEXtra)
explainer_rf <- explain_tidymodels(
  model=last_fit,
  data=train.X.rf,
  y=train.Y.rf,
  label="rf",
  type = "classification"
)
vip_rf_auc <- model_parts(explainer_rf,loss_function = loss_one_minus_auc) #metric is 1-auc
plot(vip_rf_auc)

