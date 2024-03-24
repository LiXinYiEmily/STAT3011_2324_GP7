# Feature Engineering (part 2)
# Feature importance 
# Elastic Net 


```r
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
```

```
##         cvm lambda.1se alpha
## 2 0.8484222   0.014722  0.15
```

```r
# modelling with best parameters
elnet<-glmnet(train.X, train.Y, family="binomial", lambda = best$lambda.1se, alpha = best$alpha)
coef(elnet)
```

```
## 16 x 1 sparse Matrix of class "dgCMatrix"
##                                                      s0
## (Intercept)                               -3.8850050447
## age                                       -0.0064899010
## numberofopencreditlinesandloans            0.0079966567
## numberrealestateloansorlines               0.0449150561
## numberofdependents                         0.0138204854
## age_group                                 -0.0619129317
## prop_3059                                  0.8860853416
## prop_6089                                  1.2995704384
## prop_90plus                                1.6261524849
## category_pastdue                           0.3982434741
## dependents_groups                          0.0091084612
## rsll_groups                                .           
## ocll_quantile_groups                       .           
## sqrt_debtratio                             0.0600517539
## sqrt_monthlyincome                        -0.0004725869
## sqrt_revolvingutilizationofunsecuredlines  1.7421151233
```

```r
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
```

```
## Preparation of a new explainer is initiated
##   -> model label       :  Elastic Net 
##   -> data              :  149999  rows  15  cols 
##   -> target variable   :  149999  values 
##   -> predict function  :  yhat.glmnet  will be used (  default  )
##   -> predicted values  :  No value for predict function target column. (  default  )
##   -> model_info        :  package glmnet , ver. 4.1.8 , task classification (  default  ) 
##   -> model_info        :  type set to  classification 
##   -> predicted values  :  numerical, min =  0.007726903 , mean =  0.06684045 , max =  0.6167332  
##   -> residual function :  difference between y and yhat (  default  )
##   -> residuals         :  numerical, min =  -0.6167332 , mean =  -3.062151e-10 , max =  0.9914846  
##   A new explainer has been created!
```

```r
vip_elnet_auc <- model_parts(explainer_elnet,loss_function = loss_one_minus_auc) #metric is 1-auc
plot(vip_elnet_auc)
```

![plot of chunk unnamed-chunk-1](figure-Feature Engineering(part 2)/unnamed-chunk-1-1.png)

# By using random forest


```r
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
```

```
## i Creating pre-processing data to finalize unknown parameter: mtry
```

```r
# stop cluster
stopCluster(cl)
# best parameters
ranger_best<-select_best(ranger_tune,metric="roc_auc")
ranger_best
```

```
## # A tibble: 1 × 4
##    mtry trees min_n .config              
##   <int> <int> <int> <chr>                
## 1     2  1359    34 Preprocessor1_Model02
```

```r
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
train.Y.rf<-as.numeric(train$seriousdlqin2yrs)-1
library(DALEXtra)
explainer_rf <- explain_tidymodels(
  model=last_fit,
  data=train.X.rf,
  y=train.Y.rf,
  label="rf",
  type = "classification"
)
```

```
## Preparation of a new explainer is initiated
##   -> model label       :  rf 
##   -> data              :  149999  rows  15  cols 
##   -> target variable   :  149999  values 
##   -> predict function  :  yhat.workflow  will be used (  default  )
##   -> predicted values  :  No value for predict function target column. (  default  )
##   -> model_info        :  package tidymodels , ver. 1.1.1 , task classification (  default  ) 
##   -> model_info        :  type set to  classification 
##   -> predicted values  :  numerical, min =  0.00499401 , mean =  0.0669339 , max =  0.835619  
##   -> residual function :  difference between y and yhat (  default  )
##   -> residuals         :  numerical, min =  -1.699689 , mean =  -1.000093 , max =  -0.01124303  
##   A new explainer has been created!
```

```r
vip_rf_auc <- model_parts(explainer_rf,loss_function = loss_one_minus_auc) #metric is 1-auc
plot(vip_rf_auc)
```

![plot of chunk unnamed-chunk-2](figure-Feature Engineering(part 2)/unnamed-chunk-2-1.png)

