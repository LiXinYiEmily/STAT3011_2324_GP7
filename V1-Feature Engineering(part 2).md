# Feature Engineering (part 2)
# Feature importance 
# Elastic Net 


```r
# variable category_pastdue to dummy variable
library(caret)
library(doParallel)
library(foreach)
library(pROC)
library(coefplot)
library(dplyr)
library(tidyverse)
setwd("/Users/cailiying/Desktop/cuhk bsc/23:24/23:24 T2/STAT3011/Project2")
train <- read.csv("cs-training-processed-v1.csv")
train <- train %>% 
  mutate(across(c(seriousdlqin2yrs, age_group, category_pastdue, dependents_groups, rsll_groups, ocll_quantile_groups),~as.factor(.x)))
glimpse(train)
```

```
## Rows: 149,999
## Columns: 16
## $ seriousdlqin2yrs                          [3m[38;5;246m<fct>[39m[23m 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ age                                       [3m[38;5;246m<int>[39m[23m 45, 40, 38, 30, 49, 74, 57, 39, 27…
## $ numberofopencreditlinesandloans           [3m[38;5;246m<int>[39m[23m 13, 4, 2, 5, 7, 3, 8, 8, 2, 9, 5, …
## $ numberrealestateloansorlines              [3m[38;5;246m<int>[39m[23m 6, 0, 0, 0, 1, 1, 3, 0, 0, 4, 0, 2…
## $ numberofdependents                        [3m[38;5;246m<int>[39m[23m 2, 1, 0, 0, 0, 1, 0, 0, 0, 2, 0, 2…
## $ age_group                                 [3m[38;5;246m<fct>[39m[23m 3, 3, 2, 2, 3, 5, 4, 2, 1, 4, 2, 4…
## $ prop_3059                                 [3m[38;5;246m<dbl>[39m[23m 1.0000000, 0.0000000, 0.5000000, 0…
## $ prop_6089                                 [3m[38;5;246m<dbl>[39m[23m 0.0000000, 0.0000000, 0.0000000, 0…
## $ prop_90plus                               [3m[38;5;246m<dbl>[39m[23m 0.0000000, 0.0000000, 0.5000000, 0…
## $ category_pastdue                          [3m[38;5;246m<fct>[39m[23m 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0…
## $ dependents_groups                         [3m[38;5;246m<fct>[39m[23m 2, 1, 0, 0, 0, 1, 0, 0, 0, 2, 0, 2…
## $ rsll_groups                               [3m[38;5;246m<fct>[39m[23m 3, 0, 0, 0, 1, 1, 3, 0, 0, 3, 0, 2…
## $ ocll_quantile_groups                      [3m[38;5;246m<fct>[39m[23m 3, 0, 0, 1, 1, 0, 2, 2, 0, 2, 1, 1…
## $ sqrt_debtratio                            [3m[38;5;246m<dbl>[39m[23m 0.8960927, 0.3491077, 0.2917420, 0…
## $ sqrt_monthlyincome                        [3m[38;5;246m<dbl>[39m[23m 95.49869, 50.99020, 55.15433, 57.4…
## $ sqrt_revolvingutilizationofunsecuredlines [3m[38;5;246m<dbl>[39m[23m 0.8752866, 0.9783410, 0.8112830, 0…
```

```r
set.seed(666)
alpha<-seq(0.1,0.9,0.05)
train.X<-as.matrix(train[,2:ncol(train)])
train.Y<-train$seriousdlqin2yrs

# Create a cluster object and then register: 
cl <- makePSOCKcluster(6)
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
##          cvm  lambda.1se alpha
## 17 0.8483736 0.003243604   0.9
```

```r
# modelling with best parameters
elnet<-glmnet(train.X, train.Y, family="binomial", lambda = best$lambda.1se, alpha = best$alpha)
```

```
## Warning: from glmnet C++ code (error code -1); Convergence for 1th lambda value not
## reached after maxit=100000 iterations; solutions for larger lambdas returned
```

```
## Warning in getcoef(fit, nvars, nx, vnames): an empty model has been returned;
## probably a convergence issue
```

```r
coef(elnet)
```

```
## 16 x 1 sparse Matrix of class "dgCMatrix"
##                                           s0
## (Intercept)                                .
## age                                        0
## numberofopencreditlinesandloans            .
## numberrealestateloansorlines               .
## numberofdependents                         .
## age_group                                  .
## prop_3059                                  .
## prop_6089                                  .
## prop_90plus                                .
## category_pastdue                           .
## dependents_groups                          .
## rsll_groups                                .
## ocll_quantile_groups                       .
## sqrt_debtratio                             .
## sqrt_monthlyincome                         .
## sqrt_revolvingutilizationofunsecuredlines  .
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
##   -> predicted values  :  numerical, min =  0.5 , mean =  0.5 , max =  0.5  
##   -> residual function :  difference between y and yhat (  default  )
##   -> residuals         :  numerical, min =  -0.5 , mean =  -0.4331596 , max =  0.5  
##   A new explainer has been created!
```

```r
vip_elnet_auc <- model_parts(explainer_elnet,loss_function = loss_one_minus_auc) #metric is 1-auc
plot(vip_elnet_auc)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

# By using random forest


```r
set.seed(3011)
library(caret)
library(ranger)
library(tidyverse)
library(e1071)
```

```
## 
## Attaching package: 'e1071'
```

```
## The following object is masked from 'package:coefplot':
## 
##     extractPath
```

```r
library(recipes)
```

```
## 
## Attaching package: 'recipes'
```

```
## The following object is masked from 'package:Matrix':
## 
##     update
```

```
## The following object is masked from 'package:stringr':
## 
##     fixed
```

```
## The following object is masked from 'package:stats':
## 
##     step
```

```r
library(workflows)
library(rsample)
```

```
## 
## Attaching package: 'rsample'
```

```
## The following object is masked from 'package:e1071':
## 
##     permutations
```

```r
library(tidymodels)
```

```
## ── Attaching packages ──────────────────────────────────────────── tidymodels 1.1.1 ──
```

```
## ✔ broom        1.0.5     ✔ parsnip      1.1.1
## ✔ dials        1.2.0     ✔ tune         1.1.2
## ✔ infer        1.0.5     ✔ workflowsets 1.0.1
## ✔ modeldata    1.2.0     ✔ yardstick    1.2.0
```

```
## ── Conflicts ─────────────────────────────────────────────── tidymodels_conflicts() ──
## ✖ purrr::accumulate()      masks foreach::accumulate()
## ✖ scales::discard()        masks purrr::discard()
## ✖ Matrix::expand()         masks tidyr::expand()
## ✖ DALEX::explain()         masks dplyr::explain()
## ✖ dplyr::filter()          masks stats::filter()
## ✖ recipes::fixed()         masks stringr::fixed()
## ✖ dplyr::lag()             masks stats::lag()
## ✖ purrr::lift()            masks caret::lift()
## ✖ Matrix::pack()           masks tidyr::pack()
## ✖ rsample::permutations()  masks e1071::permutations()
## ✖ yardstick::precision()   masks caret::precision()
## ✖ yardstick::recall()      masks caret::recall()
## ✖ yardstick::sensitivity() masks caret::sensitivity()
## ✖ yardstick::spec()        masks readr::spec()
## ✖ yardstick::specificity() masks caret::specificity()
## ✖ recipes::step()          masks stats::step()
## ✖ tune::tune()             masks parsnip::tune(), e1071::tune()
## ✖ Matrix::unpack()         masks tidyr::unpack()
## ✖ recipes::update()        masks Matrix::update(), stats::update()
## ✖ purrr::when()            masks foreach::when()
## • Dig deeper into tidy modeling with R at https://www.tmwr.org
```

```r
library(pROC)
library(dplyr)
library(doMC)

# train dataset
train <- read_csv("cs-training-processed-v1.csv")
```

```
## Rows: 149999 Columns: 16
```

```
## ── Column specification ──────────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (16): seriousdlqin2yrs, age, numberofopencreditlinesandloans, numberrealestate...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
train <- train %>% 
  mutate(across(where(is.integer),~as.factor(.x)))

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
cl <- makePSOCKcluster(6)
registerDoParallel(cl)

# Perform tuning to find the best parameters
ranger_tune <-
  tune_grid(ranger_workflow, resamples = ranger_rsample, 
            grid = 50, control=control_grid(verbose=TRUE))
```

```
## i Creating pre-processing data to finalize unknown parameter: mtry
```

```
## Warning: All models failed. Run `show_notes(.Last.tune.result)` for more information.
```

```r
# stop cluster
stopCluster(cl)
# best parameters
ranger_best<-select_best(ranger_tune,metric="roc_auc")
```

```
## Error in `estimate_tune_results()`:
## ! All models failed. Run `show_notes(.Last.tune.result)` for more information.
```

```r
ranger_best
```

```
## Error in eval(expr, envir, enclos): object 'ranger_best' not found
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
```

```
## Error in `check_outcome()`:
## ! For a classification model, the outcome should be a `factor`, not a `numeric`.
```

```r
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
## Error in model$spec: object of type 'closure' is not subsettable
```

```r
vip_rf_auc <- model_parts(explainer_rf,loss_function = loss_one_minus_auc) #metric is 1-auc
```

```
## Error in eval(expr, envir, enclos): object 'explainer_rf' not found
```

```r
plot(vip_rf_auc)
```

```
## Error in eval(expr, envir, enclos): object 'vip_rf_auc' not found
```

