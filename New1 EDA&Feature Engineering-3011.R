#' # Exploratory Data Analysis (EDA)
#-------------------------------------------------------------------------------
setwd("/Users/cailiying/Desktop/cuhk bsc/23:24/23:24 T2/STAT3011/Project2")
library(dlookr)
library(tidyverse)
library(ggcorrplot)
library(patchwork)

#' # Descriptive Statistics
#Computes the statistics of all numerical variables 
#describe(df)
#In SQL
library(RSQL) #Generate and Process 'SQL' Queries in R
library(RSQLite)
con <- dbConnect(drv = RSQLite::SQLite(),
                 dbname = ":memory:")
dbListTables(con)

training_data = read.csv("cs-training.csv")
#Drop the column (Unnamed: 0)
training_data <- training_data[,-1]

#Clean the column names
colnames(training_data) <- gsub("[-,.]", "", tolower(colnames(training_data)))
head(training_data)

#Load the table
dbWriteTable(conn = con, 
             name = "training_data",
             value = training_data)
describe  =  DBI::dbGetQuery(conn = con,
                             statement = "
                SELECT 'revolvingutilizationofunsecuredlines' AS variable,
                       COUNT(revolvingutilizationofunsecuredlines) AS count,
                       AVG(revolvingutilizationofunsecuredlines) AS mean,
                       STDEV(revolvingutilizationofunsecuredlines) AS std,
                       MIN(revolvingutilizationofunsecuredlines) AS min,
                       MAX(revolvingutilizationofunsecuredlines) AS max
                FROM training_data
                UNION ALL
                SELECT 'age' AS variable,
                       COUNT(age) AS count,
                       AVG(age) AS mean,
                       STDEV(age) AS std,
                       MIN(age) AS min,
                       MAX(age) AS max
                FROM training_data
                UNION ALL
                SELECT 'numberoftime3059dayspastduenotworse' AS variable,
                       COUNT(numberoftime3059dayspastduenotworse) AS count,
                       AVG(numberoftime3059dayspastduenotworse) AS mean,
                       STDEV(numberoftime3059dayspastduenotworse) AS std,
                       MIN(numberoftime3059dayspastduenotworse) AS min,
                       MAX(numberoftime3059dayspastduenotworse) AS max
                FROM training_data
                UNION ALL
                SELECT 'debtratio' AS variable,
                       COUNT(debtratio) AS count,
                       AVG(debtratio) AS mean,
                       STDEV(debtratio) AS std,
                       MIN(debtratio) AS min,
                       MAX(debtratio) AS max
                FROM training_data
                UNION ALL
                SELECT 'monthlyincome' AS variable,
                       COUNT(monthlyincome) AS count,
                       AVG(monthlyincome) AS mean,
                       STDEV(monthlyincome) AS std,
                       MIN(monthlyincome) AS min,
                       MAX(monthlyincome) AS max
                FROM training_data
                UNION ALL
                SELECT 'numberofopencreditlinesandloans' AS variable,
                       COUNT(numberofopencreditlinesandloans) AS count,
                       AVG(numberofopencreditlinesandloans) AS mean,
                       STDEV(numberofopencreditlinesandloans) AS std,
                       MIN(numberofopencreditlinesandloans) AS min,
                       MAX(numberofopencreditlinesandloans) AS max
                FROM training_data
                UNION ALL 
                SELECT 'numberoftimes90dayslate' AS variable,
                       COUNT(numberoftimes90dayslate) AS count,
                       AVG(numberoftimes90dayslate) AS mean,
                       STDEV(numberoftimes90dayslate) AS std,
                       MIN(numberoftimes90dayslate) AS min,
                       MAX(numberoftimes90dayslate) AS max
                FROM training_data
                UNION ALL
                SELECT 'numberrealestateloansorlines' AS variable,
                       COUNT(numberrealestateloansorlines) AS count,
                       AVG(numberrealestateloansorlines) AS mean,
                       STDEV(numberrealestateloansorlines) AS std,
                       MIN(numberrealestateloansorlines) AS min,
                       MAX(numberrealestateloansorlines) AS max
                FROM training_data
                UNION ALL
                SELECT 'numberoftime6089dayspastduenotworse' AS variable,
                       COUNT(numberoftime6089dayspastduenotworse) AS count,
                       AVG(numberoftime6089dayspastduenotworse) AS mean,
                       STDEV(numberoftime6089dayspastduenotworse) AS std,
                       MIN(numberoftime6089dayspastduenotworse) AS min,
                       MAX(numberoftime6089dayspastduenotworse) AS max
                FROM training_data
                UNION ALL
                SELECT 'numberofdependents' AS variable,
                       COUNT(numberofdependents) AS count,
                       AVG(numberofdependents) AS mean,
                       STDEV(numberofdependents) AS std,
                       MIN(numberofdependents) AS min,
                       MAX(numberofdependents) AS max
                FROM training_data
                ")
describe=as.matrix(describe)
#calculate percentiles
percentiles <- apply(training_data, 2, quantile, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
percentiles = percentiles[,2:11]
a=t(percentiles)
describe=cbind(a,describe)
#descriptive analysis
describe

#Read the CSV file
df <- read_csv("cs-training.csv")

#Drop the column (Unnamed: 0)
df <- df[,-1]
#Clean the column names
colnames(df) <- gsub("-", "", tolower(colnames(df)))

#' # Plot the distribution of target variable “seriousdlqin2yrs”
#Calculate the total length of the training data
total_len <- length(df$seriousdlqin2yrs)

#Calculate the percentage of each category in the target label
percentage_labels <- prop.table(table(df$seriousdlqin2yrs)) * 100
#Print the percentage labels
print(percentage_labels)

library(ggplot2)
df$seriousdlqin2yrs <- as.factor(df$seriousdlqin2yrs)
#Define custom colors for each bar
bar_colors <- c("steelblue", "darkgreen")
#Create the countplot with different colors for each bar
plot <- ggplot(df, aes(x = seriousdlqin2yrs, fill = seriousdlqin2yrs)) +
  geom_bar() +
  scale_fill_manual(values = bar_colors) +
  labs(title = "Data Distribution",
       x = "Labels for seriousdlqin2yrs attribute",
       y = "Numbers of records") +
  theme_bw()
#Create a data frame with label values for each bar
label_data <- data.frame(
  seriousdlqin2yrs = levels(df$seriousdlqin2yrs),
  #Specify the label values for each bar as percentages
  label = c(93.316, 6.684) / 100  
)
#Compute the maximum bar height
max_height <- max(table(df$seriousdlqin2yrs))
#Add labels to the plot
plot_with_labels <- plot +
  geom_text(data = label_data,
            aes(label = paste0(label * 100, "%"), fill = seriousdlqin2yrs),
            y = max_height, vjust = -0.5, size = 5, position = position_stack(vjust = 0.5))
#Display the plot
print(plot_with_labels)


#' Plot every variables
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))

df_numeric %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(~variable,scales = "free")+
  theme_bw()
#Many variables are very unevenly distributed


#' # MISSING VALUES 
#Calculate the count of missing values in each attribute
missing_counts <- colSums(is.na(df))

#Create a data frame with x and y values
missing_data <- data.frame(attributes = names(missing_counts),
                           missing_count = missing_counts)

#Create the bar plot
bar_colors <- c("steelblue","black","darkgreen", "pink","blue","green","grey","yellow","purple","orange","darkorange")
plot <- ggplot(missing_data, aes(x = attributes, y = missing_count, fill = attributes)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = bar_colors) +
  labs(title = "Missing Data",
       x = "Data Attributes",
       y = "Count of Missing Records") +
  #Rotate x-axis labels if needed
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

#Add labels to the plot
plot_with_labels <- plot +
  geom_text(aes(label = missing_count), vjust = -0.5, size = 3)
#Display the plot
print(plot_with_labels)
#There are two variables "monthly income" and "number of dependents" with missing values.

#Fill in the missing values of these two variables and calculate their average values according to age groups.
table(df$age)
#delete age=0, change age=21 to 22
df <- df[df$age != 0, ]
df$age[df$age == 21] <- 22
table(df$age)
#categorize age into five age_group: 20-30, 31-40, 41-50, 51-60, 60+
df$age_group<-cut(df$age, breaks = c(20, 30, 40, 50, 60, Inf), 
                  labels = c("20-30", "31-40", "41-50", "51-60", "60+"),
                  right = FALSE)
table(df$age_group)
df <- subset(df, select = -age)
#Calculate the average monthly income and number of dependents for each age group
df_summary <- df %>%
  group_by(age_group) %>%
  summarise(
    avg_monthly_income = mean(monthlyincome, na.rm = TRUE),
    avg_number_of_dependents = round(mean(numberofdependents, na.rm = TRUE))
  )
print(df_summary)
#Merge the average values back into the original data frame
df <- df %>%
  left_join(df_summary, by = "age_group") %>%
  mutate(
    numberofdependents = ifelse(is.na(numberofdependents), avg_number_of_dependents, 
                                numberofdependents),
    monthlyincome = ifelse(is.na(monthlyincome), avg_monthly_income, monthlyincome)
  ) %>%
  #Remove the temporary average columns
  select(-avg_number_of_dependents, -avg_monthly_income) 

#Check Missing value again
colSums(is.na(df))

#' # CORRELATION
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))
cor <- cor(df_numeric)
p_mat <- ggcorrplot::cor_pmat(cor)
ggcorrplot::ggcorrplot(cor,p.mat = p_mat,lab = TRUE)


#' # Feature Engineering
#' # Process variables
#' Process the three variables 'numberoftime3059dayspastduenotworse', 'numberoftime6089dayspastduenotworse', 'numberoftimes90dayslate'
#Create a new column 'category_pastdue' to store the classification
df$category_pastdue <- apply(df[, c('numberoftime3059dayspastduenotworse', 
                                    'numberoftime6089dayspastduenotworse', 
                                    'numberoftimes90dayslate')], 1, function(x) {
                                      #Calculate the total number of past due occurrences
                                      total_past_due <- x['numberoftime3059dayspastduenotworse'] + 
                                        x['numberoftime6089dayspastduenotworse'] + x['numberoftimes90dayslate']
                                      #Calculate the proportion for each time frame
                                      prop_3059 <- ifelse(is.na(x['numberoftime3059dayspastduenotworse'] / total_past_due), 0, 
                                                          x['numberoftime3059dayspastduenotworse'] / total_past_due)
                                      prop_6089 <- ifelse(is.na(x['numberoftime6089dayspastduenotworse'] / total_past_due), 0, 
                                                          x['numberoftime6089dayspastduenotworse'] / total_past_due)
                                      prop_90plus <- ifelse(is.na(x['numberoftimes90dayslate'] / total_past_due), 0, 
                                                            x['numberoftimes90dayslate'] / total_past_due)
                                      if (prop_3059 == 0 & 
                                          prop_6089 == 0 & 
                                          prop_90plus == 0) {
                                        return('No Past Due')
                                      } else {
                                        #Classify based on the proportions
                                        if (prop_3059 > 0.5) {
                                          return('Mostly 30-59 Days Past Due')
                                        } else if (prop_6089 > 0.3) {
                                          return('Mostly 60-89 Days Past Due')
                                        } else {
                                          return('Mostly 90 days/+ Past Due')
                                        }
                                      }
                                    })
table(df$category_pastdue)
#remove three columns
df <- subset(df, select = -c(numberoftime3059dayspastduenotworse, numberoftime6089dayspastduenotworse, numberoftimes90dayslate))

#' Process "numberofdependents"
df$categoryofdependents <- cut(df$numberofdependents,
                               breaks = c(-Inf, 0, 1, 2, Inf),
                               labels = c("0", "1", "2", "3+"),
                               right = FALSE)
df <- subset(df, select = -numberofdependents)
table(df$categoryofdependents)
#' Process numberrealestateloansorlines
summary(df$numberrealestateloansorlines)
count_by_value <- table(df$numberrealestateloansorlines)
df$nrl_quantile_groups <- cut(df$numberrealestateloansorlines, 
                           breaks = c(0, 1, 2, 3,54),
                           labels = c("0", "1", "2", "3+"),
                           include.lowest = TRUE)
table(df$nrl_quantile_groups)
df <- subset(df, select = -numberrealestateloansorlines)


#' Process numberofopencreditlinesandloans
summary(df$numberofopencreditlinesandloans)
df$cll_quantile_groups <- cut(df$numberofopencreditlinesandloans, 
                           breaks = c(0,5, 8, 11, 58),
                           labels = c("0", "5", "8", "11+"),
                           include.lowest = TRUE)
table(df$cll_quantile_groups)
df <- subset(df, select = -numberofopencreditlinesandloans)

#' Plot Categoriacal variables
df_chr_factor <- df %>% 
  select(where(~ is.character(.) || is.factor(.)))
df_chr_factor %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=value))+
  geom_bar()+
  facet_wrap(~variable,scales = "free")+
  theme_bw()


#' # OUTLIER DETECTION
#Boxplot
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))
df_numeric %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=variable, y=value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme_bw()

#OUTLIER DETECTION
percentile_based_outlier <- function(data) {
  stats <- boxplot.stats(data)
  return(data %in% stats$out)
}

mad_based_outlier <- function(data, thresh = 3.5){
  if (length(dim(data)) == 1) {
    data <- matrix(data, ncol = 1)
  }
  med <- median(data)
  mad <- median(abs(data - med))
  modified_z_scores <- 0.6745*(data-med)/mad
  return(modified_z_scores>thresh)
}

std_div<- function(data, threshold=1.96){
  std=sd(data)
  mean=mean(data)
  z <- abs(data - mean)/std
  return(z>threshold)
}

outlierVote <- function(data, mad_thresh = 3.5, std_div_thresh=1.96) {
  pbo <- percentile_based_outlier(data)
  mbo <- mad_based_outlier(data, mad_thresh)
  sdo <- std_div(data, std_div_thresh)
  combined <- (pbo & mbo) | (pbo & sdo) | (mbo & sdo)
  return(combined)
}

plotOutlier <- function(x) {
  par(mfrow = c(4, 1))  # Set up 4 subplots in a vertical layout
  funcs <- list(percentile_based_outlier, mad_based_outlier, std_div, outlierVote)
  titles <- c('Percentile-based Outliers', 'MAD-based Outliers', 'STD-based Outliers', 'Majority vote based Outliers')
  for (i in 1:length(funcs)) {
    outliers <- x[funcs[[i]](x)]
    plot(density(x), main = titles[i], xlab = '', ylab = '', ylim = c(0, max(density(x)$y) * 1.1))  # Plot density estimate
    rug(x)
    points(outliers, rep(0, length(outliers)), col = 'red')  # Plot outliers as red dots
  }
  par(mfrow = c(1, 1))  # Reset plot layout to default
}

# Plot outliers
sample_data <- df$revolvingutilizationofunsecuredlines[sample(nrow(df), 5000)]
plotOutlier(sample_data)
sample_data <- df$debtratio[sample(nrow(df), 5000)]
plotOutlier(sample_data)
sample_data <- df$monthlyincome[sample(nrow(df), 5000)]
plotOutlier(sample_data)

#Remove some outliers using MAD method
debtratio_outlier <- outlierVote(df$debtratio)
sum(debtratio_outlier)
df <- df[!debtratio_outlier, ]

monthlyincome_outlier <- outlierVote(df$monthlyincome)
sum(monthlyincome_outlier)
df <- df[!monthlyincome_outlier, ]

revolvingutilizationofunsecuredlines_outlier <- outlierVote(df$revolvingutilizationofunsecuredlines)
sum(revolvingutilizationofunsecuredlines_outlier)
df <- df[!revolvingutilizationofunsecuredlines_outlier, ]

#Check boxplot again
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))
df_numeric %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=variable, y=value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme_bw()

#' Plot numeric variables
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))

df_numeric %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=value))+
  geom_histogram(bins=30)+
  facet_wrap(~variable,scales = "free")+
  theme_bw()


#' some of them is right skew. Try to take square root.
df_numeric %>% select(
  debtratio,monthlyincome, revolvingutilizationofunsecuredlines
) %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=sqrt(value)))+
  geom_histogram(bins=30)+
  facet_wrap(~variable,scales = "free")+
  theme_bw()

df$sqrt_debtratio <- sqrt(df$debtratio)
df$sqrt_monthlyincome <- sqrt(df$monthlyincome)
df$sqrt_revolvingutilizationofunsecuredlines <- sqrt(df$revolvingutilizationofunsecuredlines)
#remove three original columns
df <- subset(df, select = -c(debtratio, monthlyincome, revolvingutilizationofunsecuredlines))

#' See correlation again
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))
cor <- cor(df_numeric)
p_mat <- ggcorrplot::cor_pmat(cor)
ggcorrplot::ggcorrplot(cor,p.mat = p_mat,lab = TRUE)


#' # Relation between response and predictors
#' Now we start to see the relation between response and predictors.
df %>% select(
  sqrt_debtratio,sqrt_monthlyincome,sqrt_revolvingutilizationofunsecuredlines,seriousdlqin2yrs
) %>%
  pivot_longer(1:3,names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=seriousdlqin2yrs,y=value))+
  geom_boxplot()+
  facet_wrap(~variable,scales = "free")+
  theme_bw()
# Not being more than 90 days past due appears to be associated with a low debt ratio, high monthly income and a low ratio of total unsecured debt to total unsecured lines of credit (RevolvingUtilizationOfUnsecuredLines)

#' Plot mosaic plot
#the longer the horizontal axis represents a higher proportion, the longer the numerical axis represents a higher proportion of this subcategory within the category
library(devtools)
library(ggmosaic)
library(patchwork)
graph <- df %>% select(where(~ is.character(.) || is.factor(.))) %>% 
  pivot_longer(-seriousdlqin2yrs,names_to = "variable",values_to = "value") %>% 
  group_by(variable) %>% nest() %>% ungroup() %>% 
  mutate(
    graph=map2(data,variable,~ggplot(.x,)+geom_mosaic(aes(x=product(seriousdlqin2yrs),fill=value))+theme_bw()+theme(legend.position = "none")+scale_color_binned()+labs(title = .y[[1]],y=""))
  ) %>%  select(graph) %>% as.list() 

wrap_plots(graph$graph,ncol = 2)


#' # Process categorical variables
df$age_group <- as.factor(
  ifelse(df$age_group == "20-30", 1,
         ifelse(df$age_group == "31-40", 2,
                ifelse(df$age_group == "41-50", 3,
                       ifelse(df$age_group == "51-60", 4, 5)))))
table(df$age_group)

df$categoryofdependents <- as.factor(
  ifelse(df$categoryofdependents == "0", 0, 
         ifelse(df$categoryofdependents == "1", 1,
                ifelse(df$categoryofdependents == "2", 2, 3)))
)
table(df$categoryofdependents)

df$cll_quantile_groups <- as.factor(
  ifelse(df$cll_quantile_groups == "0", 0, 
         ifelse(df$cll_quantile_groups == "5", 5,
                ifelse(df$cll_quantile_groups == "8", 8, 11)))
)
table(df$cll_quantile_groups)

df$nrl_quantile_groups <- as.factor(
  ifelse(df$nrl_quantile_groups == "0", 0, 
         ifelse(df$nrl_quantile_groups == "1", 1,
                ifelse(df$nrl_quantile_groups == "2", 2, 3)))
)
table(df$nrl_quantile_groups)

#' # Feature importance 
#dummy variables
#category_pastdue dummy variable
library(caret)
train <- df %>% 
  mutate(across(where(is.integer),~as.factor(.x)))
dmy.pastdue<-data.frame(predict(dummyVars(~category_pastdue,train, fullRank=F), train))
dmy.pastdue <- dmy.pastdue %>% 
  mutate(across(where(is.double),~as.factor(.x)))
train<-cbind(df[,-which(names(train)=="category_pastdue")], dmy.pastdue)

#use df directly without using dummy variables
library(glmnet)
library(doParallel)
library(foreach)
library(pROC)
library(coefplot)
#registerDoParallel(8)
set.seed(666)
alpha<-seq(0.1,0.9,0.05)
train.X<-as.matrix(train[,2:ncol(train)])
train.Y<-train[,1]
search<-foreach(i=alpha, .combine=rbind) %dopar% {
  cv<-cv.glmnet(train.X, train.Y, family="binomial", nfold=10, type.measure="auc", paralle=TRUE, alpha=i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
best<-search[search$cvm==min(search$cvm),]
best
cv<-cv.glmnet(train.X, train.Y, family="binomial", nfold=10, type.measure="auc", paralle=TRUE, alpha=best$alpha)
plot(cv)
# modelling with best parameters
elnet<-glmnet(train.X, train.Y, family="binomial", lambda = best$lambda.1se, alpha = best$alpha)
coef(elnet)
# variable importance
train.X<-train[,-which(names(train)=="seriousdlqin2yrs")]
train.Y<-as.numeric(train$seriousdlqin2yrs)
library(DALEXtra)
explainer_elnet <- explain_tidymodels(
  model=elnet,
  data=train.X,
  y=train.Y-1,
  label="Elastic Net",
  type = "classification"
)
vip_elnet_auc <- model_parts(explainer_elnet,loss_function = loss_one_minus_auc) #metric is 1-auc
plot(vip_elnet_auc)


### By using random forest
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
ranger_rsample <- vfold_cv(train,v=10)

# Perform tuning
ranger_tune <-
  tune_grid(ranger_workflow, resamples = ranger_rsample, 
            grid = 50, control=control_grid(verbose=TRUE))

ranger_best<-select_best(ranger_tune,metric="roc_auc")
ranger_best

last_ranger_spec <- 
  rand_forest(mtry=ranger_best$mtry,
              min_n =ranger_best$min_n,
              trees = ranger_best$trees) %>% 
  set_mode("classification") %>% 
  set_engine("ranger") 

last_ranger_workflow <- 
  ranger_workflow %>% 
  update_model(last_ranger_spec)

# Fit
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
