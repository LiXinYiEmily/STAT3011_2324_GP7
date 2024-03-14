install.packages("pandas")
install.packages("numpy")
install.packages("seaborn")
install.packages("ggplot2")
# Read the CSV file
training_data <- read.csv("~/Desktop/大四/STAT 3011 cs-training.csv")

# Drop the column (Unnamed: 0)
training_data <- training_data[,-1]

# Clean the column names
colnames(training_data) <- gsub("-", "", tolower(colnames(training_data)))

########## Exploratory Data Analysis (EDA)
#-------------------------------------------------------------------------------
setwd("/Users/cailiying/Desktop/cuhk bsc/23:24/23:24 T2/STAT3011/Project2")
library(dlookr)
library(tidyverse)
library(ggcorrplot)
library(patchwork)
df <- read_csv("cs-training.csv")
glimpse(df)
# Drop the column (Unnamed: 0)
df <- df[,-1]
# Clean the column names
colnames(df) <- gsub("-", "", tolower(colnames(df)))

# Check for missing values in variables
colSums(is.na(df))
#Computes the statistics of all numerical variables 
describe(df)

df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))

df_numeric %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(~variable,scales = "free")+
  theme_bw()


sum_numberoftimeslate <- df$numberoftime3059dayspastduenotworse + 
  df$numberoftime6089dayspastduenotworse + df$numberoftimes90dayslate
df$prop_3059 <- df$numberoftime3059dayspastduenotworse/sum_numberoftimeslate
df$prop_6089 <- df$numberoftime6089dayspastduenotworse/sum_numberoftimeslate
df$prop_90plus <- df$numberoftimes90dayslate/sum_numberoftimeslate
# Replace 3 numberoftime columns' all NA values to 0
df[c('numberoftime3059dayspastduenotworse', 'numberoftime6089dayspastduenotworse', 'numberoftimes90dayslate')] <- 
  lapply(df[c('numberoftime3059dayspastduenotworse', 'numberoftime6089dayspastduenotworse', 'numberoftimes90dayslate')], 
         function(x) { replace(x, is.na(x), 0) })

# Create a new column 'category_pastdue' to store the classification
df$category_pastdue <- apply(df, 1, function(x) {
  # Calculate the total number of past due occurrences
  total_past_due <- x['numberoftime3059dayspastduenotworse'] + 
    x['numberoftime6089dayspastduenotworse'] + x['numberoftimes90dayslate']
  # Calculate the proportion for each time frame
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
    # Classify based on the proportions
    if (prop_3059 > 0.5) {
      return('Mostly 30-59 Days Past Due')
    } else if (prop_6089 > 0.3) {
      return('Mostly 60-89 Days Past Due')
    } else {
      return('Mixed Past Due')
    }
  }
})

table(df$category_pastdue)
# remove three columns
df <- subset(df, select = -c(numberoftime3059dayspastduenotworse, numberoftime6089dayspastduenotworse, numberoftimes90dayslate))

df_chr <- df %>% 
  select(where(is.character))
df_chr %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=value))+
  geom_bar()+
  facet_wrap(~variable,scales = "free")+
  theme_bw()
# have extreme values


