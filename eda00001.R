# Exploratory Data Analysis (EDA)
#-------------------------------------------------------------------------------
setwd("/Users/cailiying/Desktop/cuhk bsc/23:24/23:24 T2/STAT3011/Project2")
library(dlookr)
library(tidyverse)
library(ggcorrplot)
library(patchwork)
# Data Preprocessing
#Read the CSV file
df <- read_csv("cs-training.csv")
glimpse(df)
#Computes the statistics of all numerical variables 
describe(df)

#Drop the column (Unnamed: 0)
df <- df[,-1]

#Clean the column names
colnames(df) <- gsub("-", "", tolower(colnames(df)))

head(df)

#Compute descriptive statistics
df_subset <- df[, 2:ncol(df)]
properties <- summary(df_subset)
(std_dev <- apply(df_subset, 2, sd))
(non_missing_counts <- colSums(!is.na(df_subset)))
(means<-properties[4,])
(medians<-properties[3,])

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


# Plot every variables
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


# MISSING VALUES 
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

# CORRELATION
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))
cor <- cor(df_numeric,method = "spearman")
p_mat <- ggcorrplot::cor_pmat(cor)
ggcorrplot::ggcorrplot(cor,p.mat = p_mat,lab = TRUE)


# OUTLIER DETECTION
#Boxplot
df %>% select(
  debtratio, numberofopencreditlinesandloans, numberrealestateloansorlines,  
  numberofdependents, revolvingutilizationofunsecuredlines, numberoftime3059dayspastduenotworse,
  monthlyincome, numberoftimes90dayslate, numberoftime6089dayspastduenotworse
) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=variable, y=value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme_bw()

#Since the data contains a large number of outliers and is unevenly distributed, 
#it is better to use the MAD method to remove outliers.
percentile_based_outlier <- function(data, threshold = 95) {
  diff <- (100 - threshold) / 2.0
  minval <- quantile(data, diff / 100)
  maxval <- quantile(data, 1 - diff / 100)
  return((data < minval) | (data > maxval))
}
mad_outlier_location <- function(points, thresh = 3.5) {
  median_y <- median(points, na.rm = TRUE)
  mad <- median(abs(points - median_y), na.rm = TRUE)
  modified_z_scores <- 0.6745 * (points - median_y) / mad
  return(abs(modified_z_scores) > thresh)
}

#Remove some outliers using MAD method
debtratio_outlier <- mad_outlier_location(df$debtratio)
df <- df[!debtratio_outlier, ]

monthlyincome_outlier <- mad_outlier_location(matrix(df$monthlyincome, ncol = 1))
df <- df[!monthlyincome_outlier, ]

revolvingutilizationofunsecuredlines_outlier <- mad_outlier_location(matrix(df$revolvingutilizationofunsecuredlines, ncol = 1))
df <- df[!revolvingutilizationofunsecuredlines_outlier, ]

#Check boxplot again
df %>% select(
  debtratio, numberofopencreditlinesandloans, numberrealestateloansorlines,  
  numberofdependents, revolvingutilizationofunsecuredlines, numberoftime3059dayspastduenotworse,
  monthlyincome, numberoftimes90dayslate, numberoftime6089dayspastduenotworse
) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=variable, y=value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme_bw()

# Plot every variables
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))

df_numeric %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(~variable,scales = "free")+
  theme_bw()

# Process the three variables 'numberoftime3059dayspastduenotworse', 'numberoftime6089dayspastduenotworse', 'numberoftimes90dayslate'
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

# Process "numberofdependents"
df$categoryofdependents <- cut(df$numberofdependents,
                               breaks = c(-Inf, 0, 1, 2, Inf),
                               labels = c("0", "1", "2", "3+"),
                               right = FALSE)
df <- subset(df, select = -numberofdependents)

# continue to process other variables
#......

# Plot Categoriacal variables
df_chr <- df %>% 
  select(where(is.character))
df_chr %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=value))+
  geom_bar()+
  facet_wrap(~variable,scales = "free")+
  theme_bw()

# See correlation again
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))
cor <- cor(df_numeric,method = "spearman")
p_mat <- ggcorrplot::cor_pmat(cor)
ggcorrplot::ggcorrplot(cor,p.mat = p_mat,lab = TRUE)


# Now we start to see the relation between response and predictors.
#........





