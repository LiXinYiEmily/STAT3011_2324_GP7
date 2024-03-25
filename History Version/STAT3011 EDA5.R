#EDA
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

head(training_data)

# Compute descriptive statistics
df <- training_data[, 2:ncol(training_data)]
properties <- summary(df)
(std_dev <- apply(df, 2, sd))
(non_missing_counts <- colSums(!is.na(df)))
(means<-properties[4,])
(medians<-properties[3,])

# Calculate the total length of the training data
total_len <- length(training_data$seriousdlqin2yrs)

# Calculate the percentage of each category in the target label
percentage_labels <- prop.table(table(training_data$seriousdlqin2yrs)) * 100

# Print the percentage labels
print(percentage_labels)

library(ggplot2)
training_data$seriousdlqin2yrs <- as.factor(training_data$seriousdlqin2yrs)

# Define custom colors for each bar
bar_colors <- c("steelblue", "darkgreen")

# Create the countplot with different colors for each bar
plot <- ggplot(training_data, aes(x = seriousdlqin2yrs, fill = seriousdlqin2yrs)) +
  geom_bar() +
  scale_fill_manual(values = bar_colors) +
  labs(title = "Data Distribution",
       x = "Labels for seriousdlqin2yrs attribute",
       y = "Numbers of records") +
  theme_bw()

# Create a data frame with label values for each bar
label_data <- data.frame(
  seriousdlqin2yrs = levels(training_data$seriousdlqin2yrs),
  label = c(93.316, 6.684) / 100  # Specify the label values for each bar as percentages
)

# Compute the maximum bar height
max_height <- max(table(training_data$seriousdlqin2yrs))

# Add labels to the plot
plot_with_labels <- plot +
  geom_text(data = label_data,
            aes(label = paste0(label * 100, "%"), fill = seriousdlqin2yrs),
            y = max_height, vjust = -0.5, size = 5, position = position_stack(vjust = 0.5))

# Display the plot
print(plot_with_labels)

colSums(is.na(training_data))



#MISSING VALUES
# Calculate the count of missing values in each attribute
missing_counts <- colSums(is.na(training_data))

# Create a data frame with x and y values
missing_data <- data.frame(attributes = names(missing_counts),
                           missing_count = missing_counts)

# Create the bar plot
bar_colors <- c("steelblue","black","darkgreen", "pink","blue","green","grey","yellow","purple","orange","darkorange")
plot <- ggplot(missing_data, aes(x = attributes, y = missing_count, fill = attributes)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = bar_colors) +
  labs(title = "Missing Data",
       x = "Data Attributes",
       y = "Count of Missing Records") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels if needed

# Add labels to the plot
plot_with_labels <- plot +
  geom_text(aes(label = missing_count), vjust = -0.5, size = 3)

# Display the plot
print(plot_with_labels)

# Install and load the 'zoo' package
install.packages("zoo")
library(zoo)

# Identify numeric columns
numeric_cols <- sapply(training_data, is.numeric)

# Replace missing values in numeric columns with column means
training_data_mean_replace <- training_data
training_data_mean_replace[, numeric_cols] <- na.aggregate(training_data[, numeric_cols], FUN = mean)

# Display the updated data frame
head(training_data_mean_replace)

colSums(is.na(training_data_mean_replace))

# Actual replacement of the missing value using median value.
training_data_median_replace <- training_data
training_data_median_replace[, numeric_cols] <- na.aggregate(training_data[, numeric_cols], FUN = median)
head(training_data_median_replace)

colSums(is.na(training_data_median_replace))



#CORRELATION
(correlation_matrix <- cor(training_data_median_replace[, 2:ncol(training_data)]))

# Load the 'stats' package
library(stats)

# Install and load the 'gplots' package
install.packages("gplots")
library(gplots)

# Create a heatmap using modified options
heatmap(correlation_matrix,
        col = colorRampPalette(c("white", "darkblue"))(256),
        main = "Correlation Matrix",
        cex.main = 1.5,   # Increase the font size of the main title
        cex.axis = 1.2,   # Increase the font size of the axes labels
        cex.lab = 1.2,    # Increase the font size of the colorbar labels
        keysize = 1.5,    # Increase the size of the colorbar
        key.title = "",   # Remove the colorbar title
        key.axes = FALSE) # Remove the colorbar axes


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
