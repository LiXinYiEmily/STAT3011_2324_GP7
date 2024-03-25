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
install.packages("corrplot")  # Install corrplot package
library(corrplot)  # Load corrplot package

# Create a correlation matrix plot with labels
corrplot(correlation_matrix, method = "color", addCoef.col = "black")



#OUTLIER DETECTION
percentile_based_outlier <- function(data, threshold = 95) {
  diff <- (100 - threshold) / 2.0
  minval <- quantile(data, diff / 100)
  maxval <- quantile(data, 1 - diff / 100)
  return((data < minval) | (data > maxval))
}
mad_based_outlier <- function(points, thresh = 3.5) {
  if (length(dim(points)) == 1) {
    points <- matrix(points, ncol = 1)
  }
  median_y <- apply(points, 2, median)
  mad <- apply(points, 2, function(y) median(abs(y - median_y)))
  modified_z_scores <- lapply(points, function(y) {
    0.6745 * (y - median_y) / mad
  })
  modified_z_scores <- unlist(modified_z_scores)
  x <- c()
  for (i in 1:length(modified_z_scores)) {
    if (modified_z_scores[i]>3.5){
      x <- c(x, modified_z_scores[i])
    }
  }
  return(x)
}
points <- matrix(training_data$revolvingutilizationofunsecuredlines, ncol = 1)
outliers <- mad_based_outlier(points)
print(outliers)

