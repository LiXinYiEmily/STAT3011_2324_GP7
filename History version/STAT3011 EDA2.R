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

# Create the countplot
plot <- ggplot(training_data, aes(x = seriousdlqin2yrs)) +
  geom_bar(fill = "steelblue") +
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
            aes(label = paste0(label * 100, "%")),
            y = max_height, vjust = -0.5, size = 5, position = position_stack(vjust = 0.5))

# Display the plot
print(plot_with_labels)


