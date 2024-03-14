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
