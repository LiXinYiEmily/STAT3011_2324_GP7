library(rpart)
library(rpart.plot)
df = read.csv("C:/Users/Zhaoruotong/Downloads/cs-training.csv", header= TRUE)
#Drop the column (Unnamed: 0)
df <- df[,-1]
#Clean the column names
colnames(df) <- gsub("[-,.]", "", tolower(colnames(df)))
head(df)


total_past_due <-  df['numberoftime3059dayspastduenotworse'] + 
  df['numberoftime6089dayspastduenotworse'] + df['numberoftimes90dayslate']



df$prop_3059 <- apply(df[, c('numberoftime3059dayspastduenotworse', 
                          'numberoftime6089dayspastduenotworse', 
                          'numberoftimes90dayslate')], 1, function(x) {
                            # Calculate the total number of past due occurrences
                            total_past_due <- sum(x['numberoftime3059dayspastduenotworse'],
                                                  x['numberoftime6089dayspastduenotworse'],
                                                  x['numberoftimes90dayslate'])
                            
                            # Calculate the proportion for each time frame
                            prop_3059 <- ifelse(is.na(x['numberoftime3059dayspastduenotworse'] / total_past_due), 0, 
                                                x['numberoftime3059dayspastduenotworse'] / total_past_due)
                          })

df$prop_6089 <- apply(df[, c('numberoftime3059dayspastduenotworse', 
                          'numberoftime6089dayspastduenotworse', 
                          'numberoftimes90dayslate')], 1, function(x) {
                            # Calculate the total number of past due occurrences
                            total_past_due <- sum(x['numberoftime3059dayspastduenotworse'],
                                                  x['numberoftime6089dayspastduenotworse'],
                                                  x['numberoftimes90dayslate'])
                            
                            # Calculate the proportion for each time frame
                            prop_6089 <- ifelse(is.na(x['numberoftime6089dayspastduenotworse'] / total_past_due), 0, 
                                                x['numberoftime6089dayspastduenotworse'] / total_past_due)
                          })

df$prop_90plus <- apply(df[, c('numberoftime3059dayspastduenotworse', 
                          'numberoftime6089dayspastduenotworse', 
                          'numberoftimes90dayslate')], 1, function(x) {
                            # Calculate the total number of past due occurrences
                            total_past_due <- sum(x['numberoftime3059dayspastduenotworse'],
                                                  x['numberoftime6089dayspastduenotworse'],
                                                  x['numberoftimes90dayslate'])
                            
                            # Calculate the proportion for each time frame
                            prop_6089 <- ifelse(is.na(x['numberoftimes90dayslate'] / total_past_due), 0, 
                                                x['numberoftimes90dayslate'] / total_past_due)
                          })


labels <- rep(0, nrow(data))

# maximum proporation
df$label <- ifelse(df$prop_3059 == 0 & df$prop_6089 == 0 & df$prop_90plus == 0, 0,
                   ifelse(df$prop_3059 > df$prop_6089 | df$prop_3059 > df$prop_90plus, 1, 2))

model <- rpart(label ~ prop_3059 + prop_6089 + prop_90plus, data = df)
rpart.plot(model)
