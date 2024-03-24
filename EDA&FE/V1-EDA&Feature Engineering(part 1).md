# Exploratory Data Analysis (EDA)


```r
if (!requireNamespace("readr", quietly = TRUE)) {install.packages("readr")}
if (!requireNamespace("rpart", quietly = TRUE)) {install.packages("rpart")}
if (!requireNamespace("rpart.plot", quietly = TRUE)) {install.packages("rpart.plot")}
if (!requireNamespace("dlookr", quietly = TRUE)) {install.packages("dlookr")}
if (!requireNamespace("tidyverse", quietly = TRUE)) {install.packages("tidyverse")}
if (!requireNamespace("conflicted", quietly = TRUE)) {install.packages("conflicted")}
if (!requireNamespace("ggcorrplot", quietly = TRUE)) {install.packages("ggcorrplot")}
if (!requireNamespace("patchwork", quietly = TRUE)) {install.packages("patchwork")}
library(readr)
library(rpart)
library(rpart.plot)
library(dlookr)
```

```
## 
## Attaching package: 'dlookr'
```

```
## The following object is masked from 'package:base':
## 
##     transform
```

```r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.3     ✔ purrr     1.0.2
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
```

```
## ── Conflicts ────────────────────────────────────── tidyverse_conflicts() ──
## ✖ tidyr::extract() masks dlookr::extract()
## ✖ dplyr::filter()  masks stats::filter()
## ✖ dplyr::lag()     masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(conflicted)
conflicts_prefer("tidyr"::extract, "tidyr"::extract)
```

```
## [conflicted] Will prefer tidyr::extract over any other package.
## [conflicted] Removing existing preference.
## [conflicted] Will prefer tidyr::extract over any other package.
```

```r
conflicts_prefer("dplyr"::filter, "dplyr"::lag)
```

```
## [conflicted] Will prefer dplyr::filter over any other package.
## [conflicted] Will prefer dplyr::lag over any other package.
```

```r
library(ggcorrplot)
library(patchwork)
setwd("/Users/cailiying/Desktop/cuhk bsc/23:24/23:24 T2/STAT3011/Project2")
df = read_csv("cs-training.csv")
```

```
## New names:
## Rows: 150000 Columns: 12
## ── Column specification
## ──────────────────────────────────────────────────── Delimiter: "," dbl
## (12): ...1, SeriousDlqin2yrs, RevolvingUtilizationOfUnsecuredLines, ...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this
## message.
## • `` -> `...1`
```

# Descriptive Statistics


```r
#Computes the statistics of all numerical variables 
#describe(df)
#In SQL
if (!requireNamespace("RSQL", quietly = TRUE)) {install.packages("RSQL")}
if (!requireNamespace("RSQLite", quietly = TRUE)) {install.packages("RSQLite")}
library(RSQL) #Generate and Process 'SQL' Queries in R
library(RSQLite)
con <- dbConnect(drv = RSQLite::SQLite(),
                 dbname = ":memory:")
dbListTables(con)
```

```
## character(0)
```

```r
training_data = read.csv("cs-training.csv")
#Drop the column (Unnamed: 0)
training_data <- training_data[,-1]

#Clean the column names
colnames(training_data) <- gsub("[-,.]", "", tolower(colnames(training_data)))
head(training_data)
```

```
##   seriousdlqin2yrs revolvingutilizationofunsecuredlines age
## 1                1                            0.7661266  45
## 2                0                            0.9571510  40
## 3                0                            0.6581801  38
## 4                0                            0.2338098  30
## 5                0                            0.9072394  49
## 6                0                            0.2131787  74
##   numberoftime3059dayspastduenotworse  debtratio monthlyincome
## 1                                   2 0.80298213          9120
## 2                                   0 0.12187620          2600
## 3                                   1 0.08511338          3042
## 4                                   0 0.03604968          3300
## 5                                   1 0.02492570         63588
## 6                                   0 0.37560697          3500
##   numberofopencreditlinesandloans numberoftimes90dayslate
## 1                              13                       0
## 2                               4                       0
## 3                               2                       1
## 4                               5                       0
## 5                               7                       0
## 6                               3                       0
##   numberrealestateloansorlines numberoftime6089dayspastduenotworse
## 1                            6                                   0
## 2                            0                                   0
## 3                            0                                   0
## 4                            0                                   0
## 5                            1                                   0
## 6                            1                                   0
##   numberofdependents
## 1                  2
## 2                  1
## 3                  0
## 4                  0
## 5                  0
## 6                  1
```

```r
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
#describe=as.matrix(describe)
#calculate percentiles
percentiles <- apply(training_data, 2, quantile, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
percentiles = percentiles[,2:11]
describe=rbind(t(describe[,2:6]),percentiles)
#descriptive analysis
describe
```

```
##       revolvingutilizationofunsecuredlines          age
## count                         1.500000e+05 150000.00000
## mean                          6.048438e+00     52.29521
## std                           2.497554e+02     14.77187
## min                           0.000000e+00      0.00000
## max                           5.070800e+04    109.00000
## 25%                           2.986744e-02     41.00000
## 50%                           1.541807e-01     52.00000
## 75%                           5.590462e-01     63.00000
##       numberoftime3059dayspastduenotworse    debtratio monthlyincome
## count                        1.500000e+05 1.500000e+05    120269.000
## mean                         4.210333e-01 3.530051e+02      6670.221
## std                          4.192781e+00 2.037819e+03     14384.674
## min                          0.000000e+00 0.000000e+00         0.000
## max                          9.800000e+01 3.296640e+05   3008750.000
## 25%                          0.000000e+00 1.750738e-01      3400.000
## 50%                          0.000000e+00 3.665078e-01      5400.000
## 75%                          0.000000e+00 8.682538e-01      8249.000
##       numberofopencreditlinesandloans numberoftimes90dayslate
## count                    1.500000e+05            1.500000e+05
## mean                     8.452760e+00            2.659733e-01
## std                      5.145951e+00            4.169304e+00
## min                      0.000000e+00            0.000000e+00
## max                      5.800000e+01            9.800000e+01
## 25%                      5.000000e+00            0.000000e+00
## 50%                      8.000000e+00            0.000000e+00
## 75%                      1.100000e+01            0.000000e+00
##       numberrealestateloansorlines numberoftime6089dayspastduenotworse
## count                 1.500000e+05                        1.500000e+05
## mean                  1.018240e+00                        2.403867e-01
## std                   1.129771e+00                        4.155179e+00
## min                   0.000000e+00                        0.000000e+00
## max                   5.400000e+01                        9.800000e+01
## 25%                   0.000000e+00                        0.000000e+00
## 50%                   1.000000e+00                        0.000000e+00
## 75%                   2.000000e+00                        0.000000e+00
##       numberofdependents
## count       1.460760e+05
## mean        7.572223e-01
## std         1.115086e+00
## min         0.000000e+00
## max         2.000000e+01
## 25%         0.000000e+00
## 50%         0.000000e+00
## 75%         1.000000e+00
```

```r
#Drop the column (Unnamed: 0)
df <- df[,-1]

#Clean the column names
colnames(df) <- gsub("-", "", tolower(colnames(df)))
```

# Plot the distribution of target variable “seriousdlqin2yrs”


```r
#Calculate the total length of the training data
total_len <- length(df$seriousdlqin2yrs)

#Calculate the percentage of each category in the target label
percentage_labels <- prop.table(table(df$seriousdlqin2yrs)) * 100
#Print the percentage labels
print(percentage_labels)
```

```
## 
##      0      1 
## 93.316  6.684
```

```r
if (!requireNamespace("ggplot2", quietly = TRUE)) {install.packages("ggplot2")}
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
```

```
## Warning in geom_text(data = label_data, aes(label = paste0(label * 100, :
## Ignoring unknown aesthetics: fill
```

```r
#Display the plot
print(plot_with_labels)
```

```
## Warning: Stacking requires either the ymin and ymin or the y aesthetics
## ℹ Maybe you want `position = "identity"`?
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Plot every variables


```r
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))

df_numeric %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +  
  facet_wrap(~variable, scales = "free", ncol = 5) +  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  
```

```
## Warning: Removed 33655 rows containing non-finite values (`stat_bin()`).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
#Many variables are very unevenly distributed
```

# MISSING VALUES 


```r
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
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
#There are two variables "monthly income" and "number of dependents" with missing values.

#Fill in the missing values of numberofdependents and calculate the average values according to age groups.
table(df$age)
```

```
## 
##    0   21   22   23   24   25   26   27   28   29   30   31   32   33   34 
##    1  183  434  641  816  953 1193 1338 1560 1702 1937 2038 2050 2239 2155 
##   35   36   37   38   39   40   41   42   43   44   45   46   47   48   49 
## 2246 2379 2521 2631 2987 3093 3122 3082 3208 3294 3502 3714 3719 3806 3837 
##   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64 
## 3753 3627 3609 3648 3561 3416 3589 3375 3443 3280 3258 3522 3568 3719 3058 
##   65   66   67   68   69   70   71   72   73   74   75   76   77   78   79 
## 2594 2494 2503 2235 1954 1777 1646 1649 1520 1451 1241 1183 1099 1054  981 
##   80   81   82   83   84   85   86   87   88   89   90   91   92   93   94 
##  876  774  647  512  480  483  407  357  313  276  198  154   93   87   47 
##   95   96   97   98   99  101  102  103  105  107  109 
##   45   18   17    6    9    3    3    3    1    1    2
```

```r
#delete age=0, change age=21 to 22
df <- df[df$age != 0, ]
df$age[df$age == 21] <- 22
table(df$age)
```

```
## 
##   22   23   24   25   26   27   28   29   30   31   32   33   34   35   36 
##  617  641  816  953 1193 1338 1560 1702 1937 2038 2050 2239 2155 2246 2379 
##   37   38   39   40   41   42   43   44   45   46   47   48   49   50   51 
## 2521 2631 2987 3093 3122 3082 3208 3294 3502 3714 3719 3806 3837 3753 3627 
##   52   53   54   55   56   57   58   59   60   61   62   63   64   65   66 
## 3609 3648 3561 3416 3589 3375 3443 3280 3258 3522 3568 3719 3058 2594 2494 
##   67   68   69   70   71   72   73   74   75   76   77   78   79   80   81 
## 2503 2235 1954 1777 1646 1649 1520 1451 1241 1183 1099 1054  981  876  774 
##   82   83   84   85   86   87   88   89   90   91   92   93   94   95   96 
##  647  512  480  483  407  357  313  276  198  154   93   87   47   45   18 
##   97   98   99  101  102  103  105  107  109 
##   17    6    9    3    3    3    1    1    2
```

```r
#categorize age into five age_group: 20-29, 30-39, 40-49, 50-59, 60+
df$age_group<-cut(df$age, breaks = c(20, 30, 40, 50, 60, Inf), 
                  labels = c("20-29", "30-39", "40-49", "50-59", "60+"),
                  right = FALSE)
table(df$age_group)
```

```
## 
## 20-29 30-39 40-49 50-59   60+ 
##  8820 23183 34377 35301 48318
```

```r
#Calculate the average number of dependents for each age group
df_dependents <- df %>%
  group_by(age_group) %>%
  summarise(
    avg_number_of_dependents = round(mean(numberofdependents, na.rm = TRUE))
  )
print(df_dependents)
```

```
## # A tibble: 5 × 2
##   age_group avg_number_of_dependents
##   <fct>                        <dbl>
## 1 20-29                            0
## 2 30-39                            1
## 3 40-49                            1
## 4 50-59                            1
## 5 60+                              0
```

```r
#Merge the average values back into the original data frame
df <- df %>%
  left_join(df_dependents, by = "age_group") %>%
  mutate(
    numberofdependents = ifelse(is.na(numberofdependents), avg_number_of_dependents, 
                                numberofdependents),
  ) %>%
  #Remove the temporary average columns
  select(-avg_number_of_dependents) 
sum(is.na(df$numberofdependents))
```

```
## [1] 0
```

# OUTLIER DETECTION (for continuous variables)


```r
#Boxplot
df_numeric <- df %>% 
  ##select all numeric column
  select(c(debtratio, monthlyincome, revolvingutilizationofunsecuredlines))
df_numeric %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=variable, y=value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free", ncol = 5) +
  theme_bw()
```

```
## Warning: Removed 29731 rows containing non-finite values
## (`stat_boxplot()`).
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

Detect Outlier Methods: 1.Percentile 2.Median Absolute Deviation (MAD) 3.Standard deviation


```r
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
```

Debtratio Outlier


```r
debtratio_outlier <- outlierVote(df$debtratio)
sum(debtratio_outlier)
```

```
## [1] 31311
```

```r
summary(df[debtratio_outlier==TRUE,"debtratio"])
```

```
##    debtratio       
##  Min.   :     1.9  
##  1st Qu.:   160.0  
##  Median :  1179.0  
##  Mean   :  1689.9  
##  3rd Qu.:  2383.0  
##  Max.   :329664.0
```

```r
summary(df[debtratio_outlier==FALSE,"debtratio"])
```

```
##    debtratio     
##  Min.   :0.0000  
##  1st Qu.:0.1319  
##  Median :0.2841  
##  Mean   :0.3349  
##  3rd Qu.:0.4597  
##  Max.   :1.9068
```

```r
# Replace the debtratio outlier
# all the debtratio outliers are above normal values, we use the max normal value to replace them
df[debtratio_outlier==TRUE, "debtratio"] <- max(df[debtratio_outlier==FALSE, "debtratio"])
summary(df$debtratio)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.1751  0.3665  0.6630  0.8683  1.9068
```

Monthly income outlier


```r
# Since there exists missing values in monthlyincome, we fill them first
sum(is.na(df$monthlyincome))
```

```
## [1] 29731
```

```r
# Findings: A majority of 'DebtRatio' outliers correspond to rows with missing 'Monthly Income'
if (!requireNamespace("VennDiagram", quietly = TRUE)) {install.packages("VennDiagram")}
library(VennDiagram)
```

```
## Loading required package: grid
```

```
## Loading required package: futile.logger
```

```r
monthlyincome_missing <- is.na(df$monthlyincome)
#Identify how many rows of data contain both debtratio outliers and monthly income missing values
overlapping <- debtratio_outlier & monthlyincome_missing
# Find the number of overlaps
total_outliers <- sum(debtratio_outlier)
total_missing <- sum(monthlyincome_missing)
total_overlapping <- sum(overlapping)
print(paste("Total overlapping rows:", total_overlapping))
```

```
## [1] "Total overlapping rows: 27904"
```

```r
# Prepare Venn diagram data
category_list <- list(
  DebtratioOutliers = which(debtratio_outlier),
  MonthlyIncomeMissing = which(monthlyincome_missing)
)
# Draw Venn diagram
dev.off()
```

```
## null device 
##           1
```

```r
venn.plot <- venn.diagram(
  x = category_list,
  category.names = c("Debtratio Outliers", "Monthly Income Missing"),
  filename = NULL,
  output = NULL, 
  fill = c("#FF9999", "#99CC99"),
  cex = 1.5,  
  cat.cex = 1.2,  
  cat.col = "blue",  
  margin = 0.05,
  lwd = 2,  
  lty = 'dotted'
)
grid.text(
  x = 0.5, 
  y = 0.95, 
  label = "Venn Diagram of Debtratio Outliers and MonthlyIncome Missing Values", 
  just = "center", 
  gp = gpar(fontsize = 16, fontface = "bold"))
grid.draw(venn.plot)

# There are two filling methods
# For debtratio outlier rows, fill monthlyincome values according to the average monthlyincome value of the high/low debtratio outlier groups
# Calculate the median of debtratio_outlier
median_debtratio_out <- median(df$debtratio[debtratio_outlier])
# High and Low debtratio group
low_debtratio_out <- df$debtratio < median_debtratio_out & debtratio_outlier
high_debtratio_out <- df$debtratio >= median_debtratio_out & debtratio_outlier
# Calculate the mean monthlyincome for high and low debtratio groups
avg_monthlyincome_out_low <- mean(df$monthlyincome[low_debtratio_out], na.rm = TRUE)
avg_monthlyincome_out_high <- mean(df$monthlyincome[high_debtratio_out], na.rm = TRUE)
# Fill in the missing values in the monthly income of the debt ratio outlier row
df$monthlyincome[is.na(df$monthlyincome) & low_debtratio_out] <- avg_monthlyincome_out_low
df$monthlyincome[is.na(df$monthlyincome) & high_debtratio_out] <- avg_monthlyincome_out_high

#For other missing value from monthlyincome, we fill them by the average monthlyincome values according to each age groups.
# calculate the average monthly income for each age group
df_monthlyincome <- df %>%
  group_by(age_group) %>%
  summarise(
    avg_monthly_income = mean(monthlyincome, na.rm = TRUE)
  )
print(df_monthlyincome)
```

```
## # A tibble: 5 × 2
##   age_group avg_monthly_income
##   <fct>                  <dbl>
## 1 20-29                  2696.
## 2 30-39                  4920.
## 3 40-49                  6274.
## 4 50-59                  6487.
## 5 60+                    5241.
```

```r
#Merge the average values back into the original data frame
df <- df %>%
  left_join(df_monthlyincome, by = "age_group") %>%
  mutate(
    monthlyincome = ifelse(is.na(monthlyincome), avg_monthly_income, monthlyincome)
  ) %>%
  #Remove the temporary average columns
  select(-avg_monthly_income) 
sum(is.na(df$monthlyincome))
```

```
## [1] 0
```

```r
# Detect monthly income outlier
monthlyincome_outlier <- outlierVote(df$monthlyincome)
sum(monthlyincome_outlier)
```

```
## [1] 2557
```

```r
summary(df[monthlyincome_outlier==FALSE,"monthlyincome"])
```

```
##  monthlyincome  
##  Min.   :    0  
##  1st Qu.: 1700  
##  Median : 4400  
##  Mean   : 5005  
##  3rd Qu.: 7125  
##  Max.   :19022
```

```r
summary(df[monthlyincome_outlier==TRUE,"monthlyincome"])
```

```
##  monthlyincome    
##  Min.   :  19036  
##  1st Qu.:  20834  
##  Median :  25000  
##  Mean   :  38247  
##  3rd Qu.:  34000  
##  Max.   :3008750
```

```r
# all the monthlyincome outliers are above normal values, we use the max normal value to replace them
df[monthlyincome_outlier==TRUE, "monthlyincome"] <- max(df[monthlyincome_outlier==FALSE, "monthlyincome"])
summary(df$monthlyincome)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    1800    4500    5244    7400   19022
```

revolvingutilizationofunsecuredlines outlier


```r
revolvingutilizationofunsecuredlines_outlier <- outlierVote(df$revolvingutilizationofunsecuredlines)
sum(revolvingutilizationofunsecuredlines_outlier)
```

```
## [1] 763
```

```r
summary(df[revolvingutilizationofunsecuredlines_outlier==FALSE,"revolvingutilizationofunsecuredlines"])
```

```
##  revolvingutilizationofunsecuredlines
##  Min.   :0.00000                     
##  1st Qu.:0.02958                     
##  Median :0.15189                     
##  Mean   :0.31699                     
##  3rd Qu.:0.54929                     
##  Max.   :1.35216
```

```r
summary(df[revolvingutilizationofunsecuredlines_outlier==TRUE,"revolvingutilizationofunsecuredlines"])
```

```
##  revolvingutilizationofunsecuredlines
##  Min.   :    1.35                    
##  1st Qu.:    1.54                    
##  Median :    1.97                    
##  Mean   : 1127.08                    
##  3rd Qu.:  753.50                    
##  Max.   :50708.00
```

```r
# all the revolvingutilizationofunsecuredlines outliers are above normal values, we use the max normal value to replace them
df[revolvingutilizationofunsecuredlines_outlier==TRUE, "revolvingutilizationofunsecuredlines"] <- max(df[revolvingutilizationofunsecuredlines_outlier==FALSE, "revolvingutilizationofunsecuredlines"])
summary(df$revolvingutilizationofunsecuredlines)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00000 0.02987 0.15418 0.32225 0.55904 1.35216
```

```r
# Plot outliers
sample_data <- df$debtratio[sample(nrow(df), 5000)]
plotOutlier(sample_data)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
sample_data <- df$monthlyincome[sample(nrow(df), 5000)]
plotOutlier(sample_data)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-2.png)

```r
sample_data <- df$revolvingutilizationofunsecuredlines[sample(nrow(df), 5000)]
plotOutlier(sample_data)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-3.png)

# CORRELATION


```r
df_numeric <- df %>% 
  ##select all numeric column
  select(where(is.numeric))
cor <- cor(df_numeric)
p_mat <- ggcorrplot::cor_pmat(cor)
ggcorrplot::ggcorrplot(cor,p.mat = p_mat,lab = TRUE)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

# Feature Engineering
# Process variables
Process the three variables 'numberoftime3059dayspastduenotworse', 'numberoftime6089dayspastduenotworse', 'numberoftimes90dayslate'


```r
# Using Decision tree
#Calculate the total number of past due occurrences and proportion of each of tree pastduedate variable
#Create a new column 'category_pastdue' to store the classification
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



# assign new label
df$label <- ifelse(df$prop_3059 == 0 & df$prop_6089 == 0 & df$prop_90plus == 0, 0,
                   ifelse(df$prop_3059 > df$prop_6089 | df$prop_3059 > df$prop_90plus, 1, 2))
df$label <- as.factor(df$label)
# plot random forest
model <- rpart(label ~ prop_3059 + prop_6089 + prop_90plus, data = df)
par(mar = c(1, 1, 1, 1))
rpart.plot(model)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
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
                                      if (prop_3059 < 0.059) {
                                        if (prop_90plus < 0.071) {
                                          if (prop_6089 < 0.5) {
                                            return(0)
                                          } else {
                                            return(2)
                                          }
                                        } else {
                                          return(2)
                                        }
                                      } else {
                                        if (prop_3059 >= 0.35) {
                                          return(1)
                                        } else {
                                          if (prop_6089 < 0.11) {
                                            return(1)
                                          } else {
                                          return(2)
                                            }
                                        }
                                      }
                                    })
df$category_pastdue <- as.factor(df$category_pastdue)
table(df$category_pastdue)
```

```
## 
##      0      1      2 
## 119637  22590   7772
```

```r
#remove three original columns and label column
df <- subset(df, select = -c(label, numberoftime3059dayspastduenotworse, numberoftime6089dayspastduenotworse, numberoftimes90dayslate))
```

Process "numberofdependents"


```r
summary(df$numberofdependents)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.0000  0.7467  1.0000 20.0000
```

```r
table(df$numberofdependents)
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10    13 
## 89426 27716 19521  9483  2862   746   158    51    24     5     5     1 
##    20 
##     1
```

```r
df$dependents_groups <- cut(df$numberofdependents,
                            breaks = c(0, 1, 2, 3, Inf),
                            labels = c("0", "1", "2", "3+"),
                            right = FALSE)
table(df$dependents_groups)
```

```
## 
##     0     1     2    3+ 
## 89426 27716 19521 13336
```

Process numberrealestateloansorlines


```r
summary(df$numberrealestateloansorlines)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   0.000   1.000   1.018   2.000  54.000
```

```r
table(df$numberrealestateloansorlines)
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10    11 
## 56188 52338 31521  6300  2170   689   320   171    93    78    37    23 
##    12    13    14    15    16    17    18    19    20    21    23    25 
##    18    15     7     7     4     4     2     2     2     1     2     3 
##    26    29    32    54 
##     1     1     1     1
```

```r
df$rsll_groups <- cut(df$numberrealestateloansorlines, 
                      breaks = c(0, 1, 2, 3,Inf),
                      labels = c("0", "1", "2", "3+"),
                      right = FALSE)
table(df$rsll_groups)
```

```
## 
##     0     1     2    3+ 
## 56188 52338 31521  9952
```

Process numberofopencreditlinesandloans


```r
summary(df$numberofopencreditlinesandloans)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   8.000   8.453  11.000  58.000
```

```r
table(df$numberofopencreditlinesandloans)
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10    11 
##  1888  4438  6666  9058 11609 12931 13613 13245 12562 11355  9624  8321 
##    12    13    14    15    16    17    18    19    20    21    22    23 
##  7005  5667  4546  3645  3000  2370  1874  1433  1169   864   685   533 
##    24    25    26    27    28    29    30    31    32    33    34    35 
##   422   337   239   194   150   114    88    74    52    47    35    27 
##    36    37    38    39    40    41    42    43    44    45    46    47 
##    18     7    13     9    10     4     8     8     2     8     3     2 
##    48    49    50    51    52    53    54    56    57    58 
##     6     4     2     2     3     1     4     2     2     1
```

```r
df$ocll_quantile_groups <- cut(df$numberofopencreditlinesandloans, 
                               breaks = c(0,5, 8, 11, Inf),
                               labels = c("0-4", "5-7", "8-10", "11+"),
                               right = FALSE)
table(df$ocll_quantile_groups)
```

```
## 
##   0-4   5-7  8-10   11+ 
## 33659 39789 33541 43010
```

Plot Categoriacal variables


```r
df_chr_factor <- df %>% 
  select(where(~ is.character(.) || is.factor(.)))
df_chr_factor %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=value))+
  geom_bar()+
  facet_wrap(~variable,scales = "free")+
  theme_bw()
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

# Process Numeric Variables
Plot numeric variables


```r
df_numeric <- df %>% 
  ##select all numeric column
  select(c(debtratio, monthlyincome, revolvingutilizationofunsecuredlines))

df_numeric %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=value))+
  geom_histogram(bins=30)+
  facet_wrap(~variable,scales = "free")+
  theme_bw()
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

some of them is right skew. Try to take square root.


```r
df_numeric %>% select(
  debtratio,monthlyincome, revolvingutilizationofunsecuredlines
) %>% 
  pivot_longer(everything(),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=sqrt(value)))+
  geom_histogram(bins=30)+
  facet_wrap(~variable,scales = "free")+
  theme_bw()
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)

```r
df$sqrt_debtratio <- sqrt(df$debtratio)
df$sqrt_monthlyincome <- sqrt(df$monthlyincome)
df$sqrt_revolvingutilizationofunsecuredlines <- sqrt(df$revolvingutilizationofunsecuredlines)
#remove three original columns
df <- subset(df, select = -c(debtratio, monthlyincome, revolvingutilizationofunsecuredlines))

#Check boxplot again
df_numeric <- df %>% 
  ##select all numeric column
  select(c(sqrt_debtratio, sqrt_monthlyincome, sqrt_revolvingutilizationofunsecuredlines))
df_numeric %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=variable, y=value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme_bw()
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-2.png)

# Relation between response and predictors
Now we start to see the relation between response and predictors.


```r
df %>% select(
  sqrt_debtratio,sqrt_monthlyincome,sqrt_revolvingutilizationofunsecuredlines,seriousdlqin2yrs
) %>%
  pivot_longer(1:3,names_to = "variable",values_to = "value") %>% 
  ggplot(aes(x=seriousdlqin2yrs,y=value))+
  geom_boxplot()+
  facet_wrap(~variable,scales = "free")+
  theme_bw()
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)

```r
# Being more than 90 days past due appears to be associated with a high debt ratio, low monthly income and a high ratio of total unsecured debt to total unsecured lines of credit (RevolvingUtilizationOfUnsecuredLines)
```

Plot mosaic plot


```r
#the longer the horizontal axis represents a higher proportion, the longer the numerical axis represents a higher proportion of this subcategory within the category
if (!requireNamespace("devtools", quietly = TRUE)) {install.packages("devtools")}
if (!requireNamespace("ggmosaic", quietly = TRUE)) {install.packages("ggmosaic")}
if (!requireNamespace("patchwork", quietly = TRUE)) {install.packages("patchwork")}
library(devtools)
```

```
## Loading required package: usethis
```

```r
library(ggmosaic)
library(patchwork)
df$age_group <- as.character(df$age_group)
df$category_pastdue <- as.character(df$category_pastdue)
graph <- df %>% select(c(age_group, dependents_groups, rsll_groups, ocll_quantile_groups, category_pastdue, seriousdlqin2yrs)) %>% 
  pivot_longer(-seriousdlqin2yrs,names_to = "variable",values_to = "value") %>% 
  group_by(variable) %>% nest() %>% ungroup() %>% 
  mutate(
    graph=map2(data,variable,~ggplot(.x,)+geom_mosaic(aes(x=product(seriousdlqin2yrs),fill=value))+theme_bw()+theme(legend.position = "none")+scale_color_binned()+labs(title = .y[[1]],y=""))
  ) %>%  select(graph) %>% as.list() 

wrap_plots(graph$graph,ncol = 2)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)

# Convert categorical variables


```r
df$age_group <- as.factor(
  ifelse(df$age_group == "20-29", 1,
         ifelse(df$age_group == "30-39", 2,
                ifelse(df$age_group == "40-49", 3,
                       ifelse(df$age_group == "50-59", 4, 5)))))
table(df$age_group)
```

```
## 
##     1     2     3     4     5 
##  8820 23183 34377 35301 48318
```

```r
df$dependents_groups <- as.factor(
  ifelse(df$dependents_groups == "0", 0, 
         ifelse(df$dependents_groups == "1", 1,
                ifelse(df$dependents_groups == "2", 2, 3)))
)
table(df$dependents_groups)
```

```
## 
##     0     1     2     3 
## 89426 27716 19521 13336
```

```r
df$rsll_groups <- as.factor(
  ifelse(df$rsll_groups == "0", 0, 
         ifelse(df$rsll_groups == "1", 1,
                ifelse(df$rsll_groups == "2", 2, 3)))
)
table(df$rsll_groups)
```

```
## 
##     0     1     2     3 
## 56188 52338 31521  9952
```

```r
df$ocll_quantile_groups <- as.factor(
  ifelse(df$ocll_quantile_groups == "0-4", 0, 
         ifelse(df$ocll_quantile_groups == "5-7", 1,
                ifelse(df$ocll_quantile_groups == "8-10", 2, 3)))
)
table(df$ocll_quantile_groups)
```

```
## 
##     0     1     2     3 
## 33659 39789 33541 43010
```

```r
df$category_pastdue <- as.factor(df$category_pastdue)
```

# write in csv


```r
write.csv(df, file="cs-training-processed-v1.csv", row.names=FALSE)
```

