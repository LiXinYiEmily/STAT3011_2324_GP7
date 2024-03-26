Project Name:
Chapter 1. Credit Risk Modeling

_______________________________________________________________________________
Problem Statement

It is crucial for banks and money lenders to understand the customers’ risk and financial behaviours. A high prediction accuracy of loan default, that is, borrowers fail to make payments, helps businesses to make more responsible lending and pricing strategies and improve revenue by keeping losses low. 

In this project, we develop predictive models based on a number of features, including monthly income, debt ratio and so on, to access the payment behaviours and creditworthiness of borrowers and identify those who is likely to default.

In particular, the following questions will be addressed:

1.	What is the probability of borrowers experiencing serious delinquency (e.g., more than 90 days) in 2 years?

2.	Which features are the most important in assessing the customer’s risk of defaulting? How can we effectively incorporate these features into the models?

3.	What kind of techniques can be used to optimize the model performance?

Variables:
SeriousDlqin2yrs, RevolvingUtilizationOfUnsecuredLines, Age, NumberOfTime30-59DaysPastDueNotWorse, DebtRatio, MonthlyIncome, NumberOfOpenCreditLinesAndLoans, NumberOfTimes90DaysLate, NumberRealEstateLoansOrLines, NumberOfTime60-89DaysPastDueNotWorse, NumberOfDependents


____________________________________________________________________________
Part 1 EDA: [save in EDA&FE]
Find code in EDA&FE/EDA&FE(part 1)/Markdown-EDA&Feature Engineering(part 1).md

Understanding attributes of the dataset (Dataset：150,000 rows, 11 columns)
    Data Distribution: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-3-1.png
    Variable Sudoku: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-4-1.png
    Missing Data: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-5-1.png
    OUTLIER DETECTION: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-6-1.png
    Plot Outlier: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-10-1.png
                  EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-10-2.png
                  EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-10-3.png
    Correlation: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-11-1.png

___________________________________________________________________________
Part 2 Feature Engineering: [Save in EDA&FE]
Find code in EDA&FE/EDA&FE(part 1)/Markdown-EDA&Feature Engineering(part 1).md

How much each feature contributes to the model prediction
      Decision Tree: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-12-1.png
      variable Sudoku(after feature engineering): EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-16-1.png
      Process Numeric Variables: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-17-1.png
                                EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-18-1.png
      Boxplot: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-18-2.png
      Relation between response and predictors: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-19-1.png
      Plot mosaic plot: EDA&FE/EDA&FE(part 1)/figure/unnamed-chunk-20-1.png


___________________________________________________________________________
Part 3 Feature Importance: [Save in EDA&FE]
Find Code in EDA&FE/FE(part 2)-Feature Selection/Markdown-Feature Engineering(part 2).md

Feature Importance: which features are the most important in assessing the customer’s risk of defaulting;
      Elastic Net + Random Forest
      EDA&FE/FE(part 2)-Feature Selection/figure/unnamed-chunk-1-1.png
      EDA&FE/FE(part 2)-Feature Selection/figure/unnamed-chunk-2-1.png

___________________________________________________________________________
Part 4 Models: [Save in models]

Ada Boosting & XGBoosting
      Ada Boosting can be found in models/STAT3011_project2_model_Ada+Gb.ipynb
      XGBBoosting can be found in models/xgboost.ipynb


_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
History Version: See in History Version
Data Source: See in data file 
Replication: See in replication file, replicated from the book Thanaki, J. (2018). Machine Learning Solutions (1st edition). Packt Publishing.




