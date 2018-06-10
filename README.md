# Time Series Project

Team members: Liz Chen, Beiming Liu, Bingyi Li, Shannon McNish

Goal: predict Canadian monthly bankruptcy rate

# Biref Intro

The goal of this project is to use one or more variables to accurately predict monthly Canadian bankruptcy rates from January 2011 to December 2012. Data available for modeling includes the monthly data from January 1987 to December 2010 for Canada for the following four variables:  Unemployment Rate, Population, Housing Price Index and Bankruptcy Rate.


Our approach includes an initial exploratory data analysis to get familiar with the relationship between variables to inform our model building. Next, we fit models using four different modeling approaches, both univariate and multivariate. For each approach, we tuned the appropriate parameters with the goal of maximizing prediction accuracy--in this case we measured that by minimizing root mean squared error (RMSE). RMSE measures differences between the bankruptcy rate the model predicted for the validation set to the actual bankruptcy rate given by the observed data. The validation set is a subset of data from January 2009 to December 2010 that is used to test model predictions, since this data includes the actual bankruptcy rate.


After producing optimal models for each approach, we evaluated each model’s predictions and RMSE. Our final model took a combination of the best two approaches to give our final predictions for Canada bankruptcy rates between 2011-2012.

# Report: 

The report summarized the feature selection and modeling process and our conclusions 
