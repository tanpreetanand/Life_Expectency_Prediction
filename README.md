# Life_Expectency_Prediction

•	Classified the life expectancy in various countries in 2019. 
•	Used Multivariate imputation by chained equations (MICE) and collinearity. 
•	Used Machine learning models for classification and finally ANOVA model was applied. 


Introduction : The project is based on the experimental analysis of Life Expectancy in various countries in the year 2019. Initailly the Preliminary 
               Analyssis and Data Visualization is done. In the Next steps, Data preprocessing and Missing Value Imputation is persformed with one 
               of a Imputation Method. Finally, The model building is done and results are compared to get the predictions. At last, ANOVA method 
               is implemneted to separate the obeserved variance dat into differnet components. 
               
Preliminary Ananlysis and Data Visualization : In this phase I have done Numerical and Graphical Analysis. From the given data we can find the whole 
   numerical analysis by just using the built in function called summary. The whole code is done in R Langage and everything is made easy by using 
   built in function. The Data visualization is done in order to find the insights and predictor variables.
   
Data Preprocessing and Missing Value Imputation : The step involves the transforming of data so that it may be easily interpreted by a computer. In 
   the dataset two of the columns are not numerical and lot of data is missing. I have used Machine Learning feature "Hashing" to make non-numerical 
   columns again numerical. I have also used the Multivariate imputation by chained equations (MICE) package to prform missing value imputation. 
   
Handling Collinearity :  Collinearity is the phenomena that occurs when two predictor variables in a multiple regression have a non-zero correlation, 
   multicollinearity is the effect that occurs when more than two variables are correlated with one another. There is a problem of multicollinearity 
   with the data and I appleied Variance inflation factor (VIF) to handle it. 
   
Model Builidng and Predicions : In this phase, the model is built to predict the life expectancy. First of all the outliers are removed. The data 
   was split to 75% train and 25% test and then built the model using Linear Regression and Random Forest Regressor. The Linear regression model is good
   but is not a good fit as the R2 value is around 84%. I tried using the Random Forest regressor where the data was divided manually and fitted the model 
   got an accuracy of around 87%.   
   
Experimental Design : In the last part, i tried to find the differnece in average life expectancy in various continents. I have used the ANOVA method to 
   study the difference between the Life expectancies. ANOVA stands for “Analysis of variance” and it is a statistical test which compares the means of 
   two groups to check if there is any difference. Anova test uses F-test which is group wise comparison test. The result shows us the p value is very 
   small which reflects that Life expectancies are significantly different in different continents.
   
Conclusion : There are multiple factors behind the life expectancies. In the above analysis, it is vivid that the life expectancy in developed countries 
   is higher than the developing nations. The random forest regressor reflected the best results with accuracy around 87%. The ANOVA method was applied 
   to get the life expectancies in various countries. To sum up, the Regressors are the best to capture the data for life expectancies and it can be used 
   in other fields also.   
               
               
               
