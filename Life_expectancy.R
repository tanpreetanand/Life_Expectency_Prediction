#install.packages(c('ggplot','ggthemes','scales','dplyr','mice','randomForest','data.table','gridExtra','corrplot','GGally','e1071'))

#load data
df <- read.csv("Life_Expectancy_Data1.csv", fileEncoding = "UTF-8-BOM",header = T)

#df<-read.csv("MA317-MED/LifeExpectancyData1.csv", header=TRUE)
names(df)

#renaming the columns for readability
#load libraries
library(dplyr)
#using the function rename
df<-df%>%rename("Life.expectancy" = "SP.DYN.LE00.IN","Access.to.electricity" = "EG.ELC.ACCS.ZS","National.income" = "NY.ADJ.NNTY.KD.ZG","National.income.pc" = "NY.ADJ.NNTY.PC.KD.ZG",
                      "Children.hiv" = "SH.HIV.INCD.14","Children.primary" = "SE.PRM.UNER","Education.completed.primary" = "SE.PRM.CUAT.ZS",
                      "Education.atleast.Bachelors" = "SE.TER.CUAT.BA.ZS","Mortality.rate" = "SP.DYN.IMRT.IN","Primary.completion" = "SE.PRM.CMPT.ZS",
                      "Literacy.rate" = "SE.ADT.LITR.ZS","Real.interest.rate" = "FR.INR.RINR","Population.growth" = "SP.POP.GROW",
                      "Population.density" = "EN.POP.DNST","Population.total" = "SP.POP.TOTL", "Current.health.expenditure.pc" = "SH.XPD.CHEX.PC.CD", 
                      "Current.health.expenditure" = "SH.XPD.CHEX.GD.ZS","Unemployment" = "SL.UEM.TOTL.NE.ZS", "Gdp.growth" = "NY.GDP.MKTP.KD.ZG",
                      "Gdp.pc" = "NY.GDP.PCAP.CD","Birth.rate" = "SP.DYN.CBRT.IN","Renewable.energy" = "EG.FEC.RNEW.ZS","Adults.hiv" = "SH.HIV.INCD",
                      "Safe.drinking.water" = "SH.H2O.SMDW.ZS","Poverty.headcount" = "SI.POV.LMIC","Compulsory.education" = "SE.COM.DURS")

names(df)
summary(df)

### Descriptive Statistics (with variables' names)
library(stargazer)
stargazer(df, type = "text", title="Descriptive statistics", digits=1, out="table0.html")
str(df)


#visualizations of some variables
Life_Expectancy <- df$Life.expectancy   
hist(Life_Expectancy)

par(mfrow = c(3,4))
boxplot(df$Life.expectancy , main="Life Expectancy" , ylab = "years")
boxplot(df$Pri.Comp.Rate,main="Primary completion rate",ylab="% of total labor force") 
boxplot(df$Cur.Ht.Exp,main="Current health expenditure",ylab="% of GDP") 
boxplot(df$cur.Ht.PPP ,main="Current health expe per capita",ylab="current international $") 
boxplot(df$Unemp.Tot , main="Unemployment total",ylab="% of total labor force") 
boxplot(df$Mor.Rate.F,main="Mortality rate, adult, female",ylab="per 1,000 female adults") 
boxplot(df$Mor.Rate.M,main="Mortality rate, adult, male",ylab="per 1,000 male adults") 
boxplot(df$GDP.Grow.Per,main="GDP growth",ylab="annual %") 
boxplot(df$GDP.PPP,main="GDP per capita",ylab="current international $") 
boxplot(df$Bir.Rate ,main="Birth rate, crude",ylab="per 1,000 people") 
boxplot(df$GNI.PPP,main="GNI per capita",ylab="current international $") 
boxplot(df$Emp.Per,main="Employment to population ratio 15+",ylab="total (%)")

#Question-2####
#Missing values in data

#libraries
library(mice)

#count of missing values in the whole dataset
table(is.na(df))

#complete case analysis by default
#fit model on original data for comparison(without Renewable energy column)
model1 <- lm(Life.expectancy ~ National.income+National.income+National.income.pc+Children.hiv+Children.primary+Education.completed.primary+Education.atleast.Bachelors+Mortality.rate+Primary.completion+Literacy.rate+Real.interest.rate+Population.growth+Population.density+Population.total+Current.health.expenditure.pc+Current.health.expenditure+Unemployment+Gdp.growth+Gdp.pc+Birth.rate+Adults.hiv+Safe.drinking.water+Poverty.headcount+Compulsory.education,data=df)
summary(model1)

# Building the model without preprocessing, without removing NA's, the analysis shows NAs
#complete case analysis is not a feasible method to deal with the 
#missing values in this dataset
library(olsrr)
library(leaps)
dftest <- df[complete.cases(df),]
str(dftest)
head(dftest)


#There are 0 rows available without any missing values, henace rejecting the complete case.

#response variable
#life expectancy at birth
#The statistic "Life expectancy at birth" refers to the average 
#number of years a newborn is expected to live if mortality patterns at 
#the time of its birth remain constant in the future.
#response variable
y<-df$Life.expectancy
#missing values in response variable
table(is.na(y))
#there is one missing value in y
#drop from data 1 and add to data2 to predict after model is fitted
#drop the case with missing value in y after imputation of missing values 
#in predictor variables 

#predictor variables
x <- df[ ,c(-4)]
names(x)
x
#column names

#number of missing values in each predictor variable
sapply(x, function(x) sum(is.na(x)))

#percentage of missing values in each predictor variable
apply(x, 2, function(col)sum(is.na(col))/length(col))

#number of missing values in each row
rowSums(is.na(x))

# TODO - We will replace below plot with md.pattern
# plot of missing value information
image(is.na(x), axes = FALSE, col = gray(1:0))
axis(2, at = 1:28/28, labels = colnames(x))
axis(1, at = 1:217/217, labels = row.names(x), las = 2)


#single imputation
#single imputation using mean or regression is not used as they impute a single value
#and decreases the variability in the data
#multiple imputation is used to overcome the drawbacks of single imputation method


#multiple imputation
#making any changes to the data on a copy of the original data
df1<-df

#removing the country name and country code and renewable energy column from the data for imputation
df1<-df1[,-c(1,2,25)]
names(df1)

md.pattern(df1)

# Change the categorical value to numeric in the Continent column
df1$Continent <- as.numeric(as.factor(df1$Continent))
names(df1)
str(df1)

### Descriptive Statistics (with variables' names)
library(stargazer)
stargazer(df1, type = "text", title="Descriptive statistics", digits=1, out="table1.html")
str(df1)

###stage 1: Imputation
#imputations<-mice(df1);

imputations<-mice(df1, method = "cart")

print(imputations)

#checking the m imputed datasets 
complete(imputations)
#complete(imp,m) where m is the number of iterations
complete(imputations,2)

#checking the imputed values
imputations$imp

#plots showing distribution of data for m imputations of all variables 
stripplot(imputations, pch = 20, cex = 1.2)

#plots showing distribution of data using any two variables for m imputations
#xyplot(imputations, literacy.rate~Access.to.electricity  | .imp, pch = 20, cex = 1.4)

###stage 2:Analysis
#fit model using imputed data
model.fit <- with(imputations, lm(Life.expectancy ~ Continent+Access.to.electricity+National.income+National.income+National.income.pc+Children.hiv+Children.primary+Education.completed.primary+Education.atleast.Bachelors+Mortality.rate+Primary.completion+Literacy.rate+Real.interest.rate+Population.growth+Population.density+Population.total+Current.health.expenditure.pc+Current.health.expenditure+Unemployment+Gdp.growth+Gdp.pc+Birth.rate+Adults.hiv+Safe.drinking.water+Poverty.headcount+Compulsory.education))

#summary of the fitted model
summary(model.fit)
#TODO : watch lab 4 pdf and video, to write the summary in report.

#t-test to compare  the mean values of two variables
#model.fit2 <- with(imputations, t.test(Continent ~ National.income))
#model.fit2
##???


###stage 3:Pooling
#combining the m estimates of the coefficients and of the standard error obtained previously
#TODO - Watch the Video to interpret the results. We may need to remove high p-value feature and fit the model again.
pooled.model<-pool(model.fit)
summary(pooled.model)
pool.r.squared(model.fit)

#final imputed data to use for further analysis and modelling
#using the imputed dataset from the 5th iteration
df2<-complete(imputations,5)
names(df2)
#plots showing distribution of data for m imputations of all variables 
stripplot(imputations, pch = 10, cex = 1.2)

#---- MODIFY 

#replace imputed response variable,y or Life.Exp with the original data
#we are not going to use the imputed data for response variable 
#because the task is to predict it. We only used to help impute predictor variables
#df2$Life.expectancy<-df1$Life.expectancy


#as observed earlier, response variable has one missing value
#the response value of the case will be predicted in the later stage, (as a part of test data)
#pred_df<-df2[is.na(df2$Life.expectancy),]
#pred_df

#the case with missing value in response will be dropped from train data
#df2<-na.omit(df2)

#sapply(df2, function(df2) sum(is.na(df2)))

#Question-3####


#Checking for collinearity
#One way to check for collinearity is through variance inflation factor (VIF)
#which uses the coefficient of determination R2 to calculate how much the variance of the regression 
#coefficients is elevated due to strong correlations between the predictor variables


#Checking for a model with all the available predictors is a good choice or not
#we are predicting life expectancy which is our response variable
#df2<-df2[,-1]

model1 <- lm(Life.expectancy ~ ., data=df2)

#summary of the model

summary(model1)

#The F-statistic and p-value from the F-test indicate that the model is statistically important, 
#only the intercept is significant at a 5% stage
#We check the pairwise correlation between all of the predictor variables before using the built-in R function vif() 
#to measure the VIF value of each predictor variable.


# removing life expactancy column, and assigning the features to X
X<-df2[,-2]
names(df2)
names(X)
data.corr<-cor(X)

round(data.corr,2)

library(corrplot)
corrplot.mixed(data.corr, lower.col = "black", number.cex = .7)

#We can see that there are some significant correlations in both pairwise correlations, 
#suggesting collinearity between some predictors.
#the variables government expenditure on education, Current health expenditure, Children out of school, 
#and  Mortality rate female are highly correlated
library(faraway)
vif(model1)
#Multi-colinearlity exists if VIF > 10
#Dropping the variable with highest VIF
#Dropping  Birth.rate since it has the highest VIF around 10.89

df3 <- subset(df2, select=-c(Birth.rate))

#performing the regression by dropping the above variable

model2 <- lm(Life.expectancy ~ ., data=df3)

summary(model2)

#Again checking VIF

vif(model2)

#Dropping  Mortality rate female since it has the highest VIF in this case

df4 <- subset(df3, select = -c(Access.to.electricity,Current.health.expenditure.pc, Gdp.pc))

#performing the regression by dropping the above variable

model3 <- lm(Life.expectancy ~ .  , data=df4)

summary(model3)

#Again checking VIF

vif(model3)

#Dropping  Mortality rate  since it has the highest VIF in this case

df5 <- subset(df4, select = -c( National.income,National.income.pc,Mortality.rate))

#performing the regression by dropping the above variable

model4 <- lm(Life.expectancy ~ .  , data=df5)

summary(model4)

vif(model4)

#Thus the step wise variable selection leads to elimination of multi-collinearity and
#Fitting the model with the optional number of explanatory variables 
#Running an F-test to compare some of the models above
# TODO - Watch the video to interprit the result and write in report
anova(model4,model1)

#Since The p-value for F statistics is less than our significance level, we reject the null hypothesis.


#TODO - adding back continent variable to data
#df5$Country.Code<-df$Country.Code

### Check for Heteroscedasticity ###
### Heteroskedasticity occurs when the variance for all observations in a data set are not the same. It is a violation of the ordinary least square assumption (var(yi)=var(ei)=Q^2)
library(lmtest)
bptest(model1) # Breusch-Pagan test is a mathematical method of detecting heteroskedasticity. 
# data:  model1
# BP = 34.511, df = 24, p-value = 0.076
# Result: If the p-value is less than the level of significance, means that heteroskedasticity is present, then the null hypothesis could be rejected. In our case 7.309e-06 < 0.05, we can reject the null hypothesis. Therefore, we can conclude that heteroskedasticity is present. 

#More info on :https://rpubs.com/cyobero/187387#:~:text=One%20informal%20way%20of%20detecting,plot%2C%20then%20heteroskedasticity%20is%20present.

#--Robust Linear Regression (Deal with heteroskedacity))----------------------------------
install.packages("robustbase")
library(robustbase)

# Run the Robust Linear Regression
lmrobfit <- lmrob(Life.expectancy ~ .,data = df2)

# summary of the model
summary(lmrobfit) # R-square = 0.9255

#more info:https://www.brodrigues.co/blog/2018-07-08-rob_stderr/
# Way of dealing with heteroskedasticity is to use the lmrob() function from the {robustbase} package. This package is quite interesting, and offers quite a lot of functions for robust linear, and nonlinear, regression models. Running a robust linear regression is just the same as with lm(). This however, gives you different estimates than when fitting a linear regression model. The estimates should be the same, only the standard errors should be different. This is because the estimation method is different, and is also robust to outliers.


#Question - 4
#Q 4 (A)
#for model selection (feature selection)
#r has a function called step() that does a model 'selection' based on AIC 
#out of the three methods 'forward' , 'backward' and 'stepwise' , we select stepwise for maximum efficiency
#AIC stans for Akaike Information Criteria 
#Smaller the AIC better the model fit 
#returns the best model fit 


#checking the data with all the variables , df2
head(df2)
names(df1)
#just checking the entire glm model , is life exp ~ . 
initial_model <- glm(Life.expectancy ~ . , data = df2)

#checking the quality of the model 
summary(initial_model)


#feature selection start
#stepwise selection 
selection <- step(initial_model , 
                  scope = list(lower = ~ 1 ) , 
                  data = df2 , 
                  direction = "both")
selection


#step(initial_model , direction = "both")
#viewing AIC
selection$anova


#Q 4 (B)
#evaluation of the model (the co-efficients are already given at the end of the selection step)
eval <- selection$coefficients
eval

summary(selection)

#defining best model
best_model <- glm(formula = Life.expectancy ~ Access.to.electricity + Mortality.rate + 
                    Population.growth + Population.density + Current.health.expenditure.pc + Birth.rate + Adults.hiv, data = df2)

summary(best_model)
best_model
#all good values


### TODO - Not applicable for our assignment ###
#### Q4 (C)
#can we use the above described model to predict?
#prediction step
#for this we need to load the test data and clean like the train data
test_data <- read.csv("LifeExpectancyData2.csv")
head(test_data)


#renaming like the train data
test_data <- test_data %>% rename("Acc.Ele"="EG.ELC.ACCS.ZS",
                                  "Adj.Nni.Per"="NY.ADJ.NNTY.KD.ZG","Adj.Nni.Const"="NY.ADJ.NNTY.KD",
                                  "Child.Sch.Per"="SE.PRM.UNER.ZS","Gov.Exp.Edu"="SE.XPD.PRIM.ZS", "Mor.rate"="SP.DYN.IMRT.IN", "Lit.Rate"="SE.ADT.LITR.ZS",
                                  "Pop.grow"="SP.POP.GROW","Pop.Tot"="SP.POP.TOTL","Pri.Comp.Rate"="SE.PRM.CMPT.ZS",
                                  "Cur.Ht.Exp"="SH.XPD.CHEX.GD.ZS","cur.Ht.PPP"="SH.XPD.CHEX.PC.CD",
                                  "Unemp.Tot"="SL.UEM.TOTL.NE.ZS","Mor.Rate.F"="SP.DYN.AMRT.FE","Mor.Rate.M"="SP.DYN.AMRT.MA",
                                  "GDP.Grow.Per"="NY.GDP.MKTP.KD.ZG","GDP.PPP"="NY.GDP.PCAP.PP.CD","Bir.Rate"="SP.DYN.CBRT.IN",
                                  "GNI.PPP"="NY.GNP.PCAP.PP.CD","Emp.Per"="SL.EMP.TOTL.SP.ZS")


#there are NA's , perform imputations again to get a complete dataset
dim(test_data)

image(is.na(test_data), axes = FALSE, col = gray(1:0))
axis(2, at = 1:22/22, labels = colnames(test_data))
axis(1, at = 1:11/11, labels = row.names(test_data), las = 2)

#copying test_data to new variable
test_data_x <- test_data[,-c(1,2)]
md.pattern(test_data_x)

#cleaning NA's , same method as train data , imputation , description under question 1
library(mice)
test_imputations <- mice(test_data_x , method = "cart")
test_imputations

print(test_imputations)
complete(test_imputations)
#complete(imp,m) where m is the number of iterations
complete(test_imputations,3)

#checking the imputed values
test_imputations$imp

#plots showing distribution of data for m imputations of all variables 
stripplot(test_imputations, pch = 20, cex = 1.2)

#plots showing distribution of data using any two variables for m imputations
xyplot(test_imputations, Adj.Nni.Per~Acc.Ele  | .imp, pch = 20, cex = 1.4)

test_data_x2 <- complete(test_imputations , 5)
head(test_data_x2)
#final test data

#checking predictions
#the function predict() uses the model to predict for a new data set 
#the first attempt with all variables 
predict(best_model , newdata = test_data_x2)

#still NA 's , reason could be high collinearity between the variable , hence trying to take only the predictor variables from the model

#trial and error with imputations


#defining new dataset after dropping collinear variables
test_again <- test_data[, c(4,8,10,13,14,16,17,19,20)]
test_again
test_again_imp <- mice(test_again , method = "cart")
test_again_imp

#NA's even after imputations arise as a result of , collinearity between some of the variables
#from the best_model description , GDP.PPP has lesser p value than GNI.PPP ,hence dropping GNI.PPP
#also Pri.Comp.Rate , cuz p value = 0.1 and also collinear and redifining the data for imputation

#finalising the data set for prediction
test_imp_data <- complete(test_again_imp , 5)
test_imp_data

#checking the dataset
stripplot(test_again_imp, pch = 20, cex = 1.2)

#modifying model for the predictions after imputations
prediction_model <- glm(formula = Life.exp ~ Adj.Nni.Per + Mor.rate + Pop.grow + 
                          Cur.Ht.Exp + cur.Ht.PPP + Mor.Rate.F + Mor.Rate.M +
                          GDP.PPP + Bir.Rate, data = df2)
summary(prediction_model)

#predictions
final_predictions <- predict(prediction_model , newdata = test_imp_data)
final_predictions

#to add back the country codes from the test data
country_codes <- test_data[ , c(1,2)]
country_codes

#new dataset with the test data and the predictions  
prediction_dataset <- cbind(country_codes , final_predictions , test_imp_data)
prediction_dataset <- prediction_dataset %>% rename("Life.exp.predictions" = "final_predictions")
prediction_dataset

#writing to new .csv
write.csv(prediction_dataset , file = "Prediction dataset.csv")







###Q 5 ANOVA

install.packages("gplots")
install.packages("car")
install.packages("report")
install.packages("countrycode")
library(gplots)
library(car)
library(report) 
library(countrycode)

#df=read.csv("D:/MA317/Coursework2-Group Project/LifeExpectancyData1.csv")
#df
#View(df)

continent <- c(df$Continent)
unique(continent)
#View(country)
#class(country)

#Creating a new column "Continents" using the countrycode
#df$Continents=countrycode(sourcevar = country, origin = "country.name",destination = "continent")
#unique(df$Continents)
#view(df$Continents)



### Our code for Step 5
df$Continent[df$Continent == 1] <- "Australia/Oceania" # Na - country name: Solomon Islands which are in Oceania
df$Continent[df$Continent == 2] <- "Africa"
df$Continent[df$Continent == 3] <- "Asia" 
df$Continent[df$Continent == 4] <- "Australia/Oceania"
df$Continent[df$Continent == 5] <- "Europe"
df$Continent[df$Continent == 6] <- "North America"
df$Continent[df$Continent == 7] <- "South America"

# ANOVA
exp.aov <-aov(Life.expectancy ~ continent, data = df)
summary(exp.aov)
# P-value of F statistics indicates that Life expectancy is significantly different (<2e-16 *) across continents

#Pairwise comparison
an3=with(df, aov(Life.expectancy~as.factor(Continent)))
pwc <- TukeyHSD(an3)
pwc

install.packages("ggplot2")
library(ggplot2)

install.packages("corrplot")
library(corrplot)

install.packages("ggpubr")
library(ggpubr)

# Visualization: box plots
ggboxplot(df, x = "Continent", y = "Life.expectancy", add = "point") +
  coord_flip()

# Visualization: TukeyHSD family-wise comparison
df$Continent[df$Continent == "Australia/Oceania"] <- "A/O" # Na - country name: Solomon Islands which are in Oceania
df$Continent[df$Continent == "Africa"] <- "Af"
df$Continent[df$Continent == "Asia" ] <- "As"
df$Continent[df$Continent == "Australia/Oceania"] <- "A/O"
df$Continent[df$Continent == "Europe"] <- "Eu"
df$Continent[df$Continent == "North America"] <- "NA"
df$Continent[df$Continent == "South America"] <- "SA"

an3=with(df, aov(Life.expectancy~as.factor(Continent)))
pwc1 <- TukeyHSD(an3)
plot(pwc1, las=1)

###

### TODO - Previous code 
#Compute the analysis of variance
exp.aov <- aov(Life.expectancy ~ continent, data = df)
#Summary of the analysis
summary(exp.aov)     

#The Null hypothesis (H0) is the equity in all population means  
#Alternative hypothesis (H1) is a difference in at least one mean. 
#The p-value is lower than the usual threshold of 0.05. 
#We have strong evidence that there is statistical difference between the continents, indicated by the "*".
#So we reject the null hypothesis i.e. we reject that all means are same.

# Plot the mean of life exp  by continent groups
plotmeans(Life.exp ~ Continents, data = df, frame = FALSE,mean.labels = TRUE,main="Interval plot - Mean of life exp by continent groups")

#Normality visualization
par(mfrow = c(1, 2))    #combine plots
hist(exp.aov$residuals) #histogram
qqPlot(exp.aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)         #QQ-plot

report(exp.aov)

#Post-Hoc Analysis using Tukey method
TukeyHSD(exp.aov)
plot(TukeyHSD(exp.aov))