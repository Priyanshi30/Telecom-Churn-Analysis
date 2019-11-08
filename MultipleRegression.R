##Multivariate Project
##TELECOM-CHURN-ANALYSIS
##Author : Priyanshi Bajpai

##-----------------------------------------------------------------------------------
##Importing Libraries
##-----------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library (stringr)
library(data.table)
library(grid)
library(gridExtra)
library(corrplot)
library(scales)
library(qqplotr)
library(MASS)
library(DMwR)
library(car)
library(e1071)
library(caret)
library(caTools)
library(pROC)
library(tidyverse)
library(MVA)
library(GGally)
library(gvlma)



##-----------------------------------------------------------------------------------
##Importing Dataset and doing preliminary analysis
##-----------------------------------------------------------------------------------

#Importing CSV file from drive on my local computer and viewing it 

tablechurn<-read.csv("C:/Users/SHIVANSHI/Desktop/Priyanshi/MVA/Telecom Churn Analysis Data.csv")
tablechurn <- as.data.frame(tablechurn)
View(tablechurn)

#Checking the Dimension of the dataset

dim(tablechurn)

#Viewing the first 4 rows of the dataset to get the overview of the dataset

head(tablechurn,4)

#Gaining more insight about the kind of data stored in each column

summary(tablechurn)
glimpse(tablechurn)

#The above results give us an insight that TotalCharges and MonthlyCharges are numerical values
#SeniorCitizen and Tenure are stored as numerical which need to be converted to categorical variables


##-----------------------------------------------------------------------------------
## Performing Data Cleaning and Formatting
##-----------------------------------------------------------------------------------

#Converting SeniorCitizen numerical variable into Categorical Variable 

tablechurn$SeniorCitizen<-factor(tablechurn$SeniorCitizen,levels = c(0 ,1),labels = c('no','yes'))

#Converting tenure values into ranges of 12 months

tablechurn <- mutate(tablechurn,Tenure_Range = Tenure)
cut(tablechurn$Tenure_Range,6,labels = c('0-1 Years','1-2 Years','2-3 Years','4-5 Years','5-6 Years','6-7 Years'))
tablechurn$Tenure_Range <- cut(tablechurn$Tenure_Range,6,labels = c('0-1 Years','1-2 Years','2-3 Years','4-5 Years','5-6 Years','6-7 Years'))

#Checking if there are any NULL values in any of the columns  
table(is.na(tablechurn))
str_detect(tablechurn,'NA')
setDT(tablechurn)
tablechurn[is.na(TotalCharges),NROW(TotalCharges)]

#There are 11 rows out of 7043 rows that have null values.Hence removing these rows since they are only 0.15% of total so we can afford to drop them

tablechurn <- tablechurn[complete.cases(tablechurn), ]

#Replacing 'No Internet Service' values in OnlineSecurity,OnlineBackup DeviceProtection,TechSupport,StreamingTV and StreamingMovies columns with 'No'

tablechurn$OnlineSecurity[tablechurn$OnlineSecurity=='No internet service'] <- 'No'
tablechurn$OnlineBackup[tablechurn$OnlineBackup=='No internet service'] <- 'No'
tablechurn$DeviceProtection[tablechurn$DeviceProtection=='No internet service'] <- 'No'
tablechurn$TechSupport[tablechurn$TechSupport=='No internet service'] <- 'No'
tablechurn$StreamingTV[tablechurn$StreamingTV=='No internet service'] <- 'No'
tablechurn$StreamingMovies[tablechurn$StreamingMovies=='No internet service'] <- 'No'

#Deleting the unused levels from the factor variables

tablechurn$OnlineSecurity <- factor(tablechurn$OnlineSecurity)
tablechurn$OnlineBackup <- factor(tablechurn$OnlineBackup)
tablechurn$DeviceProtection <- factor(tablechurn$DeviceProtection)
tablechurn$TechSupport <- factor(tablechurn$TechSupport)
tablechurn$StreamingTV <- factor(tablechurn$StreamingTV)

#Converting Categorical Columns into numerical values 
tablechurn$Partner<-ifelse(tablechurn$Partner=='Yes', 1,2)
tablechurn$Dependents<-ifelse(tablechurn$Dependents=='Yes', 1,2)
tablechurn$PhoneService<-ifelse(tablechurn$PhoneService=='Yes', 1,2)
tablechurn$OnlineBackup<-ifelse(tablechurn$OnlineBackup=='Yes', 1,2)
tablechurn$OnlineSecurity<-ifelse(tablechurn$OnlineSecurity=='Yes', 1,2)
tablechurn$DeviceProtection<-ifelse(tablechurn$DeviceProtection=='Yes', 1,2)
tablechurn$TechSupport<-ifelse(tablechurn$TechSupport=='Yes', 1,2)
tablechurn$StreamingTV<-ifelse(tablechurn$StreamingTV=='Yes', 1,2)
tablechurn$StreamingMovies<-ifelse(tablechurn$StreamingMovies=='Yes', 1,2)
tablechurn$PaperlessBilling<-ifelse(tablechurn$PaperlessBilling=='Yes', 1,2)
tablechurn$Churn<-ifelse(tablechurn$Churn=='Yes', 1,2)

#Converting Categorical Columns into numerical values
tablechurn$Gender<-factor(tablechurn$Gender,levels = c('Female','Male'),labels = c(1,2))
tablechurn$MultipleLines<-factor(tablechurn$MultipleLines,levels = c('No','Yes','No phone service'),labels = c(1,2,3))
tablechurn$InternetService<-factor(tablechurn$InternetService,levels = c('No','Fiber optic','DSL'),labels = c(1,2,3))
tablechurn$Contract<-factor(tablechurn$Contract,levels = c('Month-to-month','One year','Two year'),labels = c(1,2,3))
tablechurn$PaymentMethod<-factor(tablechurn$PaymentMethod,levels = c('Bank transfer (automatic)',
                                                                     'Credit card (automatic)',
                                                                     'Electronic check',
                                                                     'Mailed check'),labels = c(1,2,3,4))

#Checking the dimension of the dataset again after performing above changes

dim(tablechurn)
str(tablechurn)

##Converting factors into numeric columns
tablechurn$Gender<-as.numeric(tablechurn$Gender)
tablechurn$SeniorCitizen<-as.numeric(tablechurn$SeniorCitizen)
tablechurn$Tenure<-as.numeric(tablechurn$Tenure)
tablechurn$MultipleLines<-as.numeric(tablechurn$MultipleLines)
tablechurn$InternetService<-as.numeric(tablechurn$InternetService)
tablechurn$Contract<-as.numeric(tablechurn$Contract)
tablechurn$PaymentMethod<-as.numeric(tablechurn$PaymentMethod)

tablechurn$Churn<-as.integer(tablechurn$Churn)

str(tablechurn)

dim(tablechurn)


#MULTIPLE Regression

##Performing regression taking all independent variables

fit <- lm(Churn~Gender+SeniorCitizen+Partner+Dependents+Tenure+PhoneService
          +MultipleLines+InternetService+OnlineBackup+OnlineSecurity
          +DeviceProtection+TechSupport+StreamingTV+StreamingMovies
          +Contract+PaperlessBilling+PaymentMethod+MonthlyCharges
          +TotalCharges, data=tablechurn)

summary(fit)
coefficients(fit)
confint(fit,level=0.95)

# Predicted Values
fitted(fit)
residuals(fit)

#Anova Table
anova(fit)
vcov(fit)
cov2cor(vcov(fit))
temp <- influence.measures(fit)
temp

##Diagnostics Plots for visualization
plot(fit)

#Finding Outliers and plottting scatterplot for visual analysis
outlierTest(fit)
qqPlot(fit, main="QQ Plot")

#Plotting leverage plots
leveragePlots(fit) 

# Plotting variable plots for Influential Observations

avPlots(fit)

# Normality of Residuals and qqplot for studentized resid
qqPlot(fit, main="QQ Plot")

#Distribution of studentized residuals
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

#Non-constant Error Variance
#Evaluating homoscedasticity
#Non-constant error variance test
ncvTest(fit)

#Plotting studentized residuals v/s. fitted values
spreadLevelPlot(fit)

#Multi-collinearity
# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2

#Nonlinearity
# component + residual plot
crPlots(fit)


#Global test of model assumptions

##In the below steps we are performing regressions using all independent variable and we keep
##on eliminating the ones which are not significant in the next regression until we see
##constant values for R2

gvmodel <- gvlma(fit)
summary(gvmodel)
fit
summary(fit)
fit1<-fit
fit2<-  lm(Churn~Gender+SeniorCitizen+Partner+Dependents+Tenure+PhoneService
           +MultipleLines+InternetService+OnlineBackup+OnlineSecurity
           +DeviceProtection+TechSupport+StreamingTV+StreamingMovies
           +Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=tablechurn)
summary(fit2)



fit3<-  lm(Churn~Gender+SeniorCitizen+Partner+Dependents+Tenure+PhoneService
           +MultipleLines+InternetService+OnlineBackup+OnlineSecurity
           +DeviceProtection+TechSupport+StreamingTV+
             Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=tablechurn)
summary(fit3)

fit4<-  lm(Churn~Gender+SeniorCitizen+Partner+Dependents+Tenure+PhoneService
           +MultipleLines+InternetService+OnlineBackup+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=tablechurn)
summary(fit4)


fit5<-  lm(Churn~Gender+SeniorCitizen+Partner+Dependents+Tenure+PhoneService
           +MultipleLines+InternetService+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=tablechurn)
summary(fit5)

fit6<-  lm(Churn~Gender+SeniorCitizen+Partner+Dependents+Tenure+PhoneService
           +MultipleLines+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=tablechurn)
summary(fit6)

fit7<-  lm(Churn~SeniorCitizen+Dependents+Tenure+PhoneService
           +MultipleLines+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=tablechurn)
summary(fit7)


fit7<-  lm(Churn~SeniorCitizen+Dependents+Tenure+PhoneService+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=tablechurn)
summary(fit7)

fit8<-  lm(Churn~SeniorCitizen+Tenure+PhoneService+OnlineSecurity
           +DeviceProtection+TechSupport+Contract+PaperlessBilling+MonthlyCharges
           +TotalCharges, data=tablechurn)
summary(fit8)

fit9<-  lm(Churn~Tenure+MonthlyCharges+TotalCharges, data=tablechurn)
summary(fit9)

##By running the above regression we notice that independent variables like SeniorCitizen,Tenure,PhoneService,OnlineSecurity
##DeviceProtection,TechSupportContract,PaperlessBilling,MonthlyCharges and TotalCharges could be factors
##that cause person to churn 

#Comparing model
anova(fit1,fit8)

step <- stepAIC(fit, direction="both")
step$anova

predict.lm(fit8, data.frame(SeniorCitizen = 0,Tenure =20,PhoneService = 1,OnlineSecurity = 2,
                            DeviceProtection = 1,TechSupport = 2,Contract = 3,PaperlessBilling = 1,
                            MonthlyCharges = 100,TotalCharges=1000) )

##By running the above prediction model using values like the user not being senior citizen,Tenure
##being 20 months, leveraging Phoneservice,DeviceProtection and PaperlessBilling, not using OnlineSecurity
##and TechSupport, having contract of 2 years, MonthlyCharges and TotalCharges being 100 &1000 respectively
##there is a likelihood of this customer churning 