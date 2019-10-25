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
library(psych)

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



##----------------------------Factor Analysis-------------------------------------


library(data.table)

## Creating new data frame for numerical values for doing Factor Analysis

tablechurnFA = data.frame(tablechurn$MonthlyCharges,tablechurn$TotalCharges,tablechurn$Tenure)
setDT(tablechurnFA)

##Setting column names for our new dataframe
names(tablechurnFA) <- c('MonthlyCharges','TotalCharges','Tenure')

##head(tablechurnFA)
tablechurnFA

library(psych)

##Since we have 3 columns that we are considering for factor analysis , we are checking
#how will the variance be distributed across 3 factors and if we really need 3 factors
#for our analysis.


fit.pc <- principal(tablechurnFA, nfactors=3, rotate="varimax")
fit.pc
round(fit.pc$values, 3)
fit.pc$loadings
# Loadings with more digits
for (i in c(1,2,3)) { print(fit.pc$loadings[[1,i]])}
# Communalities
fit.pc$communality
#Rotated factor scores, Notice the columns ordering 
fit.pc$scores
# Play with FA utilities

fa.parallel(tablechurnFA) # See factor recommendation
fa.plot(fit.pc) # See Correlations within Factors
fa.diagram(fit.pc) # Visualize the relationship
vss(tablechurnFA) # See Factor recommendations for a simple structure

##Looking at the output obtained from this step we infer that we just need 2 factors in our case.
##i.e. RC1 and RC2 since most of the variance is explained by these factors itself and all the 3 variables in scope of this analysis are explained by RC1 and RC2 factors.

