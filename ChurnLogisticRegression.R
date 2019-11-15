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
library(psych)
library(cowplot)
library(regclass)
library(stats)
library(e1071)
library(pROC)



##-----------------------------------------------------------------------------------
##Importing Dataset and doing preliminary analysis
##-----------------------------------------------------------------------------------

#Importing CSV file from drive on my local computer and viewing it 

tablechurn<-read.csv("C:/Users/SHIVANSHI/Desktop/Priyanshi/MVA/Telecom Churn Analysis Data.csv")
tablechurn <- as.data.frame(tablechurn)


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
tablechurn$StreamingMovies <- factor(tablechurn$StreamingMovies)

##----------------LOGISTIC REGRESSION--------------------------------------------##

##Checking relationships between our dependent variable and each of our independent categorical variable.

xtabs(~Churn+Gender,data=tablechurn)
xtabs(~Churn+SeniorCitizen,data=tablechurn)
xtabs(~Churn+Partner,data=tablechurn)
xtabs(~Churn+Dependents,data=tablechurn)
xtabs(~Churn+Tenure_Range,data=tablechurn)
xtabs(~Churn+PhoneService,data=tablechurn)
xtabs(~Churn+MultipleLines,data=tablechurn)
xtabs(~Churn+InternetService,data=tablechurn)
xtabs(~Churn+OnlineBackup,data=tablechurn)
xtabs(~Churn+OnlineSecurity,data=tablechurn)
xtabs(~Churn+DeviceProtection,data=tablechurn)
xtabs(~Churn+TechSupport,data=tablechurn)
xtabs(~Churn+StreamingTV,data=tablechurn)
xtabs(~Churn+StreamingMovies,data=tablechurn)
xtabs(~Churn+Contract,data=tablechurn)
xtabs(~Churn+PaperlessBilling,data=tablechurn)
xtabs(~Churn+PaymentMethod,data=tablechurn)

##By above results, we find that independent variables like Senior Citizen ,Partner, Dependents,Tenure Range,Phone Service,Internet Service,OnlineBackup,OnlineSecurity,DeviceProtection,TechSupport,StreamingTV,StreamingMovies,Contract,PaperLess Billing,Payment Method variables can
##have impact on dependent variable (Churn).
##Although we see that the variables like StreamingTV and StreamingMovies don't show significant difference in
##indicating if person will churn or not based on the result. 
##So lets run 2 model.One simple model excluding StreamingTV and StreamingMovies and other
##including all independent variables mentioned above.


logistic_simple <- glm(Churn~SeniorCitizen+Partner+Dependents+Tenure_Range+
                         PhoneService+InternetService+OnlineBackup+OnlineSecurity+
                         DeviceProtection+TechSupport+Contract+
                         PaperlessBilling+PaymentMethod, data=tablechurn, family="binomial")
summary(logistic_simple)

## Calculating the p-value for R^2 for this model

ll.null <- logistic_simple$null.deviance/-2
ll.proposed <- logistic_simple$deviance/-2
(ll.null - ll.proposed) / ll.null
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic_simple$coefficients)-1))

##Performing regression using all variables including StreamingTV and StreamingMovies

logistic <- glm(Churn~SeniorCitizen+Partner+Dependents+Tenure_Range+
                         PhoneService+InternetService+OnlineBackup+OnlineSecurity+
                         DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+
                         PaperlessBilling+PaymentMethod,data=tablechurn, family="binomial")
summary(logistic)

##Calculating p value for R^2 for this model

ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null - ll.proposed) / ll.null
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))

##Plotting the graphs to visually view this regression

predicted.data <- data.frame(probability.of.Churn=logistic$fitted.values,Churn=tablechurn$Churn)
predicted.data <- predicted.data[order(predicted.data$probability.of.Churn, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

##Plotting the predicted probabilities for each samples probability of Churning and using colors to visually analyze if they Churned or not 

ggplot(data=predicted.data,aes(x=rank, y=probability.of.Churn)) +
  geom_point(aes(color=Churn), alpha=1, shape=4, stroke=2) +
  xlab("Index") +ylab("Predicted probability of Churning")


##Viewing the confusion matrix for this model

confusion_matrix(logistic)

##ROC graph is a plot of the true positive rate against the false positive rate.Hence plotting it for visualization

roc(tablechurn$Churn,logistic$fitted.values,plot=TRUE)
par(pty='s')
roc(tablechurn$Churn,logistic$fitted.values,plot=TRUE)

##Using 1-specificity (i.e. the False Positive Rate) on the x-axis by setting "legacy.axes" to TRUE for better visual analysis

roc(tablechurn$Churn,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE)
roc(tablechurn$Churn,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)

## If we want to find out the optimal threshold we can store the data used to make the ROC graph in a variable

roc.info <- roc(tablechurn$Churn, logistic$fitted.values, legacy.axes=TRUE)
str(roc.info)
roc.df <- data.frame(tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
                     fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
                     thresholds=roc.info$thresholds)

##This will show us the values for the upper right-hand corner of the ROC graph, when the threshold is so low

head(roc.df)  

##This will show us the values for the lower left-hand corner of the ROC graph, when the threshold is so high (infinity)

tail(roc.df) 

##Viewing graphs using percentage values
roc(tablechurn$Churn,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, percent=TRUE, print.auc=TRUE)
roc(tablechurn$Churn,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, percent=TRUE, print.auc=TRUE, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822", print.auc.x=45)

# Lets do two ROC plots to understand which model is better
roc(tablechurn$Churn, logistic_simple$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

# Lets add the other graph
plot.roc(tablechurn$Churn, logistic$fitted.values, percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Simple", "Non Simple"), col=c("#377eb8", "#4daf4a"), lwd=4) # Make it user friendly

# reset the par area back to the default setting
par(pty='m')


##From the above results we see that we get AUC value as 84.5% with the second model(i.e. non simple model) which implies this model is good
##fit and the predictors used in this model can influence our dependent variable Churn.

