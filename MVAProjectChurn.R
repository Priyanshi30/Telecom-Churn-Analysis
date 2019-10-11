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

##-----------------------------------------------------------------------------------
## Exploratory Data Analysis
##-----------------------------------------------------------------------------------

b1<-ggplot(tablechurn,aes(Gender,fill=Churn))+geom_bar(position = 'fill')
b1

b2<-ggplot(tablechurn,aes(SeniorCitizen,fill=Churn))+geom_bar(position = 'fill')
b2

b3<-ggplot(tablechurn,aes(Partner,fill=Churn))+geom_bar(position = 'fill')
b3

b4<-ggplot(tablechurn,aes(Dependents,fill=Churn))+geom_bar(position = 'fill')
b4

b5<-ggplot(tablechurn,aes(PhoneService,fill=Churn))+geom_bar(position = 'fill')
b5

b6<-ggplot(tablechurn,aes(MultipleLines,fill=Churn))+geom_bar(position = 'fill')
b6

b7<-ggplot(tablechurn,aes(InternetService,fill=Churn))+geom_bar(position = 'fill')
b7

b8<-ggplot(tablechurn,aes(OnlineSecurity,fill=Churn))+geom_bar(position = 'fill')
b8

b9<-ggplot(tablechurn,aes(OnlineBackup,fill=Churn))+geom_bar(position = 'fill')
b9

b10<-ggplot(tablechurn,aes(DeviceProtection,fill=Churn))+geom_bar(position = 'fill')
b10

b11<-ggplot(tablechurn,aes(TechSupport,fill=Churn))+geom_bar(position = 'fill')
b11

b12<-ggplot(tablechurn,aes(StreamingTV,fill=Churn))+geom_bar(position = 'fill')
b12

b13<-ggplot(tablechurn,aes(StreamingMovies,fill=Churn))+geom_bar(position = 'fill')
b13

b14<-ggplot(tablechurn,aes(Contract,fill=Churn))+geom_bar(position = 'fill')
b14

b15<-ggplot(tablechurn,aes(PaperlessBilling,fill=Churn))+geom_bar(position = 'fill')
b15

b16<-ggplot(tablechurn,aes(PaymentMethod,fill=Churn))+geom_bar(position = 'fill')
b16

grid.arrange(b1,b2,b3, ncol= 1)
grid.arrange(b4,b5,b6,ncol = 1)
grid.arrange(b7,b8,b9,b10, ncol = 1)
grid.arrange(b11,b12,b13,b14,b15, ncol = 1)


boxplot(tablechurn$TotalCharges,data=tablechurn,main="Total Charges")
boxplot(tablechurn$MonthlyCharges,data=tablechurn,main="Monthly Charges")
boxplot(tablechurn$Tenure,data=tablechurn,main="Tenure")

c1<-boxplot(Tenure~Churn,data=tablechurn,col=c("skyblue","green"),xlab= "churn",ylab="tenure")
c2<-boxplot(MonthlyCharges~Churn,data=tablechurn,col=c("skyblue","green"),xlab= "churn",ylab="MonthlyCharges")
c3<-boxplot(TotalCharges~Churn,data=tablechurn,col=c("skyblue","green"),xlab= "churn",ylab="TotalCharges")


plot(tablechurn$TotalCharges,tablechurn$Tenure_Range)
plot(tablechurn$TotalCharges,tablechurn$Tenure)


## Correlation Matrix
corr_data<-data.frame(tablechurn$Tenure,tablechurn$MonthlyCharges,tablechurn$TotalCharges)
corr<-cor(corr_data)
corrplot(corr,method = "number")

hist(tablechurn$Tenure,main ="Tenure Distribution",xlab ="Tenure(Years)")
