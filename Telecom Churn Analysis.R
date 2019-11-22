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
library(fpc)
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
library(cowplot)
library(regclass)
library(stats)
library(ROCR)

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

#Replacing 'No phone Service'value in MultipleLines column with 'No'

tablechurn$MultipleLines[tablechurn$MultipleLines=='No phone service'] <- 'No'

#Deleting the unused levels from the factor variables

tablechurn$OnlineSecurity <- factor(tablechurn$OnlineSecurity)
tablechurn$OnlineBackup <- factor(tablechurn$OnlineBackup)
tablechurn$DeviceProtection <- factor(tablechurn$DeviceProtection)
tablechurn$TechSupport <- factor(tablechurn$TechSupport)
tablechurn$StreamingTV <- factor(tablechurn$StreamingTV)
tablechurn$StreamingMovies <- factor(tablechurn$StreamingMovies)
tablechurn$MultipleLines <- factor(tablechurn$MultipleLines)


##-----------------------------------------------------------------------------------
## Exploratory Data Analysis
##-----------------------------------------------------------------------------------

##Plotting graphs of each independent variables against dependent variable to check if these independent
##variables influence dependent variable

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

grid.arrange(b1,b5,b6,b8,b9,b10,b11,b12,b13,ncol= 3)
grid.arrange(b2,b3,b4,b7,b14,b15,b16,ncol=2)

c1<-boxplot(Tenure~Churn,data=tablechurn,col=c("skyblue","green"),xlab= "Churn",ylab="tenure")
c2<-boxplot(MonthlyCharges~Churn,data=tablechurn,col=c("skyblue","green"),xlab= "Churn",ylab="MonthlyCharges")
c3<-boxplot(TotalCharges~Churn,data=tablechurn,col=c("skyblue","green"),xlab= "Churn",ylab="TotalCharges")


plot(tablechurn$TotalCharges,tablechurn$Tenure_Range)
plot(tablechurn$TotalCharges,tablechurn$Tenure)


## Correlation Matrix
corr_data<-data.frame(tablechurn$Tenure_Range,tablechurn$MonthlyCharges,tablechurn$TotalCharges)
corr<-cor(corr_data)
corrplot(corr,method = "number")

hist(tablechurn$Tenure,main ="Tenure Distribution",xlab ="Tenure(Years)")

##------------------------PCA---------------------------------------------------------##

library(data.table)

## Creating new data frame for numerical values for doing PCA 

tablechurnPCA = data.frame(tablechurn$MonthlyCharges,tablechurn$TotalCharges,tablechurn$Tenure)
setDT(tablechurnPCA)

##Setting column names for our new dataframe
names(tablechurnPCA) <- c('MonthlyCharges','TotalCharges','Tenure')

##head(tablechurnPCA)
tablechurnPCA

####Using prcomp function to compute the principal components (eigenvalues and eigenvectors).
#With scale=TRUE, variable means are set to zero, and variances set to one

ChurnPC <- prcomp(tablechurnPCA[,1:3],scale=TRUE)
ChurnPC
summary(ChurnPC)

##Steps to check variances between the principal components
##Sample scores are stored in ChurnPC$x
# Square roots of eigenvalues are stored in ChurnPC$sdev
# Eigenvectors are stored in ChurnPC$rotation
# variable means stored in ChurnPC$center
# variable standard deviations stored in ChurnPC$scale
# Eigenvalues are sdev^2


##Calculating Eigen values

(eigen_churn <- ChurnPC$sdev^2)
names(eigen_churn) <- paste("PC",1:3,sep="")
names(eigen_churn)
eigen_churn

##Taking sum of all eigen values

sum_churn <- sum(eigen_churn)
sum_churn

##Calculating percentage of variance

pvarchurn <- (eigen_churn/sum_churn)*100
pvarchurn

##The results show that PC1 and PC2 columns have good amount of data that is almost 98% variance
#is presented with these 2 columns and since there is only 2% information in PC3 component we can afford to lose it 

##Plotting Scree Diagram for Principal Components wrt percentage of their variances


plot(pvarchurn, xlab = "Component number", ylab = "Component variance", type = "b", main = "Scree diagram")

##Checking cumulative variances and other values 

cumvar_churn <- cumsum(pvarchurn)
cumvar_churn

matchurn <- rbind(eigen_churn,pvarchurn,cumvar_churn)
rownames(matchurn) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matchurn,4)
summary(ChurnPC)
ChurnPC$rotation
print(ChurnPC)


ChurnPC$x

sparrtyp_pca <- cbind(data.frame(tablechurn$Churn),ChurnPC$x)
sparrtyp_pca

# Means of scores for all the PC's classified by Churn value

tabmeansPC <- aggregate(sparrtyp_pca[,2:4],by=list(Churn=tablechurn$Churn),mean)
tabmeansPC
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$Churn)),]
tabmeansPC
tabfmeans <- t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans

# Standard deviations of scores for all the PC's classified by Churn value

tabsdsPC <- aggregate(sparrtyp_pca[,2:4],by=list(Churn=tablechurn$Churn),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds

#T-Test

t.test(PC1~tablechurn$Churn,data=sparrtyp_pca)
t.test(PC2~tablechurn$Churn,data=sparrtyp_pca)
t.test(PC3~tablechurn$Churn,data=sparrtyp_pca)

# F ratio test

var.test(PC1~tablechurn$Churn,data=sparrtyp_pca)
var.test(PC2~tablechurn$Churn,data=sparrtyp_pca)
var.test(PC3~tablechurn$Churn,data=sparrtyp_pca)

# Levene's tests (one-sided)

library(car)
(LTPC1 <- leveneTest(PC1~tablechurn$Churn,data=sparrtyp_pca))
(p_PC1_1sided <- LTPC1[[3]][1]/2)
(LTPC2 <- leveneTest(PC2~tablechurn$Churn,data=sparrtyp_pca))
(p_PC2_1sided=LTPC2[[3]][1]/2)
(LTPC3 <- leveneTest(PC2~tablechurn$Churn,data=sparrtyp_pca))
(p_PC3_1sided <- LTPC3[[3]][1]/2)

# Plotting the scores for the first and second PC components
plot(sparrtyp_pca$PC1, sparrtyp_pca$PC2,pch=ifelse(sparrtyp_pca$Churn == "Yes",1,16),xlab="PC1", ylab="PC2", main="7043 customer values for PC1 & PC2")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Yes","No"), pch=c(1,16))
plot(eigen_churn, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_churn), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
print(summary(ChurnPC))
View(ChurnPC)
diag(cov(ChurnPC$x))
xlim <- range(ChurnPC$x[,1])
ChurnPC$x[,1]
ChurnPC$x
plot(ChurnPC$x,xlim=xlim,ylim=xlim)
ChurnPC$rotation[,1]
ChurnPC$rotation
ChurnPC$x
plot(ChurnPC)
#get the original value of the data based on PCA
center <- ChurnPC$center
scale <- ChurnPC$scale
new_ChurnPC <- as.matrix(tablechurnPCA[,-3])
new_ChurnPC
predict(ChurnPC)[,1]

##------------------------------Factor Analysis-----------------------------------##

library(data.table)

## Creating new data frame for numerical values for doing Factor Analysis

tablechurnFA = data.frame(tablechurn$MonthlyCharges,tablechurn$TotalCharges,tablechurn$Tenure)
setDT(tablechurnFA)

##Setting column names for our new dataframe
names(tablechurnFA) <- c('MonthlyCharges','TotalCharges','Tenure')

##head(tablechurnFA)
tablechurnFA

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
##i.e. RC1 and RC2 since most of the variance is explained by these factors itself and all the 3 variables in scope of this analysis are exp


##------------------------------Cluster Analysis-----------------------------------##

library(cluster)
tablechurnclust = data.frame(
  tablechurn$Tenure,
  tablechurn$TotalCharges,
  tablechurn$MonthlyCharges)

##Making cluster on the basis of Total charges
rownames(tablechurnclust) <- tablechurn$TotalCharges

##Scaling done to make the data on scale
scaleTotalCharges <- scale(tablechurnclust[,1:ncol(tablechurnclust)])
scaleTotalCharges

#Here we have selected first row to see how our scaled matrix is like
head(scaleTotalCharges,1)

# We will find K-means by taking k=2, 3, 4, 5, 6...

#For k-means = 2
(kmeans2.scaleTotalCharges <- kmeans(scaleTotalCharges,2,nstart = 10))
# Computing the percentage of variation accounted for two clusters
perc_var_kmeans2 <- round(100*(1 - kmeans2.scaleTotalCharges$betweenss/kmeans2.scaleTotalCharges$totss),1)
names(perc_var_kmeans2) <- "Perc. 2 clus"
perc_var_kmeans2

#For  k-means = 3
(kmeans3.scaleTotalCharges <- kmeans(scaleTotalCharges,3,nstart = 10))
# Computing the percentage of variation accounted for. Two clusters
perc_var_kmeans3 <- round(100*(1 - kmeans3.scaleTotalCharges$betweenss/kmeans3.scaleTotalCharges$totss),1)
names(perc_var_kmeans3) <- "Perc. 3 clus"
perc_var_kmeans3

#For  k-means = 4
(kmeans4.scaleTotalCharges <- kmeans(scaleTotalCharges,4,nstart = 10))
# Computing the percentage of variation accounted for. Two clusters
perc_var_kmeans4 <- round(100*(1 - kmeans4.scaleTotalCharges$betweenss/kmeans4.scaleTotalCharges$totss),1)
names(perc_var_kmeans4) <- "Perc. 4 clus"
perc_var_kmeans4

#Using k means 4 could be good to preseent our data
# Saving above 4 k-means (1,2,3)  in a list


##Now we will plot these clusters
library(fpc)
plotcluster(tablechurnclust,kmeans3.scaleTotalCharges$cluster)

##We didnt find any significant result using cluster analysis hence we will not be using it
#for our dataset

##---------------------Multi Linear Regression----------------------------------------##

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

##----------------Linear Discriminant Analysis (LDA)-----------------------##

##Using same independent variables that we found from logistic regression and performing LDA to see how well we would be able to predict using this model

tablechurn.data <-(tablechurn[,c("SeniorCitizen","Partner","Dependents","Tenure_Range",
                                 "PhoneService","InternetService","OnlineBackup","OnlineSecurity",
                                 "DeviceProtection","TechSupport","Contract",
                                 "PaperlessBilling","PaymentMethod","Churn")])


##Splitting data into 75% training and 25% test so that we have some data we can test our model on

smp_size_churn <- floor(0.75 * nrow(tablechurn.data))
train_ind_churn <- sample(nrow(tablechurn.data), size = smp_size_churn)
train_churn.df <- as.data.frame(tablechurn.data[train_ind_churn, ])
test_churn.df <- as.data.frame(tablechurn.data[-train_ind_churn, ])

##Performing LDA on our training data

tablechurn.lda <- lda(Churn~SeniorCitizen+Partner+Dependents+Tenure_Range+
                        PhoneService+InternetService+OnlineBackup+OnlineSecurity+
                        DeviceProtection+TechSupport+Contract+
                        PaperlessBilling+PaymentMethod, data=train_churn.df)




plot(tablechurn.lda)

##Making predictions on our testing data 

tablechurn.lda.predict <- predict(tablechurn.lda, newdata = test_churn.df)

### CONSTRUCTING ROC AUC PLOT:

# Get the posteriors as a dataframe.
tablechurn.lda.predict.posteriors <- as.data.frame(tablechurn.lda.predict$posterior)
head(tablechurn.lda.predict.posteriors)

# Evaluating the model

pred <- prediction(tablechurn.lda.predict.posteriors[,2], test_churn.df$Churn)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plotting the graph for better visualization

plot(roc.perf)
abline(a=0, b= 1)
text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

##From the above results we see that we get AUC value as 83.5% using LDA which implies this model is good
##fit and the predictors used in this model can influence our dependent variable Churn.



