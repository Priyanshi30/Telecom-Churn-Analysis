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