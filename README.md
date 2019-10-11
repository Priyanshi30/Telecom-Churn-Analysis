# TELECOM-CHURN-ANALYSIS

## Problem Statement:

Analysis is to predict the driving forces that lead to the user changing their network provider.

## Data Dictionary :

Variable | Description | Data Type
---------|-------------|----------
CustomerID | Customer's unique identification ID | Factor
Gender | Whether customer is Male or Female | Factor
SeniorCitizen | Whether the customer is a Senior Citizen or not (1,0) | int
Partner | Whether the customer has a partner or not (Yes, No) | Factor
Dependents | Whether the customer has dependents or not (Yes, No) | Factor
Tenure | Number of months the customer has stayed with the company | int
PhoneService | Whether the customer has a phone service or not (Yes, No) | Factor
MultipleLines | Whether the customer has multiple lines or not (Yes, No, No phone service) | Factor
InternetService | Customer’s internet service provider (DSL, Fiber optic, No) | Factor
OnlineSecurity | Whether the customer has online security or not (Yes, No, No internet service) | Factor
OnlineBackup | Whether the customer has online backup or not (Yes, No, No internet service) | Factor
DeviceProtection | Whether the customer has device protection or not (Yes, No, No internet service) | Factor
TechSupport | Whether the customer has tech support or not (Yes, No, No internet service) | Factor
StreamingTV | Whether the customer has streaming TV or not (Yes, No, No internet service) | Factor
StreamingMovies | Whether the customer has streaming movies or not (Yes, No, No internet service) | Factor
Contract | The contract term of the customer (Month-to-month, One year, Two year) | Factor
PaperlessBilling | Whether the customer has paperless billing or not (Yes, No) | Factor
PaymentMethod | The customer’s payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic)) | Factor
MonthlyCharges | The amount charged to the customer monthly | num
TotalCharges | The total amount charged to the customer | num
Churn | Whether the customer churned or not (Yes or No) | Factor

## Dataset Question:

What are the factors that lead to users changing their network providers?

Datasource:https://www.kaggle.com/blastchar/telco-customer-churn
