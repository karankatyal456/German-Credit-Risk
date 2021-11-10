#####################################################################
####################### Classificaion Case Study ###################
#####################################################################

#In case of a classification problem:  the target variable is a categorical in  nature.
#We'll be doing Logistic Regression and Classification decision tree. 


# Step-1
#Loading the raw Data
GermanData=read.csv('F:/IVY/ISA JUNE 2021/R/German dataset/German_Credit_data.csv',na.strings=c(""," ","NA","NULL"))
View(GermanData)
dim(GermanData)
class(GermanData)
names(GermanData)
summary(GermanData)

##Status_of_existing_account  
length(unique(GermanData$Status_of_existing_account))
#4 unique values and also it can help to predict.

##Duration_of_Credit_month
## It will also help , to see whether the duration of month is very much shorter or longer.

##Payment_Status_of_Previous_Credit.Credit_history.
length(unique(GermanData$Payment_Status_of_Previous_Credit.Credit_history.))
## 5 , what is their status of payment can help to predict.

##Purpose_of_loan
length(unique(GermanData$Purpose_of_loan))
## As here too many unique labels and also it can't help



##"Credit_Amount","Value_of_Savings_account.bonds","Years_of_Present_Employment","Percentage_of_disposable_income","Sex_._Marital_Status",                            
##"Guarantors.Debtors","Duration_in_Present_Residence","Property","Age_in_years","Concurrent_Credits","Housing",
##"No_of_Credits_at_this__Bank","Occupation","No_of_dependents","Telephone" ,"Foreign_Worker"
## As we don't have proper definition of all the variable so lets explore on the basis of statistical test

# Removing useless columns
GermanData[, c("Purpose_of_loan")] = NULL

head(GermanData)
str(GermanData)


###check if all the categorical variables are factor or not

factor_cols=c("Creditability","Status_of_existing_account","Payment_Status_of_Previous_Credit.Credit_history.",
              "Value_of_Savings_account.bonds","Years_of_Present_Employment","Sex_._Marital_Status",                            
              "Guarantors.Debtors","Property","Concurrent_Credits","Housing",
              "Occupation","Telephone" ,"Foreign_Worker")

for (cat_cols in factor_cols){
  GermanData[ , cat_cols]=as.factor(GermanData[ , cat_cols])
}

str(GermanData)
############################################################
# Step-6
# Checking and treating missing values

# Checking missing values
colSums(is.na(GermanData))


# Step-8
# Explore each "Potential" predictor for distribution and Quality
############################################################

# Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("Duration_of_Credit_month","Credit_Amount","Percentage_of_disposable_income","Duration_in_Present_Residence",
              "Age_in_years","No_of_Credits_at_this__Bank","No_of_dependents")

#Splitting the plot window into four parts
par(mfrow=c(2,4))

# library to generate professional colors
library(RColorBrewer) 

# looping to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(GermanData[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}


############################################################
# Exploring MULTIPLE CATEGORICAL features
ColsForBar=c("Creditability","Status_of_existing_account","Payment_Status_of_Previous_Credit.Credit_history.",
             "Value_of_Savings_account.bonds","Years_of_Present_Employment","Sex_._Marital_Status",                            
             "Guarantors.Debtors","Property","Concurrent_Credits","Housing",
             "Occupation","Telephone" ,"Foreign_Worker")

#Splitting the plot window into four parts
par(mfrow=c(3,6))

# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(GermanData[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Paired"))
}


############################################################# 
# Statistical Relationship between target variable (Categorical) and predictors

# Categorical Vs Continuous --- ANOVA
# Categorical Vs Categorical -- Chi-square test


# Continuous Vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)
# H0: Variables are NOT correlated
# Small P-Value <5%--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)

summary(aov(Duration_of_Credit_month ~ Creditability, data = GermanData))
summary(aov(Credit_Amount ~ Creditability, data = GermanData))
summary(aov(Percentage_of_disposable_income ~ Creditability, data = GermanData))
summary(aov(Duration_in_Present_Residence ~ Creditability, data = GermanData))
## We will reject duration in present residence

summary(aov(Age_in_years ~ Creditability, data = GermanData))
summary(aov(No_of_Credits_at_this__Bank ~ Creditability, data = GermanData))
## We will reject no of credits at bank

summary(aov(No_of_dependents ~ Creditability, data = GermanData))
## We will reject no of dependents 

## Good variable = Duration_of_Credit_month","Credit_Amount","Percentage_of_disposable_income","Age_in_years"

#### Categorical Vs Categorical relationship strength: Chi-Square test
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)


##It takes crosstabulation as the input and gives you the result

Chisqcols=c("Status_of_existing_account","Payment_Status_of_Previous_Credit.Credit_history.",
            "Value_of_Savings_account.bonds","Years_of_Present_Employment","Sex_._Marital_Status",                            
            "Guarantors.Debtors","Property","Concurrent_Credits","Housing",
            "Occupation","Telephone" ,"Foreign_Worker")

for(chi_cols in Chisqcols){
  CrossTabResult=table(GermanData[,c('Creditability',chi_cols)])
  ChiResult=chisq.test(CrossTabResult)
  print(chi_cols)
  print(ChiResult)
}

## Rejecting the below columns
##Sex_._Marital_Status,Guarantors.Debtors,Occupation,Telephone,Foreign_Worker


############################################################



############################################################
# Step-10
InputData=GermanData

# Specifying the Target Variable
TargetVariableName="Creditability"

# Specifying the Predictor Variable
BestPredictorName= c("Duration_of_Credit_month","Credit_Amount","Percentage_of_disposable_income",
                     "Age_in_years","Status_of_existing_account","Payment_Status_of_Previous_Credit.Credit_history.",
                     "Value_of_Savings_account.bonds","Years_of_Present_Employment",                            
                     "Property","Concurrent_Credits","Housing")

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=InputData[, c(TargetVariableName)]
str(TargetVariable)

# Here I am Selecting all other columns as Predictors apart from target variable
#but based on EDA, you'll have to choose those columns which are important

PredictorVariables=InputData[,BestPredictorName]
str(PredictorVariables)


DataForML=data.frame(TargetVariable,PredictorVariables)
##make sure you look at the structure before running any classification algo
str(DataForML)
head(DataForML)

# Step-12
#############################################################################################
# Sampling | Splitting data into 70% for training 30% for testing
set.seed=(123)
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)



#############################################################################################
#############################################################################################
# Creating Predictive models on training data to check the accuracy on test data
###### Logistic Regression #######

##we are predicting TV based on all other variables
##glm() is used for wide variety of modeling activities. Logistic regression
#is one of the models that you can create using glm()
##in order to tell glm() that you have to perform logistic regression,
#you have to say family= 'binomial"

startTime=Sys.time()
LR_Model=glm(TargetVariable ~ . , data=DataForMLTrain, family='binomial')

summary(LR_Model)
endTime=Sys.time()
endTime-startTime

##probabilities will guide you whether to accept or reject a particular column


LR_Model_2=glm(TargetVariable ~ .-Duration_of_Credit_month, data=DataForMLTrain, family='binomial')
summary(LR_Model_2)



LR_Model_3=glm(TargetVariable ~ .-Duration_of_Credit_month-Property, data=DataForMLTrain, family='binomial')
summary(LR_Model_3)

LR_Model_4=glm(TargetVariable ~ .-Duration_of_Credit_month-Property-Age_in_years, data=DataForMLTrain, family='binomial')
summary(LR_Model_4)

LR_Model_5=glm(TargetVariable ~ .-Duration_of_Credit_month-Property-Age_in_years-Housing, data=DataForMLTrain, family='binomial')
summary(LR_Model_5)

LR_Model_6=glm(TargetVariable ~ .-Duration_of_Credit_month-Property-Age_in_years-Housing-Years_of_Present_Employment, data=DataForMLTrain, family='binomial')
summary(LR_Model_6)


LR_Model_7=glm(TargetVariable ~ Credit_Amount + Percentage_of_disposable_income  + I(Status_of_existing_account==2) + I(Status_of_existing_account==3) + 
                 I(Status_of_existing_account==4) +I(Payment_Status_of_Previous_Credit.Credit_history.==2)+ 
                 I(Payment_Status_of_Previous_Credit.Credit_history.==3) +  I(Payment_Status_of_Previous_Credit.Credit_history.==4) + I(Value_of_Savings_account.bonds==2)+
                 I(Value_of_Savings_account.bonds==3) + I(Value_of_Savings_account.bonds==4) + I(Value_of_Savings_account.bonds==5) +
                   + I(Concurrent_Credits==2) + I(Concurrent_Credits==3) 
                 , data=DataForMLTrain, family='binomial')

summary(LR_Model_7)


LR_Model_8=glm(TargetVariable ~ Credit_Amount + Percentage_of_disposable_income  + I(Status_of_existing_account==2) + I(Status_of_existing_account==3) + 
                 I(Status_of_existing_account==4) +I(Payment_Status_of_Previous_Credit.Credit_history.==2)+ 
                 I(Payment_Status_of_Previous_Credit.Credit_history.==3) +  I(Payment_Status_of_Previous_Credit.Credit_history.==4) + 
                 I(Value_of_Savings_account.bonds==3) + I(Value_of_Savings_account.bonds==4) + I(Value_of_Savings_account.bonds==5) +
                 + I(Concurrent_Credits==2) + I(Concurrent_Credits==3) 
               , data=DataForMLTrain, family='binomial')

summary(LR_Model_8)

LR_Model_9=glm(TargetVariable ~ Credit_Amount + Percentage_of_disposable_income  + I(Status_of_existing_account==2) + I(Status_of_existing_account==3) + 
                 I(Status_of_existing_account==4) +I(Payment_Status_of_Previous_Credit.Credit_history.==2)+ 
                 I(Payment_Status_of_Previous_Credit.Credit_history.==3) +  I(Payment_Status_of_Previous_Credit.Credit_history.==4) + 
                  I(Value_of_Savings_account.bonds==4) + I(Value_of_Savings_account.bonds==5) +
                 + I(Concurrent_Credits==2) + I(Concurrent_Credits==3) 
               , data=DataForMLTrain, family='binomial')

summary(LR_Model_9)

LR_Model_10=glm(TargetVariable ~ Credit_Amount + Percentage_of_disposable_income  + I(Status_of_existing_account==2) + I(Status_of_existing_account==3) + 
                 I(Status_of_existing_account==4) +I(Payment_Status_of_Previous_Credit.Credit_history.==2)+ 
                 I(Payment_Status_of_Previous_Credit.Credit_history.==3) +  I(Payment_Status_of_Previous_Credit.Credit_history.==4) + 
                  I(Value_of_Savings_account.bonds==5) +
                 + I(Concurrent_Credits==2) + I(Concurrent_Credits==3) 
               , data=DataForMLTrain, family='binomial')

summary(LR_Model_10)


LR_Model_11=glm(TargetVariable ~ Credit_Amount + Percentage_of_disposable_income  + I(Status_of_existing_account==2) + I(Status_of_existing_account==3) + 
                  I(Status_of_existing_account==4) +I(Payment_Status_of_Previous_Credit.Credit_history.==2)+ 
                  I(Payment_Status_of_Previous_Credit.Credit_history.==3) +  I(Payment_Status_of_Previous_Credit.Credit_history.==4) + 
                  I(Value_of_Savings_account.bonds==5) +
                   I(Concurrent_Credits==3) 
                , data=DataForMLTrain, family='binomial')

summary(LR_Model_11)

LR_Model_12=glm(TargetVariable ~ Credit_Amount + Percentage_of_disposable_income  + I(Status_of_existing_account==2) + I(Status_of_existing_account==3) + 
                  I(Status_of_existing_account==4) +I(Payment_Status_of_Previous_Credit.Credit_history.==2)+ 
                  I(Payment_Status_of_Previous_Credit.Credit_history.==3) +  I(Payment_Status_of_Previous_Credit.Credit_history.==4) + 
                  I(Value_of_Savings_account.bonds==5) 
                   , data=DataForMLTrain, family='binomial')

summary(LR_Model_12)


#the regression coefficient for credit amount is -1.582. 
#This indicate that one unit increase in the credit  will decrease
#the creditability by a factor of exp(-1.848)

## Similarly give observations for other continuous column


#if you belong to status of existing account 2 compared to status of existing account 1, you are more  likely to give creditability
#if you belong to status of existing account 2 compared to tatus of existing account 2, then the Crediatibility increases
#by 2.802 %

## Similarly give observations for other categorical column 

# Checking Accuracy of model on Testing data
PredictionProb=predict(LR_Model_12,DataForMLTest,type = "response")
PredictionProb

##considering a threshold of 0.55

DataForMLTest$Prediction=ifelse(PredictionProb>0.55, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
head(DataForMLTest)

# Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
##install.packages('caret', dependencies = TRUE)
library(caret)

AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")

# Since AccuracyResults is a list of multiple items, fetching useful components only

AccuracyResults[['table']]
AccuracyResults[['byClass']]
AccuracyResults[['overall']][1]

print(paste('### Overall Accuracy of Logistic Reg Model is: ', round(100 * AccuracyResults[['overall']][1]) , '%'))

### Overall Accuracy of Logistic Reg Model is:  76 %
### You can increase the accuracy by changing the threshold value.
#############################################################################################



