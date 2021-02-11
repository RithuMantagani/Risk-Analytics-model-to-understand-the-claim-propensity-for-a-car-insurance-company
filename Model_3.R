library(readxl)
library(psych)
library(dplyr)
library(DataExplorer)
library(dlookr)
library(caret)
#Read the file
insurance_data = read_excel(file.choose())
attach(insurance_data)
str(insurance_data,list.len=127)
summary(insurance_data)

insurance_data <-insurance_data[which(insurance_data$Number_of_Driver=='3'),c(2:9,12:15,18:27,30:31,53:55,58:60,63:65,68:71,74:82,
                                                                                85:87,90:92,95:97,100:102,105:107,110:112,115:117,
                                                                                120:127)]

insurance_data$ClaimStatus <- as.factor(insurance_data$ClaimStatus)
insurance_data$ClaimFrequency <- as.factor(insurance_data$ClaimFrequency)
insurance_data$Billing_Term <- as.factor(insurance_data$Billing_Term)
insurance_data$Renewed <- as.factor(insurance_data$Renewed)
insurance_data$Number_of_Driver <- as.factor(insurance_data$Number_of_Driver)
insurance_data$Amendment <- as.factor(insurance_data$Amendment)
insurance_data$CoverageLiability <- as.factor(insurance_data$CoverageLiability)
insurance_data$CoverageMP <- as.factor(insurance_data$CoverageMP)
insurance_data$CoveragePD_1 <- as.factor(insurance_data$CoveragePD_1)
insurance_data$CoveragePIP_CDW <- as.factor(insurance_data$CoveragePIP_CDW)
insurance_data$CoverageUMBI <- as.factor(insurance_data$CoverageUMBI)
insurance_data$CoverageUMPD <- as.factor(insurance_data$CoverageUMPD)
insurance_data$DriverAssigned_1 <- as.factor(insurance_data$DriverAssigned_1)
insurance_data$GaragedZIP_1 <- as.factor(insurance_data$GaragedZIP_1)
insurance_data$MaritalStatus_1 <- as.factor(insurance_data$MaritalStatus_1)
insurance_data$MaritalStatus_2 <- as.factor(insurance_data$MaritalStatus_2)
insurance_data$MaritalStatus_3 <- as.factor(insurance_data$MaritalStatus_3)
insurance_data$Occupation_1 <- as.factor(insurance_data$Occupation_1)
insurance_data$Occupation_2 <- as.factor(insurance_data$Occupation_2)
insurance_data$Occupation_3 <- as.factor(insurance_data$Occupation_3)
insurance_data$Relation_1 <- as.factor(insurance_data$Relation_1)
insurance_data$Relation_2 <- as.factor(insurance_data$Relation_2)
insurance_data$Relation_3 <- as.factor(insurance_data$Relation_3)
insurance_data$Rental_1 <- as.factor(insurance_data$Rental_1)
insurance_data$Sex_1 <- as.factor(insurance_data$Sex_1)
insurance_data$Sex_2 <- as.factor(insurance_data$Sex_2)
insurance_data$Sex_3 <- as.factor(insurance_data$Sex_3)
insurance_data$Surcharge1Unit_1 <- as.factor(insurance_data$Surcharge1Unit_1)
insurance_data$Surcharge2Unit_1 <- as.factor(insurance_data$Surcharge2Unit_1)
insurance_data$Surcharge3Unit_1 <- as.factor(insurance_data$Surcharge3Unit_1)
insurance_data$Towing_1 <- as.factor(insurance_data$Towing_1)
insurance_data$Units <- as.factor(insurance_data$Units)
insurance_data$Make_1 <- as.factor(insurance_data$Make_1)
insurance_data$Model_1 <- as.factor(insurance_data$Model_1)
insurance_data$Zip <- as.factor(insurance_data$Zip)
insurance_data$NoLossSigned <- as.factor(insurance_data$NoLossSigned)
insurance_data$Type <- as.factor(insurance_data$Type)
insurance_data$CancellationType <- as.factor(insurance_data$CancellationType)

#adding up voilation points of each policy record.
insurance_data$total_voilation_points <- insurance_data$ViolPoints1Driver_1+insurance_data$ViolPoints1Driver_2+insurance_data$ViolPoints1Driver_3+insurance_data$ViolPoints2Driver_1+insurance_data$ViolPoints2Driver_2+insurance_data$ViolPoints2Driver_3+
  insurance_data$ViolPoints3Driver_1+insurance_data$ViolPoints3Driver_2+insurance_data$ViolPoints3Driver_3+
  insurance_data$ViolPoints4Driver_1+insurance_data$ViolPoints4Driver_2+insurance_data$ViolPoints4Driver_3+
  insurance_data$ViolPoints5Driver_1+insurance_data$ViolPoints5Driver_2+insurance_data$ViolPoints5Driver_3+
  insurance_data$ViolPoints6Driver_1+insurance_data$ViolPoints6Driver_2+insurance_data$ViolPoints6Driver_3+
  insurance_data$ViolPoints7Driver_1+insurance_data$ViolPoints7Driver_2+insurance_data$ViolPoints7Driver_3+
  insurance_data$ViolPoints8Driver_1+insurance_data$ViolPoints8Driver_2+insurance_data$ViolPoints8Driver_3

#Drop Columns which are no more required
insurance_data <-insurance_data[,c(1:8,10:19,23:43,68:76)]

#-----------------------Transformation-------------------------------#

#Claim Frequency is directly correlated to claim status, hence removing
table(insurance_data$ClaimFrequency,insurance_data$ClaimStatus)
insurance_data$ClaimFrequency <- NULL

#binning levels of billing term variable. if billing term is more than 1 then we bin them in '2' category.
table(insurance_data$Billing_Term,insurance_data$ClaimStatus)
insurance_data$Billing_Term=ifelse(insurance_data$Billing_Term==3 | insurance_data$Billing_Term==6 , 2, 1)
table(insurance_data$Billing_Term,insurance_data$ClaimStatus)

#binning Ammendments variable. If there are any ammendments, then we will bin them in '1' category
table(insurance_data$Amendment,insurance_data$ClaimStatus)
insurance_data$Amendment=ifelse(insurance_data$Amendment==0 , 0, 1)
table(insurance_data$Amendment,insurance_data$ClaimStatus)

table(insurance_data$Renewed,insurance_data$ClaimStatus)

#binning coverage liablity variable. If there are any coverage other than 20/40/15(mapped to '0'), then we will bin them in '1' category
table(insurance_data$CoverageLiability,insurance_data$ClaimStatus)
insurance_data$CoverageLiability=ifelse(insurance_data$CoverageLiability=='20/40/15' , 0, 1)
table(insurance_data$CoverageLiability,insurance_data$ClaimStatus)

table(insurance_data$CoveragePD_1,insurance_data$ClaimStatus)
insurance_data$CoveragePD_1=ifelse(insurance_data$CoveragePD_1=='500/500' , 0, 1) #not required
table(insurance_data$CoveragePD_1,insurance_data$ClaimStatus) 

table(insurance_data$CoverageMP,insurance_data$ClaimStatus)
insurance_data$CoverageMP <- NULL

#dropping CoveragePIP_CDW due to data imbalance being created.
table(insurance_data$CoveragePIP_CDW,insurance_data$ClaimStatus)
insurance_data$CoveragePIP_CDW=NULL

table(insurance_data$CoverageUMBI,insurance_data$ClaimStatus)
insurance_data$CoverageUMBI=NULL
insurance_data$CoverageUMPD=NULL
#is DriverAssigned making any difference to claim status if we take in 0's and 1's..or alone ? No
table(insurance_data$DriverAssigned_1,insurance_data$ClaimStatus)
levels(insurance_data$DriverAssigned_1)[levels(insurance_data$DriverAssigned_1)== 4] <- NA
table(insurance_data$DriverAssigned_1,insurance_data$ClaimStatus)
insurance_data$Engine_1 <- NULL

table(insurance_data$MaritalStatus_1,insurance_data$ClaimStatus)
table(insurance_data$MaritalStatus_2,insurance_data$ClaimStatus)
table(insurance_data$MaritalStatus_3,insurance_data$ClaimStatus)
#insurance_data$MaritalStatus_1 <- NULL
#insurance_data$MaritalStatus_2 <- NULL
insurance_data$MaritalStatus_3 <- NULL #Missing Values

table(insurance_data$Occupation_3,insurance_data$ClaimStatus)
insurance_data$Occupation_1 <- NULL
insurance_data$Occupation_2 <- NULL
insurance_data$Occupation_3 <- NULL

table(insurance_data$Relation_2,insurance_data$ClaimStatus)
table(insurance_data$Relation_3,insurance_data$ClaimStatus)
insurance_data$Relation_1 <- NULL
#insurance_data$Relation_2 <- NULL
insurance_data$Relation_3 <- NULL #missing Values
summary(insurance_data$Relation_2)

level_key <- c("aunt"="FAMILY","AUNT"="FAMILY","brother"="FAMILY","BROTHER"="FAMILY",
               "brotherin"="FAMILY","brotherlaw"="FAMILY","CHILD"="FAMILY","cousin"="FAMILY",
               "COUSIN"="FAMILY","cousing"="FAMILY","CU±ADO"="FAMILY","dad"="FAMILY","DAD"="FAMILY",
               "dau"="FAMILY","daughter"="FAMILY","Daughter"="FAMILY","DAUGHTER"="FAMILY",
               "DAUGHTERN"="FAMILY","daugther"="FAMILY","DAUGTHER"="FAMILY","DiL"="FAMILY","Dinlaw"="FAMILY",
               "father"="FAMILY","Father"="FAMILY","FATHER"="FAMILY","friend"="FAMILY","Friend"="FAMILY",
               "friends"="FAMILY","GIRLFRIEND"="FAMILY","GRANDCHILD"="FAMILY","Grandson"="FAMILY",
               "husb"="SPOUSE","HUSB"="SPOUSE","husband"="SPOUSE","Husband"="SPOUSE","HUSBAND"="SPOUSE",
               "inlaw"="FAMILY","INLAW"="FAMILY","mom"="FAMILY","Mom"="FAMILY","MOM"="FAMILY","mother"="FAMILY",
               "Mother"="FAMILY","MOTHER"="FAMILY","NIECE"="FAMILY","non-rela"="FAMILY","NONRELATI"="FAMILY",
               "Other"="FAMILY","OTHER"="FAMILY","OTHERREL"="FAMILY","PAR"="FAMILY","parent"="FAMILY",
               "Parent"="FAMILY","PARENT"="FAMILY","relative"="FAMILY","RELATIVE"="FAMILY","SIN"="FAMILY",
               "SIS"="FAMILY","SISINLAW"="FAMILY","sister"="FAMILY","SISTER"="FAMILY","SISTERS"="FAMILY",
               "son"="FAMILY","Son"="FAMILY","SON"="FAMILY","Son/law"="FAMILY","soninlaw"="FAMILY","SONINLAW"="FAMILY",
               "soninlow"="FAMILY","SPO"="SPOUSE","SPOOUSE"="SPOUSE","SPOUE"="SPOUSE","spous"="SPOUSE",
               "spouse"="SPOUSE","Spouse"="SPOUSE","SPOuse"="SPOUSE","SPOUST"="SPOUSE","FRIEND"="FAMILY",
               "sps"="SPOUSE","spuose"="SPOUSE","wife"="SPOUSE","Wife"="SPOUSE","WIFE"="SPOUSE",
               "STEPDAD"="FAMILY","UNCEL"="FAMILY","uncle"="FAMILY")
insurance_data$Relation_2=recode(insurance_data$Relation_2, !!!level_key)
levels(insurance_data$Relation_2)
summary(insurance_data$Relation_2)

table(insurance_data$Rental_1,insurance_data$ClaimStatus)
insurance_data$Rental_1=ifelse(insurance_data$Rental_1==0 , 0, 1)
#insurance_data$Rental_1 <- NULL


#dropping sex_1 and Sex_2 attribute due to similar claim rates.
table(insurance_data$Sex_1,insurance_data$ClaimStatus)
table(insurance_data$Sex_2,insurance_data$ClaimStatus)
table(insurance_data$Sex_3,insurance_data$ClaimStatus)
#insurance_data$Sex_1=NULL
#insurance_data$Sex_2=NULL
insurance_data$Sex_3<-NULL #Missing Values

table(insurance_data$Surcharge1Unit_1,insurance_data$ClaimStatus)
table(insurance_data$Surcharge2Unit_1,insurance_data$ClaimStatus)
table(insurance_data$Surcharge3Unit_1,insurance_data$ClaimStatus)
insurance_data$Surcharge1Unit_1=NULL
insurance_data$Surcharge2Unit_1=NULL
#dropping towing_1 varibale since claim rates across levels is almost same and there is data imbalance in the levels
table(insurance_data$Towing_1,insurance_data$ClaimStatus)
insurance_data$Towing_1=ifelse(insurance_data$Towing_1=='0' , 0, 1)
#insurance_data$Towing_1<-NULL

#binning  units variable to one or more than one levels mapped to 1 and 2 respectively.
table(insurance_data$Units,insurance_data$ClaimStatus)
insurance_data$Units=as.integer(insurance_data$Units)
insurance_data$Units=ifelse(insurance_data$Units>1,2,1) 
table(insurance_data$Units,insurance_data$ClaimStatus)

#binning year_1 variable to 2000 and below or more than 2000 year mapped to 0 and 1 respectively.

#This is i'm not sure about
table(insurance_data$Year_1,insurance_data$ClaimStatus)
insurance_data$Year_1=as.numeric(as.character(insurance_data$Year_1))
insurance_data$Year_1=ifelse(insurance_data$Year_1<=2000,0,1)
table(insurance_data$Year_1,insurance_data$ClaimStatus)

#insurance_data$Make_1<-NULL
insurance_data$Model_1<-NULL
insurance_data$Zip<-NULL

table(insurance_data$VehicleInspected_1,insurance_data$ClaimStatus)

table(insurance_data$Total_Distance_To_Work,insurance_data$ClaimStatus)
#Can do something with Total distance work

table(insurance_data$NoLossSigned,insurance_data$ClaimStatus)
#insurance_data$NoLossSigned<-NULL
#dropping type variable, due to definition insufficiency and thus inability to bin and explanablity/relaiblity issues
table(insurance_data$Type,insurance_data$ClaimStatus)
#insurance_data$Type=NULL
#insurance_data$Type <- insurance_data$Type
#levels(insurance_data$Type)[levels(insurance_data$Type)== 'VD'] <- NA
insurance_data$CancellationType <- NULL

table(insurance_data$total_voilation_points,insurance_data$ClaimStatus)
insurance_data$total_voilation_points=(ifelse(insurance_data$total_voilation_points>0,1,0))
table(insurance_data$total_voilation_points,insurance_data$ClaimStatus)


# Transformation for age parameter

#Method1
summary(insurance_data$AgeUSdriving_2)
insurance_data$AgeUSdriving_2=(ifelse(insurance_data$AgeUSdriving_2==0,37,insurance_data$AgeUSdriving_2))

summary(insurance_data$AgeUSdriving_3)
insurance_data$AgeUSdriving_3=(ifelse(insurance_data$AgeUSdriving_3==0,26,insurance_data$AgeUSdriving_3))

# Method2
# library(lubridate)
# reference_year=insurance_data$DOB1 %m+% years(insurance_data$AgeUSdriving_1)
# reference_year
# ref2=as.integer(round((reference_year - insurance_data$DOB2)/365.25,0))
# ref3=as.integer(round((reference_year - insurance_data$DOB3)/365.25,0))
# 
# insurance_data$AgeUSdriving_1
# insurance_data$AgeUSdriving_2=ifelse(insurance_data$AgeUSdriving_2==0,ref2,insurance_data$AgeUSdriving_2)
# insurance_data$AgeUSdriving_3
# insurance_data$AgeUSdriving_3=ifelse(insurance_data$AgeUSdriving_3==0,ref3,insurance_data$AgeUSdriving_3)

insurance_data$DOB1=NULL
insurance_data$DOB2=NULL
insurance_data$DOB3=NULL

insurance_data$AverageAge <- ((insurance_data$AgeUSdriving_1+insurance_data$AgeUSdriving_2+
                            insurance_data$AgeUSdriving_3)/3 )

insurance_data$AgeUSdriving_1 <- NULL
insurance_data$AgeUSdriving_2 <- NULL
insurance_data$AgeUSdriving_3 <- NULL

insurance_data$VehicleInspected_1 <- as.factor(insurance_data$VehicleInspected_1)
insurance_data$Units <- as.factor(insurance_data$Units)
insurance_data$Year_1 <- as.factor(insurance_data$Year_1)
insurance_data$CoverageLiability <- as.factor(insurance_data$CoverageLiability)
insurance_data$total_voilation_points <- as.factor(insurance_data$total_voilation_points)
insurance_data$Amendment <- as.factor(insurance_data$Amendment)
insurance_data$Rental_1 <- as.factor(insurance_data$Rental_1)
insurance_data$Towing_1 <- as.factor(insurance_data$Towing_1)
insurance_data$Billing_Term <- as.factor(insurance_data$Billing_Term)
insurance_data$CoveragePD_1 <- as.factor(insurance_data$CoveragePD_1)

#--------------------Missing Value Treatment using MICE Package---------------------------#

sapply(insurance_data, function(x) sum(is.na(x)))

#This dataframe does not contain the columns with missing values
dataframe <- as.data.frame(insurance_data)

sapply(dataframe, function(x) sum(is.na(x)))

library(mice)
set.seed(100)
init = mice(dataframe, maxit=0) 
meth = init$method
predM = init$predictorMatrix
meth

set.seed(103)
imputed = mice(dataframe, method=meth, predictorMatrix=predM, m=3)

imputed <- complete(imputed)
sapply(imputed, function(x) sum(is.na(x)))

sum(is.na(imputed))

#----------------------------Outlier Treatment----------------------#
# 
# table(imputed$Total_Distance_To_Work)
# histogram(imputed$Total_Distance_To_Work)
# table(log10(imputed$Total_Distance_To_Work+1),imputed$ClaimStatus)
# boxplot(log10(imputed$Total_Distance_To_Work+1))
# histogram(log10(imputed$Total_Distance_To_Work+1))
# 
# boxplot(log10(imputed$Total_Distance_To_Work+1))
# boxplot(imputed$Total_Distance_To_Work)
# histogram(imputed$Total_Distance_To_Work)
# 
# imputed$Total_Distance_To_Work <- log10(imputed$Total_Distance_To_Work+1)
# outlier_values <- boxplot.stats(imputed$Total_Distance_To_Work)$out
# outlier_values
# 
# table(imputed$Premium)
# histogram(imputed$Premium)
# boxplot(log10(insurance_data$Premium))
# histogram(log10(insurance_data$Premium))
log10(insurance_data$Premium)
# outlier_values <- boxplot.stats(insurance_data$Premium)$out
# outlier_values
# 
# imputed$Premium <- log10(imputed$Premium+1)
# boxplot(imputed$Premium)
# histogram(imputed$Premium)
# 
# table(imputed$Premium,imputed$ClaimStatus)
# 
# histogram(imputed$AverageAge)

#---------------------------Data Partition------------------------------#

library(caret)
set.seed(123)
training.samples <- imputed$ClaimStatus %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- imputed[training.samples, ]
test.data <- imputed[-training.samples, ]

prop.table(table(train.data$ClaimStatus))

prop.table(table(test.data$ClaimStatus))


#------------- Using SMOTE to handle imbalance in the target class------------------------#

library(DMwR)
set.seed(100)
train_balanced = SMOTE(ClaimStatus ~ ., train.data, perc.over = 600, perc.under=116, k=10)

prop.table(table(train_balanced$ClaimStatus))

#Previous Train Data
table(train.data$ClaimStatus)
prop.table(table(train.data$ClaimStatus))
#Synthetic Train Data
table(train_balanced$ClaimStatus)
prop.table(table(train_balanced$ClaimStatus))

#Test Data
table(test.data$ClaimStatus)
prop.table(table(test.data$ClaimStatus))
#Test Data Proportion


#----------------------------Multinomial Logistic Regression----------------------------------#

library(nnet)

set.seed(100)
multinom <- multinom(ClaimStatus ~ ., data = train_balanced, maxit=3000)
summary(multinom)
#Prediction on Train-------------------------------------- (70,72,70) /(75,75,76)
predicted=predict(multinom,train_balanced,type="class")

confusionMatrix(predicted,
                train_balanced$ClaimStatus,
                mode="everything",positive='1')

#Prediction on Test ----------------------------------------- (73,70,25) / (75,62,25)
predicted=predict(multinom,test.data,type="class")

confusionMatrix(predicted,
                test.data$ClaimStatus,
                mode="everything",positive='1')


#--------------------------Logistic Regression------------------------#
library(GMDH2)

logit = glm(ClaimStatus~ .,data= train_balanced, family = binomial(link = "logit"))
summary(logit)

summary(logit)
prediction <- predict(logit,type = "response",data=train_balanced)
cutoff = ifelse(prediction>0.5,1,0)
confmat = table(Predicted=cutoff,Actual=train_balanced$ClaimStatus)
confMat(confmat, positive = "1")      #------------------------------------(72,63,78)

prediction_test <- predict(logit,type = "response", newdata = test.data)
cutoff1 = ifelse(prediction_test>0.5,1,0)       
confmat1 = table(Predicted=cutoff1,Actual=test.data$ClaimStatus)
confMat(confmat1, positive = "1") 

logit1 = glm(ClaimStatus~ Premium+Renewed+CoverageLiability+Amendment+Rental_1+
             Units+Billing_Term+Towing_1+Year_1+AgeUSdriving_3,
             data= train_balanced, family = binomial(link = "logit"))


logit1 = glm(ClaimStatus~ Premium+Renewed+CoverageLiability+Amendment+Rental_1+
               Units+Billing_Term+Towing_1+Year_1+AgeUSdriving_3,
             data= train_balanced, family = binomial(link = "logit"))

summary(logit1)
prediction <- predict(logit1,type = "response",data=train_balanced)
cutoff = ifelse(prediction>0.5,1,0)
confmat = table(Predicted=cutoff,Actual=train_balanced$ClaimStatus)
confMat(confmat, positive = "1")      #------------------------------------(72,63,78)

prediction_test <- predict(logit1,type = "response", newdata = test.data)
cutoff1 = ifelse(prediction_test>0.5,1,0)       
confmat1 = table(Predicted=cutoff1,Actual=test.data$ClaimStatus)
confMat(confmat1, positive = "1")          #--------------------------------(80,62,31)

varImp(logit)

library(pscl)
pR2(logit1)

#Mcfadden is 0.22 so most the values are close to 0 i.e. Most customers as retained.
#out of 2334 customers in training data, retained customers are more than churned.

library(lmtest)
lrtest(logit1)
#Chisq likelihood ratio is significant, p value suggests to reject null Hypothesis.
#hence  Model is significant.

# Check for Multicolinearity
library(car)
vif(logit1)
# VIF <10 hence there is no multicolinearity among the variables.

#-------------------------------CART----------------------------------#


library(rpart)

r.ctrl = rpart.control(minsplit=10, minbucket = 4,cp=0,xval = 10)

set.seed(100)
cart.model <- rpart(ClaimStatus ~ ., data = train_balanced, method = "class", control = r.ctrl)

printcp(cart.model)
r.ctrl = rpart.control(minsplit=10, minbucket = 4,cp=0.009,xval = 10)

plot(cart.model)
#Prediction on Train --------------------------------------------(89,90,80)
predicted <- predict(cart.model, train_balanced, type="class")

confusionMatrix(predicted,
                train_balanced$ClaimStatus,
                mode="everything",positive='1')

#Prediction on Test ---------------------------------------------(75,91,43)
predicted <- predict(cart.model, test.data, type="class") 

confusionMatrix(predicted,
                test.data$ClaimStatus,
                mode="everything",positive='1')

#---------------------------------Random Forest-------------------------------------#

library(randomForest)
# Evaluate Model performance
library(e1071)
library(caret)

set.seed(130)
RF <- randomForest(ClaimStatus ~ ., data = train_balanced, 
                   ntree=501, mtry = 4, nodesize =20,
                   importance=TRUE)

RF <- tuneRF(x = train_balanced[,-1], 
              y=train_balanced$ClaimStatus,
              mtryStart = 4, 
              ntreeTry=301, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 15, 
              importance=TRUE
)

varImpPlot(tRF)

print(RF)

#Prediction on Train --------------------------------------------(97,95,91)
predicted <- predict(RF, train_balanced, type="class")

confusionMatrix(predicted,
                train_balanced$ClaimStatus,
                mode="everything",positive='1')

#Prediction on Test ---------------------------------------------(83,90,18)
predicted <- predict(RF, test.data, type="class")

confusionMatrix(predicted,
                test.data$ClaimStatus,
                mode="everything",positive='1')


#--------------------------Model Performance Measures------------------------#


library(ROCR)
test.roc <- prediction(prediction_test, testTransformed$ClaimStatus)
plot(performance(test.roc, "tpr", "fpr"), 
     col = "red", main = "ROC Curve for train data")
abline(0, 1, lty = 8, col = "blue")

# AUC
test.auc = performance(test.roc, "auc")
test.area = as.numeric(slot(test.auc, "y.values"))
test.area


# KS
ks.test <- performance(test.roc, "tpr", "fpr")
test.ks <- max(attr(ks.test, "y.values")[[1]] - (attr(ks.test, "x.values")[[1]]))
test.ks
#KS=46.08, indicates a good model.


# Gini
test.gini = (2 * test.area) - 1
test.gini

#The Loss for the model through GINI is 0.5094

as.data.frame(sapply(df_data_4, function(x) sum(is.na(x))*100/184))


