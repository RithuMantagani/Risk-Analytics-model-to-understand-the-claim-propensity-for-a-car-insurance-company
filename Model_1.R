#invoking necessary libraries####
library(stringi)
library(psych)
library(xlsx)
library(openxlsx)
library(mice)
library(woeBinning)
library(dplyr)
library(Hmisc)
library(dlookr)
library(caTools)
library(nnet)
library(rpart)
library(rpart.plot)
library(caret)
library(DMwR)
#Reding data into a dataframe#####
insurance_data = read.csv(file.choose())
#filtering data with number of drivers=1
insurance_data=filter(insurance_data, Number_of_Driver==1)
#Removing unwanted variables######
Driver1_insurance_data=subset(insurance_data,select = -c(1,2,8:13,15:19,27:30,32,55:58,60:63,65:68,71:74,82:85,87:90,92:95,97:100,102:105,107:110,112:115,117:120,125))
Driver1_insurance_data$amend<- insurance_data$Amendment


#Changing variables to factor type######
Driver1_insurance_data$ClaimFrequency<-as.factor(Driver1_insurance_data$ClaimFrequency)
Driver1_insurance_data$Billing_Term<-as.factor(Driver1_insurance_data$Billing_Term)
Driver1_insurance_data$DriverAssigned_1<-as.factor(Driver1_insurance_data$DriverAssigned_1)
Driver1_insurance_data$ClaimStatus<-as.factor(Driver1_insurance_data$ClaimStatus)

sum(is.na(Driver1_insurance_data))
#####Check missing value %#####

as.data.frame(sapply(Driver1_insurance_data,function(x) sum(is.na(x))*100))
#log transformation for premium and Age
histogram(Driver1_insurance_data$AgeUSdriving_1)
table(Driver1_insurance_data$AgeUSdriving_1)
histogram(Driver1_insurance_data$AgeUSdriving_1)
Driver1_insurance_data$AgeUSdriving_1<-log2(Driver1_insurance_data$AgeUSdriving_1)
histogram(Driver1_insurance_data$AgeUSdriving_1)
boxplot(Driver1_insurance_data$AgeUSdriving_1)


histogram(Driver1_insurance_data$Premium)
table(Driver1_insurance_data$Premium)
Driver1_insurance_data<-Driver1_insurance_data[!(Driver1_insurance_data$Premium==0),]

Driver1_insurance_data$Premium<- (log10(Driver1_insurance_data$Premium+10))
summary(log10(Driver1_insurance_data$Premium+10))
histogram(Driver1_insurance_data$Premium)

sum(is.infinite(Driver1_insurance_data$Premium))
boxplot(Driver1_insurance_data$Premium)
histogram(Driver1_insurance_data$Premium)
table(Driver1_insurance_data$Premium,Driver1_insurance_data$ClaimStatus)

table(Driver1_insurance_data$ExcludedDriverName_01)

#treating Exculdeddriver variables
Driver1_insurance_data$ExcludedDriverName_1<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_01)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_2<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_02)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_3<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_03)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_4<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_04)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_5<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_05)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_6<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_06)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_7<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_07)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_8<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_08)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_9<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_09)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_10_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_10)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_11_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_11)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_12_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_12)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_13_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_13)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_14_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_14)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_15_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_15)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_16_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_16)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_17_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_17)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_18_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_18)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_19_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_19)>1,1,0)
Driver1_insurance_data$ExcludedDriverName_20_new<-ifelse(stri_length(Driver1_insurance_data$ExcludedDriverName_20)>1,1,0)

#Making new data set removing old excluded driver columns variables

Driver1_insurance_data1<-subset(Driver1_insurance_data,select = -c(15:34))
Driver1_insurance_data1$Renewed<-as.factor(Driver1_insurance_data1$Renewed)
Driver1_insurance_data1$GaragedZIP_1<-as.factor(Driver1_insurance_data1$GaragedZIP_1)
Driver1_insurance_data1$Units<-as.factor(Driver1_insurance_data1$Units)
Driver1_insurance_data1$VehicleInspected_1<-as.factor(Driver1_insurance_data1$VehicleInspected_1)
Driver1_insurance_data1$Zip<-as.factor(Driver1_insurance_data1$Zip)


#treating outliers for Towing Variable 0.20% of data is outlier so treating the outlier
table(Driver1_insurance_data1$Towing_1)
Driver1_insurance_data1$Towing_1[Driver1_insurance_data1$Towing_1=="50"]<-0
Driver1_insurance_data1$Towing_1[Driver1_insurance_data1$Towing_1=="70"]<-0

#treating outliers for year 1 variable
Driver1_insurance_data1$Year_1[Driver1_insurance_data1$Year_1==988]<-1998##logically

Driver1_insurance_data1<-Driver1_insurance_data1[!(Driver1_insurance_data1$Year_1==0),]
Driver1_insurance_data1<-Driver1_insurance_data1[!(Driver1_insurance_data1$Year_1==-3),]
Driver1_insurance_data1<-Driver1_insurance_data1[!(Driver1_insurance_data1$Year_1==199),]
Driver1_insurance_data1$Year_1<-as.Date.character(Driver1_insurance_data1$Year_1,format="%Y")
Driver1_insurance_data1$Year_1=ifelse(Driver1_insurance_data1$Year_1<=2000,0,1)
table(Driver1_insurance_data1$Year_1)

#treating Rental_1 variable for outliers, 0.25% of data indentified as outliers
table(Driver1_insurance_data1$Rental_1)
Driver1_insurance_data1$Rental_1[Driver1_insurance_data1$Rental_1==20]<-0
Driver1_insurance_data1$Rental_1[Driver1_insurance_data1$Rental_1==25]<-0
Driver1_insurance_data1$Rental_1[Driver1_insurance_data1$Rental_1==35]<-0
Driver1_insurance_data1$Rental_1<-as.factor(Driver1_insurance_data1$Rental_1)


#Recoding the Make_1 and occupation variables

level_key <- c("ACURA"="ACURA","AMG"="AMG","NISSAN"="nissan",
               "Pontic"="pontic","PONTIAC"="pontic","Pontiac"="pontic",
               "Chev"="CHEVROLET","Ford"="FORD","AUDI"="AUDI","BMW"="BMW",
               "BUIC"="GM","BUICK"="GM","Buick"="GM"
               ,"CADDY"="CADILLAC","CADILLAC"="CADILLAC",
               "CHEV"="CHEVROLET","CHEVERLET"="CHEVROLET","CHEVEROLET"="CHEVROLET",
               "CHEVORLET"="CHEVROLET","CHEVROET"="CHEVROLET","CHEVROLET"="CHEVROLET",
               "CHEVROLET`"="CHEVROLET","CHEVROLETQ"="CHEVROLET","CHEVY"="CHEVROLET",
               "CHEVYVAN"="CHEVROLET","CHRYLER"="CHRYSLER","CHRYSLER"="CHRYSLER",
               "CHRYSLR"="CHRYSLER","CUTLASS"="OLDSMOBILE","DAEWOO"="DAEWOO",
               "DAEWOOD"="DAEWOO","DATS"="DATSUN","DODGE"="DODGE","EAGLE"="EAGLE",
               "FORD"="FORD","FORED"="FORD","FORK"="FORD",
               "G.M.C"="GM","GENERALMO"="GM","GEO"="GM","GMA"="GM","GeneralMo"="GM","Gmc"="GM",
               "GMC"="GM","HONDA"="HONDA","HUMDAI"="HYUNDAI","HUMMER"="HUMMER",
               "HUNDAI"="HYUNDAI","HYANDI"="HYUNDAI","HYNDAI"="HYUNDAI","HYUDAI"="HYUNDAI",
               "HYUNDAI"="HYUNDAI","HYUNDIA"="HYUNDAI","HYUNDY"="HYUNDAI","Infiniti"="INFINITI",
               "INFINIT"="INFINITI","INFINITI"="INFINITI","ISUZI"="ISUZU",
               "ISUZU"="ISUZU","IZUSU"="ISUZU","JAGUAR"="JAGUAR","JEEP"="JEEP","Cheverlet"="CHEVROLET",
               "JEEPAMGC"="JEEP","KIA"="KIA","KIAMOTORS"="KIA","LANDROVER"="LANDROVER","Chevorlet"="CHEVROLET",
               "LEXUS"="LEXUS","LINC"="LINCOLN","LINCOLN"="LINCOLN","LINCON"="LINCOLN",
               "MAZDA"="MAZDA","MERC"="MERCEDES","MERCEDE"="MERCEDES","MERCEDES"="MERCEDES",
               "MERCEDESB"="MERCEDES","MERCEDES-B"="MERCEDES","MERCURY"="MERCURY","NISSAN"="nissan",
               "MINI"="MINI","MITS"="MITSUBISHI","MITSUBI"="MITSUBISHI","MITSUBISHI"="MITSUBISHI",
               "NISS"="NISSAN","NISSAN"="NISSAN","NON-OWNERS"="NO-OWNER","NO-OWNERS"="NO-OWNER",
               "OLDS"="OLDSMOBILE","OLDSMOBILE"="OLDSMOBILE","PICKUP"="DODGE","PLYM"="PLYMOUTH",
               "PLYMOUT"="PLYMOUTH","PLYMOUTH"="PLYMOUTH","PLYMT"="PLYMOUTH","PONT"="PONTIAC",
               "PONTIAC"="PONTIAC","PONTIC"="PONTIAC","PORSCHE"="PORSCHE","SAAB"="SAAB",
               "SATURN"="SATURN","SCION"="SCION","SUBARU"="SUBARU","SUZUKI"="SUZUKI",
               "TOYOTA"="TOYOTA","VAN"="FORD","VOLK"="VOLKSWAGEN","VOLKS"="VOLKSWAGEN","Chevy"="CHEVROLET",
               "VOLKSWAGEN"="VOLKSWAGEN","VOLVO"="VOLKSWAGEN","VW"="VOLKSWAGEN","WRANGLER"="WRANGLER")
Driver1_insurance_data1$Make_1=dplyr::recode(Driver1_insurance_data1$Make_1, !!! level_key)
Driver1_insurance_data1$Make_1=as.factor(Driver1_insurance_data1$Make_1)
table(Driver1_insurance_data1$Make_1,Driver1_insurance_data1$ClaimStatus)
Driver1_insurance_data1$Make_1<-ifelse(Driver1_insurance_data1$Make_1=="GM"| Driver1_insurance_data1$Make_1=="nissan"| Driver1_insurance_data1$Make_1=="pontic"| Driver1_insurance_data1$Make_1=="FORD"|Driver1_insurance_data1$Make_1=="CHEVROLET"| Driver1_insurance_data1$Make_1=="dodge"|Driver1_insurance_data1$Make_1=="mazda"|Driver1_insurance_data1$Make_1=="nissan","Popular_cars","others")
table(Driver1_insurance_data1$Make_1,Driver1_insurance_data1$ClaimStatus)
#converting to factor varibable
Driver1_insurance_data1$NoLossSigned<-as.factor(Driver1_insurance_data1$NoLossSigned)
Driver1_insurance_data1$amend<-as.factor(Driver1_insurance_data1$amend)

#--------------------Check for blank values values---------------------------#

Driver1_insurance_data1$CoverageMP[Driver1_insurance_data1$CoverageMP==""]<-NA
summary(Driver1_insurance_data1$CoverageMP)
Driver1_insurance_data1$CoveragePD_1[Driver1_insurance_data1$CoveragePD_1==""]<-NA
summary(Driver1_insurance_data1$CoveragePD_1)
Driver1_insurance_data1$CoveragePIP_CDW[Driver1_insurance_data1$CoveragePIP_CDW==""]<-NA
summary(Driver1_insurance_data1$CoveragePIP_CDW)
Driver1_insurance_data1$CoverageUMBI[Driver1_insurance_data1$CoverageUMBI==""]<-NA
summary(Driver1_insurance_data1$CoverageUMBI)
Driver1_insurance_data1$CoverageUMPD[Driver1_insurance_data1$CoverageUMPD==""]<-NA
summary(Driver1_insurance_data1$CoverageUMPD)

Driver1_insurance_data1$Surcharge1Unit_1[Driver1_insurance_data1$Surcharge1Unit_1==""]<-NA
Driver1_insurance_data1$Surcharge2Unit_1[Driver1_insurance_data1$Surcharge2Unit_1==""]<-NA
Driver1_insurance_data1$Surcharge3Unit_1[Driver1_insurance_data1$Surcharge3Unit_1==""]<-NA
summary(Driver1_insurance_data1$Surcharge1Unit_1)
sapply(Driver1_insurance_data1, function(x) sum(is.na(x)))
dataframe <- as.data.frame(Driver1_insurance_data1[,c(7:12,21:23)])


as.data.frame(sapply(Driver1_insurance_data1,function(x) sum(is.na(x))*100/8545))

init = mice(dataframe, maxit=0) 
meth = init$method
predM = init$predictorMatrix
meth
set.seed(100)
imputed = mice(dataframe, method=meth, predictorMatrix=predM, m=3)
imputed <- complete(imputed)
sapply(imputed, function(x) sum(is.na(x)))

summary(imputed)
##adding violation points to one variable
attach(Driver1_insurance_data1)
Driver1_insurance_data1$total_violpt<-rowSums(Driver1_insurance_data1[27:34])
Driver1_insurance_Final<-subset(Driver1_insurance_data1,select = -c(27:34,37,17:18,12))
#Imputing values for NA's
Driver1_insurance_Final$Surcharge1Unit_1<-imputed$Surcharge1Unit_1
Driver1_insurance_Final$Surcharge2Unit_1<-imputed$Surcharge2Unit_1
Driver1_insurance_Final$Surcharge3Unit_1<-imputed$Surcharge3Unit_1
Driver1_insurance_Final$CoverageMP<-imputed$CoverageMP
Driver1_insurance_Final$CoveragePD_1<-imputed$CoveragePD_1
Driver1_insurance_Final$CoveragePIP_CDW<-imputed$CoveragePIP_CDW
Driver1_insurance_Final$CoverageUMBI<-imputed$CoverageUMBI





#adding up excuded driver
Driver1_insurance_Final$total_excld_drivers<-rowSums(Driver1_insurance_Final[31:50])
Driver1_insurance_Final<-Driver1_insurance_Final[-c(31:50)]
table(Driver1_insurance_Final$ClaimStatus, Driver1_insurance_Final$total_excld_drivers)

Driver1_insurance_Final<-Driver1_insurance_Final[!(Driver1_insurance_Final$total_excld_drivers==11),]
Driver1_insurance_Final<-Driver1_insurance_Final[!(Driver1_insurance_Final$total_excld_drivers==14),]
Driver1_insurance_Final<-Driver1_insurance_Final[!(Driver1_insurance_Final$total_excld_drivers==13),]
Driver1_insurance_Final<-Driver1_insurance_Final[!(Driver1_insurance_Final$total_excld_drivers==15),]
Driver1_insurance_Final<-Driver1_insurance_Final[!(Driver1_insurance_Final$total_excld_drivers==17),]
Driver1_insurance_Final<-Driver1_insurance_Final[!(Driver1_insurance_Final$total_excld_drivers==20),]
Driver1_insurance_Final<-Driver1_insurance_Final[!(Driver1_insurance_Final$total_excld_drivers==9),]
Driver1_insurance_Final$total_excld_drivers<-as.factor(Driver1_insurance_Final$total_excld_drivers)
Driver1_insurance_Final$total_excld_drivers<-ifelse(Driver1_insurance_Final$total_excld_drivers=="0","No_driver_excluded","driver_excluded")
#install.packages("xlsx")
sum(is.na(Driver1_insurance_Final))
Driver1_insurance_Final<- na.omit(Driver1_insurance_Final)
Driver1_insurance_Final$total_violpt<-as.numeric(Driver1_insurance_Final$total_violpt)

histogram(Driver1_insurance_Final$DistanceToWork_1)
table(Driver1_insurance_Final$DistanceToWork_1)
Driver1_insurance_Final$DistanceToWork_1<-(log10(Driver1_insurance_Final$DistanceToWork_1)+10)
boxplot(Driver1_insurance_Final$DistanceToWork_1)
Driver1_insurance_Final$DistanceToWork_1<-as.numeric(Driver1_insurance_Final$DistanceToWork_1)
Driver1_insurance_Final$DistanceToWork_1<-ifelse(Driver1_insurance_Final$DistanceToWork_1>11,"Long distance","short distance")
table(Driver1_insurance_Final$DistanceToWork_1,Driver1_insurance_Final$ClaimStatus)
#Creating SMOTE dataset and clubbing levels

library(DMwR)
Driver1_insurance_Final$CoverageUMBI<-ifelse(Driver1_insurance_Final$CoverageUMBI=="None","others","Accepted")
Driver1_insurance_Final$CoverageMP<-ifelse(Driver1_insurance_Final$CoverageMP=="None","others","Accepted")
Driver1_insurance_Final$CoveragePD_1<-ifelse(Driver1_insurance_Final$CoveragePD_1=="500/500","Accepted","other")
Driver1_insurance_Final$CoveragePIP_CDW<-ifelse(Driver1_insurance_Final$CoveragePIP_CDW=="None","others","Accepted")
Driver1_insurance_Final$Surcharge1Unit_1<-ifelse(Driver1_insurance_Final$Surcharge1Unit_1=="N","N","Y")
Driver1_insurance_Final$Surcharge2Unit_1<-ifelse(Driver1_insurance_Final$Surcharge2Unit_1=="N","N","Y")
Driver1_insurance_Final$Surcharge3Unit_1<-ifelse(Driver1_insurance_Final$Surcharge3Unit_1=="N","N","Y")
Driver1_insurance_Final$CoveragePD_1<-as.factor(Driver1_insurance_Final$CoveragePD_1)
Driver1_insurance_Final$CoverageMP<-as.factor(Driver1_insurance_Final$CoverageMP)
Driver1_insurance_Final$CoveragePIP_CDW<-as.factor(Driver1_insurance_Final$CoveragePIP_CDW)
Driver1_insurance_Final$Surcharge1Unit_1<-as.factor(Driver1_insurance_Final$Surcharge1Unit_1)
Driver1_insurance_Final$Surcharge2Unit_1<-as.factor(Driver1_insurance_Final$Surcharge2Unit_1)
Driver1_insurance_Final$Surcharge3Unit_1<-as.factor(Driver1_insurance_Final$Surcharge3Unit_1)

table(Driver1_insurance_Final$Surcharge3Unit_1,Driver1_insurance_Final$ClaimStatus)
Driver1_insurance_Final$CoverageUMBI<-as.factor(Driver1_insurance_Final$CoverageUMBI)
table(Driver1_insurance_Final$CoverageUMBI,Driver1_insurance_Final$ClaimStatus)
Driver1_insurance_Final$DriverAssigned_1<-ifelse (Driver1_insurance_Final$DriverAssigned_1==2 |Driver1_insurance_Final$DriverAssigned_1==3 |Driver1_insurance_Final$DriverAssigned_1==4,"others","1")
Driver1_insurance_Final$DriverAssigned_1<-as.factor(Driver1_insurance_Final$DriverAssigned_1)
table(Driver1_insurance_Final$DriverAssigned_1,Driver1_insurance_Final$ClaimStatus)
table(Driver1_insurance_Final$Type,Driver1_insurance_Final$ClaimStatus)
Driver1_insurance_Final$Type<-ifelse(Driver1_insurance_Final$Type=="AP"|Driver1_insurance_Final$Type=="FC"
                                     |Driver1_insurance_Final$Type=="RET"|Driver1_insurance_Final$Type=="VD"
                                     |Driver1_insurance_Final$Type=="A","others",as.factor(Driver1_insurance_Final$Type))
Driver1_insurance_Final$Type<-as.factor(Driver1_insurance_Final$Type)

Driver1_insurance_Final$total_violpt<-ifelse(Driver1_insurance_Final$total_violpt==1,"1","more than 1")
table(Driver1_insurance_Final$ClaimStatus,Driver1_insurance_Final$total_violpt)
Driver1_insurance_Final$total_violpt<-as.factor(Driver1_insurance_Final$total_violpt)
table(Driver1_insurance_Final$total_excld_drivers,Driver1_insurance_Final$ClaimStatus)
table(Driver1_insurance_Final$DistanceToWork_1,Driver1_insurance_Final$ClaimStatus)
Driver1_insurance_Final$DistanceToWork_1<-as.factor(Driver1_insurance_Final$DistanceToWork_1)
Driver1_insurance_Final$Towing_1<-as.factor(Driver1_insurance_Final$Towing_1)
Driver1_insurance_Final$Year_1<-as.factor(Driver1_insurance_Final$Year_1)
Driver1_insurance_Final$Make_1<-as.factor(Driver1_insurance_Final$Make_1)
Driver1_insurance_Final$total_violpt<-as.factor(Driver1_insurance_Final$total_violpt)
Driver1_insurance_Final$total_excld_drivers<-as.factor(Driver1_insurance_Final$total_excld_drivers)
sum(is.na(Driver1_insurance_Final))
table(Driver1_insurance_Final$ClaimStatus,Driver1_insurance_Final$Make_1)
table(Driver1_insurance_Final$ClaimStatus,Driver1_insurance_Final$amend)
Driver1_insurance_Final$amend<-ifelse(Driver1_insurance_Final$amend=="0","N","Y")
Driver1_insurance_Final$amend<-as.factor(Driver1_insurance_Final$amend)
plot(Driver1_insurance_Final$ClaimStatus,Driver1_insurance_Final$AgeUSdriving_1)


boxplot(Driver1_insurance_Final$Premium)
str(Driver1_insurance_Final)
set.seed(149)
train=sample(x=c(TRUE,FALSE),size=nrow(Driver1_insurance_Final),replace=TRUE,prob=c(0.70,0.30))
training1<-Driver1_insurance_Final[train,]

testing1<-Driver1_insurance_Final[!train,]
#---------------------------Scaling--------------------------------#

preProcValues <- preProcess(training1, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training1)
testTransformed <- predict(preProcValues, testing1)
#write.xlsx(training1,file="C:\\Users\\Baba\\Desktop\\Capstone\\train_new.xlsx")
#write.xlsx(testing1,file="C:\\Users\\Baba\\Desktop\\Capstone\\test_new.xlsx")
## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
set.seed(121)
data_SMOTE<- SMOTE(ClaimStatus ~ .,trainTransformed ,perc.over = 600, perc.under=150, k=10)

table(data_SMOTE$ClaimStatus)
table(testing1$ClaimStatus)
table(training1$ClaimStatus)

str(Driver1_insurance_Final)
sum(is.na(Driver1_insurance_Final))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~LASSO~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

train_lasso<-data_SMOTE[,-c(14,26,2)]
#View(train_lasso)
test_lasso<-testTransformed[,-c(14,26,2)]

train_dummy_coded <- model.matrix(ClaimStatus ~., data=train_lasso)
test_dummy_coded <- model.matrix(ClaimStatus ~., data=test_lasso)
library(glmnet)
library(dplyr)
# Setting the seed
set.seed(123)


# Alpha = 1 (Lasso), Alpha=0 (Ridge)
lasso_model <- glmnet(train_dummy_coded, train_lasso$ClaimStatus, family="binomial", nlambda=200, alpha = 1)

# Shows for particular value of lambda
# how many features you have
print(lasso_model)
select_coeff <- as.matrix(coef(lasso_model, s=0.02))
select_coeff_df <- data.frame(Feature=rownames(select_coeff), Coef=select_coeff[,1])
select_coeff_df
select_coeff_df %>% filter(Coef != 0)


# Prediction
lambda_select = 0.02
# type=class will predict class
# type=response will predict probability
lasso_pred_train <- predict(lasso_model, newx =train_dummy_coded , type="response", s=lambda_select)[,1]
lasso_pred_test <-  predict(lasso_model, newx =test_dummy_coded, type="response", s=lambda_select)[,1]

pred_df_train <- data.frame(Actual=train_lasso$ClaimStatus, Pred=lasso_pred_train)
pred_df_train$Pred<-ifelse(pred_df_train$Pred <0.4,0,1)
pred_df_train
pred_df_test<- data.frame(Actual=test_lasso$ClaimStatus, Pred=lasso_pred_test)
pred_df_test$Pred<-ifelse(pred_df_test$Pred <0.4,0,1)
pred_df_test
library(e1071)
confmat_lasso_train = table(pred_df_train$Pred,pred_df_train$Actual)
confmat_lasso = table(pred_df_test$Pred,pred_df_test$Actual)
confusionMatrix(confmat_lasso,positive="1",mode="everything")
confusionMatrix(confmat_lasso_train,positive="1",mode="everything")

#--------------------------Model Performance Measures------------------------#


library(ROCR)
train.roc <- prediction(pred_df_train$Pred, train_lasso$ClaimStatus)
plot(performance(train.roc, "tpr", "fpr"), 
     col = "red", main = "ROC Curve for train data")
abline(0, 1, lty = 8, col = "blue")

# AUC
train.auc = performance(train.roc, "auc")
train.area = as.numeric(slot(train.auc, "y.values"))
train.area
#AUC=0.81

# KS
ks.train <- performance(train.roc, "tpr", "fpr")
train.ks <- max(attr(ks.train, "y.values")[[1]] - (attr(ks.train, "x.values")[[1]]))
train.ks
#KS=63.78, indicates a good model.


# Gini
train.gini = (2 * train.area) - 1
train.gini

#The Loss for the model through GINI is 0.637



library(ROCR)
test.roc <- prediction(pred_df_test$Pred, test_lasso$ClaimStatus)
plot(performance(test.roc, "tpr", "fpr"), 
     col = "red", main = "ROC Curve for train data")
abline(0, 1, lty = 8, col = "blue")

# AUC
test.auc = performance(test.roc, "auc")
test.area = as.numeric(slot(test.auc, "y.values"))
test.area
#AUC=0.766

# KS
ks.test <- performance(test.roc, "tpr", "fpr")
test.ks <- max(attr(ks.test, "y.values")[[1]] - (attr(ks.test, "x.values")[[1]]))
test.ks
#KS=0.532, indicates a good model.


# Gini
test.gini = (2 * test.area) - 1
test.gini

#The Loss for the model through GINI is 0.5094

#~~~~~~~~~~~~~~~~~~~~~~~~~`Multinom~~~~~~~~~~~~~~~~~~~~~~`
library(GMDH2)
train_log=data_SMOTE[,-c(14,26,2)]
test_log=trainTransformed[,-c(14,26,2)]


#multinom model
multinom1 <- multinom(ClaimStatus~Premium+Billing_Term+Renewed+DriverAssigned_1+Rental_1+Surcharge3Unit_1
                      +Make_1+NoLossSigned+Type+amend+CancellationType+total_violpt, data = train_log)
summary(multinom1)

logit1 <- glm (ClaimStatus~ Premium + Billing_Term + Renewed  + 
                 CoverageUMBI  + DriverAssigned_1 + Units+
                 Surcharge3Unit_1 + Towing_1  + Make_1 + NoLossSigned+ 
                 amend + total_violpt + total_excld_drivers, data=train_log, family = binomial)
alias(logit1)
summary(logit1)


#check for multicolinearity 
library(car)
vif(logit1)

#1 Identify overall fitness of model using log likehood Ratio Test

#install.packages("lmtest")
library(lmtest)

lrtest(multinom1)

#2 Calculating McFadden's Rsquared (Minimum McFadden Rsquared considered for model fitness is 10%)
#install.packages("pscl")
library(pscl)
pR2(multinom1)
(coef(logit1))
library(caret)

#prediction for train
predicted_train_log = predict(logit1, newdata=train_log,type="response")

predicted_train_log<-ifelse(predicted_train_log<0.4,0,1)
table(train_log$ClaimStatus, predicted_train_log )
confmat_train_log = table(Actual=train_log$ClaimStatus,predicted_train_log)
confusionMatrix(confmat_train_log,positive="1",mode="everything")

#prediction for test
test_log$predict <- predict(logit1, test_log,type="response")
test_log$predict<-ifelse(test_log$predict<0.4,0,1)
confmat_test<-table(test_log$ClaimStatus, test_log$predict)
confusionMatrix(confmat_test,positive="1",mode="everything")

#--------------------------Model Performance Measures------------------------#


library(ROCR)
train.roc <- prediction(predicted_train_log, train_log$ClaimStatus)
plot(performance(train.roc, "tpr", "fpr"), 
     col = "red", main = "ROC Curve for train data")
abline(0, 1, lty = 8, col = "blue")

# AUC
train.auc = performance(train.roc, "auc")
train.area = as.numeric(slot(train.auc, "y.values"))
train.area
#AUC=0.81

# KS
ks.train <- performance(train.roc, "tpr", "fpr")
train.ks <- max(attr(ks.train, "y.values")[[1]] - (attr(ks.train, "x.values")[[1]]))
train.ks
#KS=63.78, indicates a good model.


# Gini
train.gini = (2 * train.area) - 1
train.gini

#The Loss for the model through GINI is 0.637



library(ROCR)
test.roc <- prediction(test_log$predict, test_log$ClaimStatus)
plot(performance(test.roc, "tpr", "fpr"), 
     col = "red", main = "ROC Curve for train data")
abline(0, 1, lty = 8, col = "blue")

# AUC
test.auc = performance(test.roc, "auc")
test.area = as.numeric(slot(test.auc, "y.values"))
test.area
#AUC=0.766

# KS
ks.test <- performance(test.roc, "tpr", "fpr")
test.ks <- max(attr(ks.test, "y.values")[[1]] - (attr(ks.test, "x.values")[[1]]))
test.ks
#KS=0.532, indicates a good model.


# Gini
test.gini = (2 * test.area) - 1
test.gini

#The Loss for the model through GINI is 0.5094



#~~~~~~~~~~~~~~~~~~~~~~~~~`Cart`~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`#`
train_CART<-data_SMOTE[,-c(2,14,26)]
#train_CART<-train_CART[,c(2,3,4,8,12,18,20,22,23,25,27)]
summary(train_CART)
sum(is.infinite(train_CART))
test_CART<-testing1[,-c(2,14,26)]

#View(training1)
set.seed=135
r.ctrl = rpart.control(minsplit=100, minbucket = 30, cp = 0, xval = 10)
m1 <- rpart(formula = ClaimStatus ~ Premium+Billing_Term+Renewed+DriverAssigned_1+Rental_1+Surcharge3Unit_1
            +Make_1+NoLossSigned+Type+amend+CancellationType+total_violpt, data =train_CART , method = "class",parms = list(split = 'information'),
                 control = r.ctrl)
m1



#Plot the tree
rpart.plot(m1)
plotcp(m1)



## to find how the tree performs and derive cp value
printcp(m1)

ptree<- prune(m1, cp= 0.001 ,"CP")
printcp(ptree)
rpart.plot(ptree)




## Predicting and Scoring the train sample
train_CART$predict.class <- predict(ptree, train_CART, type="class")
train_CART$predict.score <- predict(ptree, train_CART)
train_CART$predict.score<-ifelse(train_CART$predict.score<0.4,0,1)


## Predicting and Scoring the test sample

test_CART$predict.class <- predict(ptree, test_CART, type="class")
test_CART$predict.score <- predict(ptree, test_CART)
test_CART$predict.score<-ifelse(test_CART$predict.score<0.4,0,1)

with(test_CART, table(ClaimStatus, predict.class))

#confusion matrix
confmat_test_cart = table(Actual=test_CART$ClaimStatus,test_CART$predict.class)
confmat_cart = table(Actual=train_CART$ClaimStatus,train_CART$predict.class)
confmat_test_cart
confmat_cart
library(caret)
confusionMatrix(confmat_test_cart,positive="1",mode="everything")
confusionMatrix(confmat_cart,positive="1",mode="everything")

#checking imp variables
library(vip)
vip(ptree,num_features=40,bar=FALSE)


#~~~~~~~~~~~~~~~~~~~~~~RF~~~~~~~~~~~~~~~~~~~~~~~


library(randomForest)
train_RF=data_SMOTE[,-c(2,14,26)]
test_RF=testTransformed[,-c(2,14,26)]
View(train_RF)
## Calling syntax to build the Random Forest
RF <- randomForest(ClaimStatus ~ Premium+Billing_Term+Renewed+DriverAssigned_1+Rental_1+Surcharge3Unit_1
                   +Make_1+NoLossSigned+Type+amend+CancellationType+total_violpt, data = train_RF, 
                   ntree=501, mtry = 3, nodesize =10,
                   importance=TRUE)
print(RF)
plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest train_RF")

RF$err.rate
impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,3], decreasing=TRUE),]
## Tuning Random Forest
set.seed(123)
tRF <- randomForest(ClaimStatus ~., data = train_RF,
              mtryStart = 10, 
              ntreeTry=188, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 12, 
              importance=TRUE
)
print(tRF)
print(RF)
#A plot for the mean decrease in accuracy levels
varImpPlot(tRF)
## Predicting the Probabilities and the prediction class for the development data
train_RF_predict_prob <- predict(tRF, train_RF, type="prob")

head(train_RF)
class(train_RF_predict_prob)
library(MLmetrics)
train_RF_predict_prob
table(train_RF_predict_prob)

## Predicting the Probabilities and the prediction class for the Holdout data. 
##Would like to see how well the model is able to predict for the holdout data
test_RF$predict.class <- predict(tRF, test_RF, type="class")
test_RF$predict.score <- predict(tRF, test_RF, type="prob")
train_RF$predict.class <- predict(tRF, train_RF, type="class")
train_RF$predict.score <- predict(tRF, train_RF, type="prob")
test_RF$predict.score <- ifelse(test_RF$predict.score<0.4,0,1)
train_RF$predict.score <- ifelse(train_RF$predict.score<0.4,0,1)
test_RF$predict.class
test_RF$predict.score
MLmetrics::AUC(test_RF$predict.score[,2], 
               test_RF$ClaimStatus)

confmat_test_rf = table(Actual=test_RF$ClaimStatus,test_RF$predict.class)
confmat_train_rf=table(Actual=train_RF$ClaimStatus,train_RF$predict.class)
confusionMatrix(confmat_test_rf,positive="1",mode="everything")
confusionMatrix(confmat_train_rf,positive="1",mode="everything")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~NB~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("naivebayes")
library(naivebayes)

#Create Dummies
library(dummies)
New_data <- dummy.data.frame(Driver1_insurance_Final[,-c(2,14,26,1,3,6)], sep = ".")
names(New_data)


New_data$age<-Driver1_insurance_Final$AgeUSdriving_1
New_data$premium<-Driver1_insurance_Final$Premium
New_data$ClaimStatus<-Driver1_insurance_Final$ClaimStatus


library(caret)
set.seed(100)
index<-createDataPartition(y=New_data$ClaimStatus,p=0.7,list=FALSE)
train.data<-New_data[index,]
test.data<-New_data[-index,]

prop.table(table(train.data$ClaimStatus))
prop.table(table(test.data$ClaimStatus))

set.seed(100)
train_balance = SMOTE(ClaimStatus ~ ., train.data, perc.over = 600,perc.under = 400)
(table(train_balance$ClaimStatus))
prop.table(table(train_balance$ClaimStatus))
(table(test.data$ClaimStatus))

set.seed(100)

nb=naive_bayes(ClaimStatus~., data=train_balance,trcontrol=trainControl(method="cv",number = 10))
nb
nb_test_pred=predict(nb,test.data)
nb_train_pred=predict(nb,train_balance)
caret::confusionMatrix(nb_test_pred,test.data$ClaimStatus,positive="1")
caret::confusionMatrix(nb_train_pred,train_balance$ClaimStatus,positive="1")



#~~~~~~~~~~~~~~~~~~~~~Adaboost~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


train_ada<-data_SMOTE[,-c(2,14,26)]
test_ada<-testTransformed[,-c(2,14,26)]

fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final' # To save out of fold predictions for best parameter combinations
  # To save the class probabilities of the out of fold predictions
)

library(adabag)

model_ada = boosting(ClaimStatus~., data=train_ada, boos=TRUE, mfinal=50)

pred_ada_train = predict(model_ada, train_ada)
print(pred_ada_train$confusion)
pred_ada_train
pred = predict(model_ada, test_ada)
print(pred$confusion)

confusionMatrix(pred$confusion,positive="1",mode="everything")
confusionMatrix(pred_ada_train$confusion,positive="1",mode="everything")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Gradient Boosting Machine~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``

train_gbm=data_SMOTE[,-c(2,14,26)]
train_gbm$ClaimStatus<-as.numeric(train_gbm$ClaimStatus)
test_gbm=testTransformed[,-c(2,14,26)]

library(gbm)
modfit=train(ClaimStatus~.,data=train_gbm,method="gbm",verbose=FALSE)
modfit
predict=predict(modfit,train_gbm)
predict_test=predict(modfit,test_gbm)
table(train_gbm$ClaimStatus,predict)
table(test_gbm$ClaimStatus,predict_test)
confmat_gbm_train = table(Actual=train_gbm$ClaimStatus,predict)
confmat_gbm_test = table(Actual=test_gbm$ClaimStatus,predict_test)
confusionMatrix(confmat_gbm_test,positive="1",mode="everything")
confusionMatrix(confmat_gbm_train,positive="1",mode="everything")




#create a task
trainTask <- makeClassifTask(data = data_SMOTE[,-c(2,14,26)],target = "ClaimStatus")
testTask <- makeClassifTask(data = testing1[,-c(2,14,26)], target = "ClaimStatus")
trainTask
testTask
trainTask <- makeClassifTask(data = data_SMOTE,target = "ClaimStatus", positive = "1")
trainTask <- makeClassifTask(data = testing1,target = "ClaimStatus", positive = "1")
#normalize the variables
#trainTask <- normalizeFeatures(data_SMOTE,method = "standardize")
#testTask <- normalizeFeatures(testing1,method = "standardize")
listLearners("classif", check.packages = TRUE, properties = "missings")[c("class","package")]
#make tree learner
makeatree <- makeLearner("classif.rpart", predict.type = "response")

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)
#Search for hyperparameters
 gs <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)
 #do a grid search
 gscontrol <- makeTuneControlGrid() 
 #hypertune the parameters
stune <- tuneParams(learner = makeatree, resampling = set_cv, task = trainTask, par.set = gs, control = gscontrol, measures=acc)
#check best parameter
stune$x                       
#cross validation result
stune$y                    

#using hyperparameters for modeling
t.tree <- setHyperPars(makeatree, par.vals = stune$x)
#train the model
t.rpart <- train(t.tree, trainTask)
getLearnerModel(t.rpart)

#make predictions
tpmodel <- predict(t.rpart, testTask)
submit <- data.frame(Claim = test_RF$ClaimStatus, claim_Status = tpmodel$data$response)
table(submit)



getParamSet("classif.randomForest")

#create a learner
rf <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(
  importance = TRUE
)

#set tunable parameters
#grid search to find hyperparameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
#let's do random search for 50 iterations
rancontrol <- makeTuneControlRandom(maxit = 50L)

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#hypertuning
rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = acc)

#cv accuracy
rf_tune$y

#best parameters
rf_tune$x

#using hyperparameters for modeling
rf.tree <- setHyperPars(rf, par.vals = rf_tune$x)

#train a model
rforest <- train(rf.tree, trainTask)
getLearnerModel(t.rpart)
