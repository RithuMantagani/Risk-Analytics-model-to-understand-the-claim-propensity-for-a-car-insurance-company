### Invoking necessary libraries for transformation
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
library(mice)
library(readxl)
library(lubridate)
library(h2o)
library(caret)
library(readxl)
insurance_data <- read_excel(file.choose(),1)
#Subsetting data to choose number of drivers == 2
insurance_data_2 <- subset(insurance_data,Number_of_Driver == 2)
insurance_data_2 <- data.frame(insurance_data_2)
str(insurance_data_2)
#Variable Transformation
insurance_data_2$Number_of_Driver <- NULL
insurance_data_2$AgeUSdriving_3 <- NULL
insurance_data_2$AgeUSdriving_4 <- NULL
insurance_data_2$AgeUSdriving_5 <- NULL
insurance_data_2$dis
insurance_data_2$ClaimStatus <- as.factor(insurance_data_2$ClaimStatus)
insurance_data_2$ClaimFrequency <- as.factor(insurance_data_2$ClaimFrequency)
insurance_data_2$Billing_Term <- as.factor(insurance_data_2$Billing_Term)
insurance_data_2$Renewed <- as.factor(insurance_data_2$Renewed)
insurance_data_2$Number_of_Driver <- as.factor(insurance_data_2$Number_of_Driver)
insurance_data_2$Amendment <- as.factor(insurance_data_2$Amendment)
insurance_data_2$CoverageLiability <- as.factor(insurance_data_2$CoverageLiability)
insurance_data_2$CoverageMP <- as.factor(insurance_data_2$CoverageMP)
insurance_data_2$CoveragePD_1 <- as.factor(insurance_data_2$CoveragePD_1)
insurance_data_2$CoveragePIP_CDW <- as.factor(insurance_data_2$CoveragePIP_CDW)
insurance_data_2$CoverageUMBI <- as.factor(insurance_data_2$CoverageUMBI)
insurance_data_2$CoverageUMPD <- as.factor(insurance_data_2$CoverageUMPD)
insurance_data_2$DriverAssigned_1 <- as.factor(insurance_data_2$DriverAssigned_1)
insurance_data_2$GaragedZIP_1 <- as.factor(insurance_data_2$GaragedZIP_1)
insurance_data_2$MaritalStatus_1 <- as.factor(insurance_data_2$MaritalStatus_1)
insurance_data_2$MaritalStatus_2 <- as.factor(insurance_data_2$MaritalStatus_2)
insurance_data_2$MaritalStatus_3 <- as.factor(insurance_data_2$MaritalStatus_3)
insurance_data_2$MaritalStatus_4 <- as.factor(insurance_data_2$MaritalStatus_4)
insurance_data_2$MaritalStatus_5 <- as.factor(insurance_data_2$MaritalStatus_5)
insurance_data_2$Occupation_1 <- as.factor(insurance_data_2$Occupation_1)
insurance_data_2$Occupation_2 <- as.factor(insurance_data_2$Occupation_2)
insurance_data_2$Occupation_3 <- as.factor(insurance_data_2$Occupation_3)
insurance_data_2$Occupation_4 <- as.factor(insurance_data_2$Occupation_4)
insurance_data_2$Occupation_5 <- as.factor(insurance_data_2$Occupation_5)
insurance_data_2$Relation_1 <- as.factor(insurance_data_2$Relation_1)
insurance_data_2$Relation_2 <- as.factor(insurance_data_2$Relation_2)
insurance_data_2$Relation_3 <- as.factor(insurance_data_2$Relation_3)
insurance_data_2$Relation_4 <- as.factor(insurance_data_2$Relation_4)
insurance_data_2$Relation_5 <- as.factor(insurance_data_2$Relation_5)
insurance_data_2$Rental_1 <- as.factor(insurance_data_2$Rental_1)
insurance_data_2$Sex_1 <- as.factor(insurance_data_2$Sex_1)
insurance_data_2$Sex_2 <- as.factor(insurance_data_2$Sex_2)
insurance_data_2$Sex_3 <- as.factor(insurance_data_2$Sex_3)
insurance_data_2$Sex_4 <- as.factor(insurance_data_2$Sex_4)
insurance_data_2$Sex_5 <- as.factor(insurance_data_2$Sex_5)
insurance_data_2$Surcharge1Unit_1 <- as.factor(insurance_data_2$Surcharge1Unit_1)
insurance_data_2$Surcharge2Unit_1 <- as.factor(insurance_data_2$Surcharge2Unit_1)
insurance_data_2$Surcharge3Unit_1 <- as.factor(insurance_data_2$Surcharge3Unit_1)
insurance_data_2$Towing_1 <- as.factor(insurance_data_2$Towing_1)
insurance_data_2$Units <- as.factor(insurance_data_2$Units)
insurance_data_2$Make_1 <- as.factor(insurance_data_2$Make_1)
insurance_data_2$Model_1 <- as.factor(insurance_data_2$Model_1)
insurance_data_2$Zip <- as.factor(insurance_data_2$Zip)
insurance_data_2$NoLossSigned <- as.factor(insurance_data_2$NoLossSigned)
insurance_data_2$Type <- as.factor(insurance_data_2$Type)
insurance_data_2$CancellationType <- as.factor(insurance_data_2$CancellationType)
insurance_data_2$ViolPoints1Driver_1 <- as.factor(insurance_data_2$ViolPoints1Driver_1)
insurance_data_2$ViolPoints1Driver_2 <- as.factor(insurance_data_2$ViolPoints1Driver_2)
insurance_data_2$ViolPoints1Driver_3 <- as.factor(insurance_data_2$ViolPoints1Driver_3)
insurance_data_2$ViolPoints1Driver_4 <- as.factor(insurance_data_2$ViolPoints1Driver_4)
insurance_data_2$ViolPoints1Driver_5 <- as.factor(insurance_data_2$ViolPoints1Driver_5)
insurance_data_2$DOB2 <- as.factor(insurance_data_2$DOB2)




insurance_data_2$ViolPoints2Driver_1 <- as.factor(insurance_data_2$ViolPoints2Driver_1)
insurance_data_2$ViolPoints2Driver_2 <- as.factor(insurance_data_2$ViolPoints2Driver_2)
insurance_data_2$ViolPoints2Driver_3 <- as.factor(insurance_data_2$ViolPoints2Driver_3)
insurance_data_2$ViolPoints2Driver_4 <- as.factor(insurance_data_2$ViolPoints2Driver_4)
insurance_data_2$ViolPoints2Driver_5 <- as.factor(insurance_data_2$ViolPoints2Driver_5)





insurance_data_2$ViolPoints3Driver_1 <- as.factor(insurance_data_2$ViolPoints3Driver_1)
insurance_data_2$ViolPoints3Driver_2 <- as.factor(insurance_data_2$ViolPoints3Driver_2)
insurance_data_2$ViolPoints3Driver_3 <- as.factor(insurance_data_2$ViolPoints3Driver_3)
insurance_data_2$ViolPoints3Driver_4 <- as.factor(insurance_data_2$ViolPoints3Driver_4)
insurance_data_2$ViolPoints3Driver_5 <- as.factor(insurance_data_2$ViolPoints3Driver_5)






insurance_data_2$ViolPoints4Driver_1 <- as.factor(insurance_data_2$ViolPoints4Driver_1)
insurance_data_2$ViolPoints4Driver_2 <- as.factor(insurance_data_2$ViolPoints4Driver_2)
insurance_data_2$ViolPoints4Driver_3 <- as.factor(insurance_data_2$ViolPoints4Driver_3)
insurance_data_2$ViolPoints4Driver_4 <- as.factor(insurance_data_2$ViolPoints4Driver_4)
insurance_data_2$ViolPoints4Driver_5 <- as.factor(insurance_data_2$ViolPoints4Driver_5)






insurance_data_2$ViolPoints5Driver_1 <- as.factor(insurance_data_2$ViolPoints5Driver_1)
insurance_data_2$ViolPoints5Driver_2 <- as.factor(insurance_data_2$ViolPoints5Driver_2)
insurance_data_2$ViolPoints5Driver_3 <- as.factor(insurance_data_2$ViolPoints5Driver_3)
insurance_data_2$ViolPoints5Driver_4 <- as.factor(insurance_data_2$ViolPoints5Driver_4)
insurance_data_2$ViolPoints5Driver_5 <- as.factor(insurance_data_2$ViolPoints5Driver_5)







insurance_data_2$ViolPoints6Driver_1 <- as.factor(insurance_data_2$ViolPoints6Driver_1)
insurance_data_2$ViolPoints6Driver_2 <- as.factor(insurance_data_2$ViolPoints6Driver_2)
insurance_data_2$ViolPoints6Driver_3 <- as.factor(insurance_data_2$ViolPoints6Driver_3)
insurance_data_2$ViolPoints6Driver_4 <- as.factor(insurance_data_2$ViolPoints6Driver_4)
insurance_data_2$ViolPoints6Driver_5 <- as.factor(insurance_data_2$ViolPoints6Driver_5)









insurance_data_2$ViolPoints7Driver_1 <- as.factor(insurance_data_2$ViolPoints7Driver_1)
insurance_data_2$ViolPoints7Driver_2 <- as.factor(insurance_data_2$ViolPoints7Driver_2)
insurance_data_2$ViolPoints7Driver_3 <- as.factor(insurance_data_2$ViolPoints7Driver_3)
insurance_data_2$ViolPoints7Driver_4 <- as.factor(insurance_data_2$ViolPoints7Driver_4)
insurance_data_2$ViolPoints7Driver_5 <- as.factor(insurance_data_2$ViolPoints7Driver_5)








insurance_data_2$ViolPoints8Driver_1 <- as.factor(insurance_data_2$ViolPoints8Driver_1)
insurance_data_2$ViolPoints8Driver_2 <- as.factor(insurance_data_2$ViolPoints8Driver_2)
insurance_data_2$ViolPoints8Driver_3 <- as.factor(insurance_data_2$ViolPoints8Driver_3)
insurance_data_2$ViolPoints8Driver_4 <- as.factor(insurance_data_2$ViolPoints8Driver_4)
insurance_data_2$ViolPoints8Driver_5 <- as.factor(insurance_data_2$ViolPoints8Driver_5)









summary(insurance_data_2)




#use of claim frequency in the model is illogical, a prediction model will not have this data
insurance_data_2$ClaimFrequency <- NULL

#behavior  of CoverageUMPD is same as CoverageUMBI variable. hence dropping one.
insurance_data_2$CoverageUMPD<- NULL

#droping coverageMP due to single level in data
table(insurance_data_2$CoverageMP)
insurance_data_2$CoverageMP=NULL


#Distance to work of individual drivers is already transformed into a variable as total distance to work.
insurance_data_2$TotalDistance_Work <- insurance_data_2$DistanceToWork_1+insurance_data_2$DistanceToWork_2 + insurance_data_2$DistanceToWork_3
                                +insurance_data_2$DistanceToWork_4 + insurance_data_2$DistanceToWork_5
  


insurance_data_2$DistanceToWork_1<- NULL
insurance_data_2$DistanceToWork_2<- NULL
insurance_data_2$DistanceToWork_3<- NULL
insurance_data_2$DistanceToWork_4<- NULL
insurance_data_2$DistanceToWork_5<- NULL


# Engine_1 has a lot of data glitches, infeasible to treat and use. moreover it will have too many variables.
insurance_data_2$Engine_1<- NULL


# All zip codes belong to state of texas and have varied cities which will overfit classification model due to too many levels.
insurance_data_2$GaragedZIP_1<- NULL
insurance_data_2$Zip<- NULL



#missing datapoints more than 5 percent
insurance_data_2$MaritalStatus_3<- NULL
insurance_data_2$MaritalStatus_4<- NULL
insurance_data_2$MaritalStatus_5<- NULL

#Model_1/occupations treatment and usage is infeasible since there are too many levels.
insurance_data_2$Model_1<- NULL
insurance_data_2$Occupation_1<- NULL
insurance_data_2$Occupation_2<- NULL
insurance_data_2$Occupation_3<- NULL
insurance_data_2$Occupation_4<- NULL
insurance_data_2$Occupation_5<- NULL

#relations and gender determinants have too many nulls to be imputed and used.
insurance_data_2$Relation_1=NULL
insurance_data_2$Relation_2=NULL
insurance_data_2$Relation_3<- NULL
insurance_data_2$Relation_4<- NULL
insurance_data_2$Relation_5<- NULL
insurance_data_2$Sex_3<- NULL
insurance_data_2$Sex_4<- NULL
insurance_data_2$Sex_5<- NULL

#serial number is a useless information from perdiction point of view.
insurance_data_2$`Sr No`<- NULL

#Surcharge unit variables have one level/levels with same claim rates need to be removed from the study.
table(insurance_data_2$Surcharge1Unit_1)
table(insurance_data_2$Surcharge2Unit_1)
table(insurance_data_2$Surcharge3Unit_1,insurance_data_2$ClaimStatus)
#claim rate when third surcharge componet is not applicable
#claim rate when third surcharge componet is not applicable
insurance_data_2$Surcharge1Unit_1=NULL
insurance_data_2$Surcharge2Unit_1=NULL
insurance_data_2$Surcharge3Unit_1=NULL



### Transformation of variables #############################################################
#adding up voilation points of each policy record.
insurance_data_2$total_voilation_points=insurance_data_2$ViolPoints1Driver_1+insurance_data_2$ViolPoints1Driver_2+insurance_data_2$ViolPoints1Driver_3+insurance_data_2$ViolPoints1Driver_4+insurance_data_2$ViolPoints1Driver_5+
  insurance_data_2$ViolPoints2Driver_1+insurance_data_2$ViolPoints2Driver_2+insurance_data_2$ViolPoints2Driver_3+insurance_data_2$ViolPoints2Driver_4+insurance_data_2$ViolPoints2Driver_5+
  insurance_data_2$ViolPoints3Driver_1+insurance_data_2$ViolPoints3Driver_2+insurance_data_2$ViolPoints3Driver_3+insurance_data_2$ViolPoints3Driver_4+insurance_data_2$ViolPoints3Driver_5+
  insurance_data_2$ViolPoints4Driver_1+insurance_data_2$ViolPoints4Driver_2+insurance_data_2$ViolPoints4Driver_3+insurance_data_2$ViolPoints4Driver_4+insurance_data_2$ViolPoints4Driver_5+
  insurance_data_2$ViolPoints5Driver_1+insurance_data_2$ViolPoints5Driver_2+insurance_data_2$ViolPoints5Driver_3+insurance_data_2$ViolPoints5Driver_4+insurance_data_2$ViolPoints5Driver_5+
  insurance_data_2$ViolPoints6Driver_1+insurance_data_2$ViolPoints6Driver_2+insurance_data_2$ViolPoints6Driver_3+insurance_data_2$ViolPoints6Driver_4+insurance_data_2$ViolPoints6Driver_5+
  insurance_data_2$ViolPoints7Driver_1+insurance_data_2$ViolPoints7Driver_2+insurance_data_2$ViolPoints7Driver_3+insurance_data_2$ViolPoints7Driver_4+insurance_data_2$ViolPoints7Driver_5+
  insurance_data_2$ViolPoints8Driver_1+insurance_data_2$ViolPoints8Driver_2+insurance_data_2$ViolPoints8Driver_3+insurance_data_2$ViolPoints8Driver_4+insurance_data_2$ViolPoints8Driver_5

#Dropping individual components of voilation points in the data set, the summary has been captured in total_voilation_points attribute
insurance_data_2$ViolPoints1Driver_1<- NULL
insurance_data_2$ViolPoints2Driver_1<- NULL
insurance_data_2$ViolPoints3Driver_1<- NULL
insurance_data_2$ViolPoints4Driver_1<- NULL
insurance_data_2$ViolPoints5Driver_1<- NULL
insurance_data_2$ViolPoints6Driver_1<- NULL
insurance_data_2$ViolPoints7Driver_1<- NULL
insurance_data_2$ViolPoints8Driver_1<- NULL
insurance_data_2$ViolPoints1Driver_2<- NULL
insurance_data_2$ViolPoints2Driver_2<- NULL
insurance_data_2$ViolPoints3Driver_2<- NULL
insurance_data_2$ViolPoints4Driver_2<- NULL
insurance_data_2$ViolPoints5Driver_2<- NULL
insurance_data_2$ViolPoints6Driver_2<- NULL
insurance_data_2$ViolPoints7Driver_2<- NULL
insurance_data_2$ViolPoints8Driver_2<- NULL
insurance_data_2$ViolPoints1Driver_3<- NULL
insurance_data_2$ViolPoints2Driver_3<- NULL
insurance_data_2$ViolPoints3Driver_3<- NULL
insurance_data_2$ViolPoints4Driver_3<- NULL
insurance_data_2$ViolPoints5Driver_3<- NULL
insurance_data_2$ViolPoints6Driver_3<- NULL
insurance_data_2$ViolPoints7Driver_3<- NULL
insurance_data_2$ViolPoints8Driver_3<- NULL
insurance_data_2$ViolPoints1Driver_4<- NULL
insurance_data_2$ViolPoints2Driver_4<- NULL
insurance_data_2$ViolPoints3Driver_4<- NULL
insurance_data_2$ViolPoints4Driver_4<- NULL
insurance_data_2$ViolPoints5Driver_4<- NULL
insurance_data_2$ViolPoints6Driver_4<- NULL
insurance_data_2$ViolPoints7Driver_4<- NULL
insurance_data_2$ViolPoints8Driver_4<- NULL
insurance_data_2$ViolPoints1Driver_5<- NULL
insurance_data_2$ViolPoints2Driver_5<- NULL
insurance_data_2$ViolPoints3Driver_5<- NULL
insurance_data_2$ViolPoints4Driver_5<- NULL
insurance_data_2$ViolPoints5Driver_5<- NULL
insurance_data_2$ViolPoints6Driver_5<- NULL
insurance_data_2$ViolPoints7Driver_5<- NULL
insurance_data_2$ViolPoints8Driver_5<- NULL

str(insurance_data_2)
View(insurance_data_2)
summary(insurance_data_2)

#comes out to be NA for all rows
insurance_data_2$total_voilation_points <- NULL
#derriving a varible with a count of total declared excluded drivers in policies
insurance_data_2$totalExcludedDrivers=(ifelse(is.na(insurance_data_2$ExcludedDriverName_01), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_02), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_03), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_04), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_05), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_06), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_07), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_08), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_09), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_10), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_11), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_12), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_14), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_15), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_16), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_17), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_18), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_19), 0, 1)+
                                  ifelse(is.na(insurance_data_2$ExcludedDriverName_20), 0, 1))

# dropping individual Excluded driver name components, quantitative taken in totalExcludedDrivers variable
insurance_data_2$ExcludedDriverName_01=NULL
insurance_data_2$ExcludedDriverName_02=NULL
insurance_data_2$ExcludedDriverName_03=NULL
insurance_data_2$ExcludedDriverName_04=NULL
insurance_data_2$ExcludedDriverName_05=NULL
insurance_data_2$ExcludedDriverName_06=NULL
insurance_data_2$ExcludedDriverName_07=NULL
insurance_data_2$ExcludedDriverName_08=NULL
insurance_data_2$ExcludedDriverName_09=NULL
insurance_data_2$ExcludedDriverName_10=NULL
insurance_data_2$ExcludedDriverName_11=NULL
insurance_data_2$ExcludedDriverName_12=NULL
insurance_data_2$ExcludedDriverName_13=NULL
insurance_data_2$ExcludedDriverName_14=NULL
insurance_data_2$ExcludedDriverName_15=NULL
insurance_data_2$ExcludedDriverName_16=NULL
insurance_data_2$ExcludedDriverName_17=NULL
insurance_data_2$ExcludedDriverName_18=NULL
insurance_data_2$ExcludedDriverName_19=NULL
insurance_data_2$ExcludedDriverName_20=NULL



#text mining Make variable
insurance_data_2$Make_1=toupper(insurance_data_2$Make_1)
level_key <- c("ACURA"="ACURA","AMG"="AMG","AUDI"="AUDI","BMW"="BMW","BUIC"="BUICK","BUICK"="BUICK","CADDY"="CADILLAC","CADILLAC"="CADILLAC","CHEV"="CHEVROLET","CHEVERLET"="CHEVROLET","CHEVEROLET"="CHEVROLET","CHEVORLET"="CHEVROLET","CHEVROET"="CHEVROLET","CHEVROLET"="CHEVROLET","CHEVROLET`"="CHEVROLET","CHEVROLETQ"="CHEVROLET","CHEVY"="CHEVROLET","CHEVYVAN"="CHEVROLET","CHRYLER"="CHRYSLER","CHRYSLER"="CHRYSLER","CHRYSLR"="CHRYSLER","CUTLASS"="OLDSMOBILE","DAEWOO"="DAEWOO","DAEWOOD"="DAEWOO","DATS"="DATSUN","DODGE"="DODGE","EAGLE"="EAGLE","FORD"="FORD","FORED"="FORD","FORK"="FORD","G.M.C"="GMC","GENERALMO"="GMC","GEO"="GEO","GMA"="GMC","GMC"="GMC","HONDA"="HONDA","HUMDAI"="HYUNDAI","HUMMER"="HUMMER","HUNDAI"="HYUNDAI","HYANDI"="HYUNDAI","HYNDAI"="HYUNDAI","HYUDAI"="HYUNDAI","HYUNDAI"="HYUNDAI","HYUNDIA"="HYUNDAI","HYUNDY"="HYUNDAI","INFINIT"="INFINITI","INFINITI"="INFINITI","ISUZI"="ISUZU","ISUZU"="ISUZU","IZUSU"="ISUZU","JAGUAR"="JAGUAR","JEEP"="JEEP","JEEPAMGC"="JEEP","KIA"="KIA","KIAMOTORS"="KIA","LANDROVER"="LANDROVER","LEXUS"="LEXUS","LINC"="LINCOLN","LINCOLN"="LINCOLN","LINCON"="LINCOLN","MAZDA"="MAZDA","MERC"="MERCEDES","MERCEDE"="MERCEDES","MERCEDES"="MERCEDES","MERCEDESB"="MERCEDES","MERCEDES-B"="MERCEDES","MERCURY"="MERCURY","MINI"="MINI","MITS"="MITSUBISHI","MITSUBI"="MITSUBISHI","MITSUBISHI"="MITSUBISHI","NISS"="NISSAN","NISSAN"="NISSAN","NON-OWNERS"="NO-OWNER","NO-OWNERS"="NO-OWNER","OLDS"="OLDSMOBILE","OLDSMOBILE"="OLDSMOBILE","PICKUP"="DODGE","PLYM"="PLYMOUTH","PLYMOUT"="PLYMOUTH","PLYMOUTH"="PLYMOUTH","PLYMT"="PLYMOUTH","PONT"="PONTIAC","PONTIAC"="PONTIAC","PONTIC"="PONTIAC","PORSCHE"="PORSCHE","SAAB"="SAAB","SATURN"="SATURN","SCION"="SCION","SUBARU"="SUBARU","SUZUKI"="SUZUKI","TOYOTA"="TOYOTA","VAN"="FORD","VOLK"="VOLKSWAGEN","VOLKS"="VOLKSWAGEN","VOLKSWAGEN"="VOLKSWAGEN","VOLVO"="VOLKSWAGEN","VW"="VOLKSWAGEN","WRANGLER"="WRANGLER")
insurance_data_2$Make_1=recode(insurance_data_2$Make_1, !!!level_key)
insurance_data_2$Make_1=as.factor(insurance_data_2$Make_1)
table(insurance_data_2$Make_1,insurance_data_2$ClaimStatus)

#too many levels present for the data volume. to be dropped.
insurance_data_2$Make_1=NULL



#cancellation type cannot be known beforehand, this has a direct coreation with claim status. hence being dropped from study.
table(insurance_data_2$CancellationType)
insurance_data_2$CancellationType=NULL

#checking data enrichment per column
(as.data.frame(sapply(insurance_data_2, function(x) sum(is.na(x))*100/184)))

#binning levels of billing term variable. if billing term is more than 1 then we bin them in '2' category.
table(insurance_data_2$Billing_Term,insurance_data_2$ClaimStatus)
insurance_data_2$Billing_Term=ifelse(insurance_data_2$Billing_Term==3 | insurance_data_2$Billing_Term==6 , 2, 1)
table(insurance_data_2$Billing_Term,insurance_data_2$ClaimStatus)

#binning Ammendments variable. If there are any ammendments, then we will bin them in '1' category
table(insurance_data_2$Amendment,insurance_data_2$ClaimStatus)
insurance_data_2$Amendment=ifelse(insurance_data_2$Amendment==0 , 0, 1)

#binning coverage liablity variable. If there are any coverage other than 20/40/15(mapped to '0'), then we will bin them in '1' category
table(insurance_data_2$CoverageLiability,insurance_data_2$ClaimStatus)
insurance_data_2$CoverageLiability=ifelse(insurance_data_2$CoverageLiability=='20/40/15' , 0, 1)
table(insurance_data_2$CoverageLiability,insurance_data_2$ClaimStatus)

#binning driver_assigned_1 variable. binning driver assignment in three buckect considering data balance, claim rates and logical basis.
#'0' mapped to 1 or 2 driver assignment of first unit
#'1' mapped to 3 assignment of first unit
#'2' mapped to 4 or 5 driver assignment of first unit
table(insurance_data_2$DriverAssigned_1,insurance_data_2$ClaimStatus)
insurance_data_2$DriverAssigned_1 = ifelse(insurance_data_2$DriverAssigned_1>1,1,0)



#dropping CoveragePIP_CDW due to data imbalance being created.
table(insurance_data_2$CoveragePIP_CDW,insurance_data_2$ClaimStatus)
insurance_data_2$CoveragePIP_CDW=NULL

#dropping Coverage_UMBI due to data imbalance being created.
table(insurance_data_2$CoverageUMBI,insurance_data_2$ClaimStatus)
insurance_data_2$CoverageUMBI=NULL

#dropping rental_1 due to lack of data points in level '1' and similar claim rates
table(insurance_data_2$Rental_1,insurance_data_2$ClaimStatus)
insurance_data_2$CoverageUMBI=NULL

#dropping sex_1 attribute due to similar claim rates.
table(insurance_data_2$Sex_1,insurance_data_2$ClaimStatus)
insurance_data_2$Sex_1=NULL

#making sex_2 varibale open to other/unspecified genders. Observing claim rates
sum(table(insurance_data_2$Sex_2))
sum(is.na(insurance_data_2$Sex_2))
insurance_data_2$Sex_2=ifelse(is.na(insurance_data_2$Sex_2), 'U',
                       ifelse(insurance_data_2$Sex_2=='M','M','F'))
table(insurance_data_2$Sex_2,insurance_data_2$ClaimStatus)

insurance_data_2$Sex_2=as.factor(insurance_data_2$Sex_2)

#dropping towing_1 varibale since claim rates across levels is almost same and there is data imbalance in the levels
table(insurance_data_2$Towing_1,insurance_data_2$ClaimStatus)
insurance_data_2$Towing_1=NULL

#binning  units variable to one or more than one levels mapped to 1 and 2 respectively.
table(insurance_data_2$Units,insurance_data_2$ClaimStatus)
insurance_data_2$Units=as.integer(insurance_data_2$Units)
insurance_data_2$Units = ifelse(insurance_data_2$Units==0,NULL)
insurance_data_2$Units=ifelse(insurance_data_2$Units==1,1,2) 
table(insurance_data_2$Units,insurance_data_2$ClaimStatus)
insurance_data_2$Units=as.factor(insurance_data_2$Units)

#binning year_1 variable to 2000 and below or more than 2000 year mapped to 0 and 1 respectively.
table(insurance_data_2$Year_1,insurance_data_2$ClaimStatus)
insurance_data_2$Year_1=as.numeric(as.character(insurance_data_2$Year_1))
insurance_data_2$Year_1=ifelse(insurance_data_2$Year_1<=2000,0,1)
table(insurance_data_2$Year_1,insurance_data_2$ClaimStatus)

#dropping type variable, due to definition insufficiency and thus inability to bin and explanablity/relaiblity issues
table(insurance_data_2$Type,insurance_data_2$ClaimStatus)
insurance_data_2$Type=NULL
summary(insurance_data_2)

#binning total excluded drivers derrived varibale based on claim rates.
table(insurance_data_2$totalExcludedDrivers,insurance_data_2$ClaimStatus)
insurance_data_2$totalExcludedDrivers=as.numeric(insurance_data_2$totalExcludedDrivers)
insurance_data_2$totalExcludedDrivers=(ifelse(insurance_data_2$totalExcludedDrivers>3,1,0))
table(insurance_data_2$totalExcludedDrivers,insurance_data_2$ClaimStatus)

#dropping no loss signed variable since claim rates across levels are same and due to data imbalance
table(insurance_data_2$NoLossSigned,insurance_data_2$ClaimStatus)
insurance_data_2$NoLossSigned=NULL

#Applying log transformation to total distance to work variable.
table(insurance_data_2$Total_Distance_To_Work)
histogram(insurance_data_2$Total_Distance_To_Work)
table(log10(insurance_data_2$Total_Distance_To_Work+1),insurance_data_2$ClaimStatus)
boxplot(log10(insurance_data_2$Total_Distance_To_Work+1))
histogram(log10(insurance_data_2$Total_Distance_To_Work+1))

#binning marital_status_2 variable. recoding NULL as U since there are other acceptable marital status as well.
table(insurance_data_2$MaritalStatus_2)
sum(table(insurance_data_2$MaritalStatus_2))
sum(is.na(insurance_data_2$MaritalStatus_2))
insurance_data_2$MaritalStatus_2=ifelse(is.na(insurance_data_2$MaritalStatus_2),NULL,ifelse(insurance_data_2$MaritalStatus_2=='S','S','M'))
table(insurance_data_2$MaritalStatus_2,insurance_data_2$ClaimStatus)

# Transformation for age parameter
reference_year=insurance_data_2$DOB1 %m+% years(insurance_data_2$AgeUSdriving_1)


insurance_data_2$DOB4
x <- as.Date("1900-01-01")
y=x %m+% days(insurance_data_2$DOB4)
insurance_data_2$DOB4=as.POSIXct.Date(y)

insurance_data_2$DOB5
x <- as.Date("1900-01-01")
y=x %m+% days(insurance_data_2$DOB5)
y
insurance_data_2$DOB5=as.POSIXct.Date(y)



ref2=as.integer(round((reference_year - insurance_data_2$DOB2)/365.25,0))
ref3=as.integer(round((reference_year - insurance_data_2$DOB3)/365.25,0))
ref4=as.integer(round((reference_year - insurance_data_2$DOB4)/365.25,0))
ref5=as.integer(round((reference_year - insurance_data_2$DOB5)/365.25,0))
insurance_data_2$AgeUSdriving_2
insurance_data_2$AgeUSdriving_2=ifelse(insurance_data_2$AgeUSdriving_2==0,ref2,insurance_data_2$AgeUSdriving_2)


insurance_data_2$AgeUSdriving_2 <- log10(insurance_data_2$AgeUSdriving_2+1)
insurance_data_2$AgeUSdriving_1 <- log10(insurance_data_2$AgeUSdriving_1+1)





insurance_data_2$DOB1=NULL
insurance_data_2$DOB2=NULL
insurance_data_2$DOB3=NULL
insurance_data_2$DOB4=NULL
insurance_data_2$DOB5=NULL

as.data.frame(sapply(insurance_data_2, function(x) sum(is.na(x))*100/184))


rm(x)
rm(y)
rm(level_key)
rm(ref2)
rm(ref3)
rm(ref4)
rm(ref5)
rm(reference_year)

#to factor conversion
insurance_data_2$Billing_Term=as.factor(insurance_data_2$Billing_Term)
insurance_data_2$Amendment=as.factor(insurance_data_2$Amendment)
insurance_data_2$CoverageLiability=as.factor(insurance_data_2$CoverageLiability)
insurance_data_2$DriverAssigned_1=as.factor(insurance_data_2$DriverAssigned_1)
insurance_data_2$MaritalStatus_2=as.factor(insurance_data_2$MaritalStatus_2)
insurance_data_2$Year_1=as.factor(insurance_data_2$Year_1)
insurance_data_2$totalExcludedDrivers=as.factor(insurance_data_2$totalExcludedDrivers)











summary(insurance_data_2)


---------------------------------------------------------------------------------------------#
#Outlier Treatment
  



#treating outliers for year 1 variable

insurance_data_2$Year_1[insurance_data_2$Year_1==988]<-1998##logically
insurance_data_2$Year_1[insurance_data_2$Year_1==199]<-NA
insurance_data_2$Year_1[insurance_data_2$Year_1==-3]<-NA
insurance_data_2$Year_1[insurance_data_2$Year_1==0]<-NA
insurance_data_2$Year_1<-impute(insurance_data_2$Year_1,fun = median)
insurance_data_2$Year_1<-as.factor(insurance_data_2$Year_1)
summary(insurance_data_2$Year_1)

#treating Rental_1 variable for outliers, 0.25% of data indentified as outliers
table(insurance_data_2$Rental_1)
insurance_data_2$Rental_1[insurance_data_2$Rental_1==20]<-0
insurance_data_2$Rental_1[insurance_data_2$Rental_1==25]<-0
insurance_data_2$Rental_1[insurance_data_2$Rental_1==35]<-0
insurance_data_2$Rental_1<-as.factor(insurance_data_2$Rental_1)


#Recoding the Make_1 and occupation variables
library(dplyr)
level_key <- c("ACURA"="ACURA","AMG"="AMG","AUDI"="AUDI","BMW"="BMW","BUIC"="BUICK","BUICK"="BUICK","CADDY"="CADILLAC","CADILLAC"="CADILLAC","CHEV"="CHEVROLET","CHEVERLET"="CHEVROLET","CHEVEROLET"="CHEVROLET","CHEVORLET"="CHEVROLET","CHEVROET"="CHEVROLET","CHEVROLET"="CHEVROLET","CHEVROLET`"="CHEVROLET","CHEVROLETQ"="CHEVROLET","CHEVY"="CHEVROLET","CHEVYVAN"="CHEVROLET","CHRYLER"="CHRYSLER","CHRYSLER"="CHRYSLER","CHRYSLR"="CHRYSLER","CUTLASS"="OLDSMOBILE","DAEWOO"="DAEWOO","DAEWOOD"="DAEWOO","DATS"="DATSUN","DODGE"="DODGE","EAGLE"="EAGLE","FORD"="FORD","FORED"="FORD","FORK"="FORD","G.M.C"="GMC","GENERALMO"="GMC","GEO"="GEO","GMA"="GMC","GMC"="GMC","HONDA"="HONDA","HUMDAI"="HYUNDAI","HUMMER"="HUMMER","HUNDAI"="HYUNDAI","HYANDI"="HYUNDAI","HYNDAI"="HYUNDAI","HYUDAI"="HYUNDAI","HYUNDAI"="HYUNDAI","HYUNDIA"="HYUNDAI","HYUNDY"="HYUNDAI","INFINIT"="INFINITI","INFINITI"="INFINITI","ISUZI"="ISUZU","ISUZU"="ISUZU","IZUSU"="ISUZU","JAGUAR"="JAGUAR","JEEP"="JEEP","JEEPAMGC"="JEEP","KIA"="KIA","KIAMOTORS"="KIA","LANDROVER"="LANDROVER","LEXUS"="LEXUS","LINC"="LINCOLN","LINCOLN"="LINCOLN","LINCON"="LINCOLN","MAZDA"="MAZDA","MERC"="MERCEDES","MERCEDE"="MERCEDES","MERCEDES"="MERCEDES","MERCEDESB"="MERCEDES","MERCEDES-B"="MERCEDES","MERCURY"="MERCURY","MINI"="MINI","MITS"="MITSUBISHI","MITSUBI"="MITSUBISHI","MITSUBISHI"="MITSUBISHI","NISS"="NISSAN","NISSAN"="NISSAN","NON-OWNERS"="NO-OWNER","NO-OWNERS"="NO-OWNER","OLDS"="OLDSMOBILE","OLDSMOBILE"="OLDSMOBILE","PICKUP"="DODGE","PLYM"="PLYMOUTH","PLYMOUT"="PLYMOUTH","PLYMOUTH"="PLYMOUTH","PLYMT"="PLYMOUTH","PONT"="PONTIAC","PONTIAC"="PONTIAC","PONTIC"="PONTIAC","PORSCHE"="PORSCHE","SAAB"="SAAB","SATURN"="SATURN","SCION"="SCION","SUBARU"="SUBARU","SUZUKI"="SUZUKI","TOYOTA"="TOYOTA","VAN"="FORD","VOLK"="VOLKSWAGEN","VOLKS"="VOLKSWAGEN","VOLKSWAGEN"="VOLKSWAGEN","VOLVO"="VOLKSWAGEN","VW"="VOLKSWAGEN","WRANGLER"="WRANGLER")
insurance_data_2$Make_1=recode(insurance_data_2$Make_1, !!!level_key)
insurance_data_2$Make_1=as.factor(insurance_data_2$Make_1)
#converting to factor varibable
insurance_data_2$NoLossSigned<-as.factor(insurance_data_2$NoLossSigned)

#--------------------Check for blank values values---------------------------#

insurance_data_2$CoverageMP[insurance_data_2$CoverageMP==""]<-NA
summary(insurance_data_2$CoverageMP)
insurance_data_2$CoveragePD_1[insurance_data_2$CoveragePD_1==""]<-NA
summary(insurance_data_2$CoveragePD_1)
insurance_data_2$CoveragePIP_CDW[insurance_data_2$CoveragePIP_CDW==""]<-NA
summary(insurance_data_2$CoveragePIP_CDW)
insurance_data_2$CoverageUMBI[insurance_data_2$CoverageUMBI==""]<-NA
summary(insurance_data_2$CoverageUMBI)
insurance_data_2$CoverageUMPD[insurance_data_2$CoverageUMPD==""]<-NA
summary(insurance_data_2$CoverageUMPD)

insurance_data_2$Surcharge1Unit_1[insurance_data_2$Surcharge1Unit_1==""]<-NA
insurance_data_2$Surcharge2Unit_1[insurance_data_2$Surcharge2Unit_1==""]<-NA
insurance_data_2$Surcharge3Unit_1[insurance_data_2$Surcharge3Unit_1==""]<-NA

#outlier treatment for Premium
insurance_data_2$Premium<-(log10(insurance_data_2$Premium+1))

#Treating missing value thorugh mice
library(mice)
#imputation of missing values
summary(insurance_data_2)
md.pattern(insurance_data_2)
md.pairs(insurance_data_2)
impute=mice(insurance_data_2,m=3,seed=123)
insurance_data_8 <- impute
summary(insurance_data_8)
impute$imp$AgeUSdriving_3
print(impute)
class(complete(impute,2))
a=complete(impute,2)
summary(a)
insurance_data_10 <- a
summary(insurance_data_10)




#Analysis of Numerical Variables


library(dlookr)
str(insurance_data_10)
dataPremium <- data.frame(insurance_data_10$Premium)
dataAgeUSdriving_1 <- data.frame(insurance_data_10$AgeUSdriving_1)
dataAgeUSdriving_2 <- data.frame(insurance_data_10$AgeUSdriving_2)
dataAgeUSdriving_3 <- data.frame(insurance_data_10$AgeUSdriving_3)
dataDistanceToWork_1 <-  data.frame(insurance_data_10$DistanceToWork_1)
dataDistanceToWork_2 <-  data.frame(insurance_data_10$DistanceToWork_2)
dataDistanceToWork_3 <- data.frame(insurance_data_10$DistanceToWork_3)
dataVehicleInspected_1 <- data.frame(insurance_data_10$VehicleInspected_1)
dataYear_1      <-        data.frame(insurance_data_10$Year_1)
dataTotal_Distance_To_Work   <- data.frame(insurance_data_10$Total_Distance_To_Work)







plot_normality(dataPremium)
plot_normality(dataAgeUSdriving_1)
plot_normality(dataAgeUSdriving_2)
plot_normality(dataAgeUSdriving_3)
plot_normality(dataDistanceToWork_1)
plot_normality(dataDistanceToWork_2)
plot_normality(dataDistanceToWork_3)
plot_normality(dataVehicleInspected_1)
plot_normality(dataYear_1)
plot_normality(dataTotal_Distance_To_Work)






-------------------------------------------------------------------------------------------
#Data Visualization
table(insurance_data_10$Premium)
plot(insurance_data_10$Premium)
boxplot(insurance_data_10$Premium)
plot(insurance_data_10$ClaimStatus)
plot(insurance_data_10$ClaimStatus,insurance_data_2$Premium)
plot(insurance_data_10$ClaimStatus,insurance_data_2$Billing_Term)
hist(insurance_data_10$Premium)

library(ggplot2)


ggplot(data = insurance_data_10, mapping = aes ( x = Premium, y = ClaimStatus))+ geom_point()
ggplot(data = insurance_data_10, mapping = aes ( x = Premium, y = ClaimStatus))+ geom_boxplot()

str(insurance_data_10)
summary(insurance_data_10)
insurance_data_10$Rental_1 <- factor(c("0","1"))
#train-test split
trainIndex <- createDataPartition(insurance_data_10$ClaimStatus,p=0.75,list=FALSE)
data_train <-insurance_data_10[trainIndex,]
data_test <- insurance_data_10[-trainIndex,]


table(data_train$ClaimStatus)
prop.table(table(data_train$ClaimStatus))
table(data_test$ClaimStatus)
prop.table(table(data_test$ClaimStatus))


summary(insurance_data_10)
#SMOTE

library(DMwR)
data_train_balanced <- SMOTE(ClaimStatus ~ ., data_train[,-1], perc.over = 100, perc.under=300,k=5)

table(data_train_balanced$ClaimStatus)

print(prop.table(table(data_train_balanced$ClaimStatus)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`CART using SMOTE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## setting the  dataset and  control paramter inputs for rpart 
set.seed=135
library(rpart)
library(rpart.plot)
r.ctrl = rpart.control(minsplit=7, minbucket = 5, cp = 0, xval = 10)
m1 <- rpart(formula = ClaimStatus ~ ., data =data_train_balanced, method = "class", control = r.ctrl)
m1


#Plot the tree
rpart.plot(m1)
plotcp(m1)



## to find how the tree performs and derive cp value
printcp(m1)

ptree<- prune(m1, cp= 0.005 ,"CP")
printcp(ptree)
rpart.plot(ptree)

#train confusion matrix
library(caret)
library(e1071)
predicted1 <- predict(ptree,data_train_balanced,type = "class")
confusionMatrix(predicted1,data_train_balanced$ClaimStatus,positive="1",mode="everything")

#test confusion matrix
data_test$predict.class <- predict(ptree, data_test, type="class")

predicted2 <- predict(ptree,data_test,type = "class")
confusionMatrix(predicted2,data_test$ClaimStatus,positive="1",mode="everything")

#checking imp variables
library(vip)
vip(ptree,num_features=40,bar=FALSE)






#Multinom Logistic

#~~~~~~~~~~~~~~~~~`Logistic reg~~~~~~~~~~~~~~~~~~~~~~
library(nnet)
#Equation
equation<-ClaimStatus~ Premium+Renewed+Billing_Term+DriverAssigned_1+Units+VehicleInspected_1+MaritalStatus_1+MaritalStatus_2+Year_1+Rental_1+Sex_2+AgeUSdriving_2+Amendment+CoverageLiability+Year_1+TotalDistance_Work+totalExcludedDrivers

#multinom model
multinom1 <- multinom(equation, data = data_train_balanced)
summary(multinom1)

#logit1 <- glm (equation, data=train_log, family = binomial)
#summary(logit1)


#1 Identify overall fitness of model using log likehood Ratio Test

#install.packages("lmtest")
library(lmtest)

lrtest(multinom1)

#2 Calculating McFadden's Rsquared (Minimum McFadden Rsquared considered for model fitness is 10%)
#install.packages("pscl")
library(pscl)
pR2(multinom1)




predicted1=predict(multinom1,data_train_balanced,type="class")
predicted2=predict(multinom1,data_test,type="class")


# Evaluate Model performance
library(e1071)
library(caret)
confusionMatrix(predicted1,
                data_train_balanced$ClaimStatus,
                mode="everything",positive = "1")
confusionMatrix(predicted2,
                data_test$ClaimStatus,
                mode="everything",positive = "1")
  
#Random Forest
  
library(randomForest)



train_RF=data_train_balanced[,-c(2,14,26,8,12,19)]
test_RF=data_test
train_RF$Year_1 <- NULL
str(data_test)
## Calling syntax to build the Random Forest
RF <- randomForest(as.factor(ClaimStatus) ~ ., data = train_RF ,
                   ntree=1001, mtry = 3, nodesize =10,
                   importance=TRUE,na.action = na.roughfix)
print(RF)
plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest train_RF")

RF$err.rate
impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,3], decreasing=TRUE),]
## Tuning Random Forest
set.seed(123)
tRF <- tuneRF(x = train_RF[,-c(1)],
              y=as.factor(train_RF$ClaimStatus),
              mtryStart = 4, 
              ntreeTry=401, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 10, 
              importance=TRUE,
              na.action = na.roughfix
)

#A plot for the mean decrease in accuracy levels
varImpPlot(tRF)
## Predicting the Probabilities and the prediction class for the development data
train_RF$predict.class <- predict(tRF, train_RF, type="class")
train_RF$predict.score <- predict(tRF, train_RF, type="prob")
head(train_RF)
class(train_RF$predict.score)
library(MLmetrics)

MLmetrics::AUC(train_RF$predict.score[,2], train_RF$ClaimStatus)

## Predicting the Probabilities and the prediction class for the Holdout data. 
##Would like to see how well the model is able to predict for the holdout data
test_RF$predict.class <- predict(tRF, test_RF, type="class")
test_RF$predict.score <- predict(tRF, test_RF, type="prob")


MLmetrics::AUC(test_RF$predict.score[,2], 
               test_RF$ClaimStatus)



predicted1=predict(RF,train_RF,type="class")
predicted2=predict(RF,test_RF,type="class")

# Evaluate Model performance
library(e1071)
library(caret)
confusionMatrix(predicted1,
                train_RF$ClaimStatus,
                mode="everything",positive = "1")

confusionMatrix(predicted2,
                test_RF$ClaimStatus,
                mode="everything",positive = "1")



library(ROCR)
test.roc <- prediction(test_RF$predict.score[,2], test_RF$ClaimStatus)
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








































#Lasso
str(data_train_balanced)
summary(data_train_balanced)
str(data_test)
summary(data_test)
data_train_balanced$Year_1 <- NULL

data_test$Number_of_Driver <- as.numeric(data_test$Number_of_Driver)
data_train_balanced$Number_of_Driver <- as.numeric(data_train_balanced$Number_of_Driver)
data_test$Sr.No <- NULL

data_train_balanced$Year_1 <- NULL
data_test$Year_1 <- NULL
data_test$ClaimStatus <- as.factor(data_test$ClaimStatus)
data_train_balanced$ClaimStatus <- as.factor(data_train_balanced$ClaimStatus)
str(data_train_balanced)
train_dummy_coded <- model.matrix(ClaimStatus ~., data=data_train_balanced)
test_dummy_coded <- model.matrix(ClaimStatus ~., data=data_test)
library(glmnet)
library(dplyr)
# Setting the seed
set.seed(123)
# Alpha = 1 (Lasso), Alpha=0 (Ridge)
lasso_model <- glmnet(train_dummy_coded,data_train_balanced$ClaimStatus, family="binomial", nlambda=200, alpha = 1)

# Shows for particular value of lambda
# how many features you have
print(lasso_model)
select_coeff <- as.matrix(coef(lasso_model, s=0.01))
select_coeff_df <- data.frame(Feature=rownames(select_coeff), Coef=select_coeff[,1])
select_coeff_df
select_coeff_df %>% filter(Coef != 0)
# Prediction
lambda_select = 0.01
# type=class will predict class
# type=response will predict probability
lasso_pred_train <- predict(lasso_model, newx =train_dummy_coded , type="class", s=lambda_select)[,1]
lasso_pred_test <- predict(lasso_model, newx = test_dummy_coded, type="class", s=lambda_select)[,1]
table(lasso_pred_train)
pred_train<- data.frame(Actual=data_train_balanced$ClaimStatus, Pred=lasso_pred_train)
pred_test<-data.frame(Actual=data_test$ClaimStatus,Pred=lasso_pred_test)
table(lasso_pred_test)
# Compute Accuracy
library(e1071)
library(caret)

#train data

confusionMatrix(pred_train$Pred,
                pred_train$Actual)


#test data

confusionMatrix(pred_test$Pred,
                pred_test$Actual)















##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Adaboost~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
library(h2o)
localH2O <- h2o.init(ip = 'localhost', port = 54321, nthreads= -1)

library(caret)
fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final' # To save out of fold predictions for best parameter combinations
  # To save the class probabilities of the out of fold predictions
)
library(adabag)
#model_ada<-train(train_ada[,-1],train_ada[,1],method='adaboost',tuneLength=3)
model_ada = boosting(ClaimStatus~., data=data_train_balanced, boos=TRUE, mfinal=50)

pred_ada_train = predict(model_ada, data_train_balanced)
print(pred_ada_train$confusion)

pred = predict(model_ada, data_test)
print(pred$confusion)

confusionMatrix(pred_ada_train$confusion,positive="1",mode="everything")
confusionMatrix(pred$confusion,positive="1",mode="everything")


#ensemble method

#Defining the training control
fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final',
  classProbs = T 
)

#Defining the training control
fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final', # To save out of fold predictions for best parameter combinantions
  classProbs = T # To save the class probabilities of the out of fold predictions
)
data_train_balanced$ClaimStatus <- make.names(data_train_balanced$ClaimStatus)
#Training the random forest model
model_rf<-train(data_train_balanced[,-1],data_train_balanced[,1],method='rf',trControl=fitControl,tuneLength=3)

#Training the logistic regression model
model_lr<-train(data_train_balanced[,-1],data_train_balanced[,1],method='multinom',trControl=fitControl,tuneLength=3)

library(fastAdaboost)
model_ada<-train(data_train_balanced[,-1],data_train_balanced[,1],method='adaboost',trControl=fitControl,tuneLength=3)


data_test$pred_rf<-predict(model_rf,data_test,type='prob')[2]
data_test$pred_lr<-predict(model_lr,data_test,type='prob')[2]
data_test$pred_ada<-predict(model_ada,data_test,type='prob')[2]

data_test$averagepredictions = (data_test$pred_rf + data_test$pred_lr + data_test$pred_ada)/3

data_test$classprediction <- ifelse(data_test$averagepredictions >0.5, "1", "0")
data_test$classprediction <- as.factor(data_test$classprediction)
str(data_test)
data_test$ClaimStatus <- factor(c("0","1"))
data_test$classprediction <- factor(c("0","1"))

confusionMatrix(data_test$ClaimStatus, data_test$classprediction)
str(data_test)
