### Invoking libraries for transformation/model building that maybe required########
library(stringi)
library(dplyr)
library(caTools)
library(h2o)
library(caret)
library(rpart)
library(psych)
library(xlsx)
library(openxlsx)
library(mice)
library(woeBinning)
library(Hmisc)
library(dlookr)
library(readxl)
library(lubridate)
library(rpart.plot)
library(DMwR)
library("robustHD")
library(ggcorrplot)
library(e1071)
library(caret)
library(randomForest)
library(nnet)
library(car)


### Retriving subset of orignal data ####################
df_data <- read_excel(file.choose(),1)
table(df_data$ClaimStatus)
778/14177
### Making a subset #################
df_data_4=subset(df_data,Number_of_Driver == 4 | Number_of_Driver == 5 )

### Summary to look  for data types############
sapply(df_data_4, class)

### Correcting the data types interpreted by R as per data dictionary and analysis needs ##########################
df_data_4$`Sr No`<-as.factor(df_data_4$`Sr No`)
df_data_4$ClaimStatus<-as.factor(df_data_4$ClaimStatus)
df_data_4$ClaimFrequency<-as.factor(df_data_4$ClaimFrequency)
df_data_4$Billing_Term<-as.factor(df_data_4$Billing_Term)
df_data_4$Renewed<-as.factor(df_data_4$Renewed)
df_data_4$Number_of_Driver<-as.factor(df_data_4$Number_of_Driver)
df_data_4$Amendment<-as.factor(df_data_4$Amendment)
df_data_4$CoverageLiability<-as.factor(df_data_4$CoverageLiability)
df_data_4$CoverageMP<-as.factor(df_data_4$CoverageMP)
df_data_4$CoveragePD_1<-as.factor(df_data_4$CoveragePD_1)
df_data_4$CoveragePIP_CDW<-as.factor(df_data_4$CoveragePIP_CDW)
df_data_4$CoverageUMBI<-as.factor(df_data_4$CoverageUMBI)
df_data_4$CoverageUMPD<-as.factor(df_data_4$CoverageUMPD)
df_data_4$DriverAssigned_1<-as.factor(df_data_4$DriverAssigned_1)
df_data_4$Engine_1<-as.factor(df_data_4$Engine_1)
df_data_4$GaragedZIP_1<-as.factor(df_data_4$GaragedZIP_1)
df_data_4$MaritalStatus_1<-as.factor(df_data_4$MaritalStatus_1)
df_data_4$MaritalStatus_2<-as.factor(df_data_4$MaritalStatus_2)
df_data_4$MaritalStatus_3<-as.factor(df_data_4$MaritalStatus_3)
df_data_4$MaritalStatus_4<-as.factor(df_data_4$MaritalStatus_4)
df_data_4$MaritalStatus_5<-as.factor(df_data_4$MaritalStatus_5)
df_data_4$Occupation_1<-as.factor(df_data_4$Occupation_1)
df_data_4$Occupation_2<-as.factor(df_data_4$Occupation_2)
df_data_4$Occupation_3<-as.factor(df_data_4$Occupation_3)
df_data_4$Occupation_4<-as.factor(df_data_4$Occupation_4)
df_data_4$Occupation_5<-as.factor(df_data_4$Occupation_5)
df_data_4$Relation_1<-as.factor(df_data_4$Relation_1)
df_data_4$Relation_2<-as.factor(df_data_4$Relation_2)
df_data_4$Relation_3<-as.factor(df_data_4$Relation_3)
df_data_4$Relation_4<-as.factor(df_data_4$Relation_4)
df_data_4$Relation_5<-as.factor(df_data_4$Relation_5)
df_data_4$Rental_1<-as.factor(df_data_4$Rental_1)
df_data_4$Sex_1<-as.factor(df_data_4$Sex_1)
df_data_4$Sex_2<-as.factor(df_data_4$Sex_2)
df_data_4$Sex_3<-as.factor(df_data_4$Sex_3)
df_data_4$Sex_4<-as.factor(df_data_4$Sex_4)
df_data_4$Sex_5<-as.factor(df_data_4$Sex_5)
df_data_4$Surcharge1Unit_1<-as.factor(df_data_4$Surcharge1Unit_1)
df_data_4$Surcharge2Unit_1<-as.factor(df_data_4$Surcharge2Unit_1)
df_data_4$Surcharge3Unit_1<-as.factor(df_data_4$Surcharge3Unit_1)
df_data_4$Towing_1<-as.factor(df_data_4$Towing_1)
df_data_4$Units<-as.factor(df_data_4$Units)
df_data_4$VehicleInspected_1<-as.factor(df_data_4$VehicleInspected_1)
df_data_4$Year_1<-as.factor(df_data_4$Year_1)
df_data_4$Make_1<-as.factor(df_data_4$Make_1)
df_data_4$Model_1<-as.factor(df_data_4$Model_1)
df_data_4$Zip<-as.factor(df_data_4$Zip)
df_data_4$NoLossSigned<-as.factor(df_data_4$NoLossSigned)
df_data_4$Type<-as.factor(df_data_4$Type)
df_data_4$CancellationType<-as.factor(df_data_4$CancellationType)

### Dropping columns due to data insufficiency/ infeasiblity of use ########################################
### EDA for columns to drop due data insufficiency/ infeasivblity of use
as.data.frame(sapply(df_data_4, function(x) sum(is.na(x))*100/184))

#use of claim frequency in the model is illogical, a prediction model will not have this data
df_data_4$ClaimFrequency<- NULL

#behavior  of CoverageUMBI is same as CoverageUMBI variable. hence dropping one.
df_data_4$CoverageUMPD<- NULL

#droping coverageMP due to single level in data
table(df_data_4$CoverageMP)
df_data_4$CoverageMP=NULL

#Distance to work of individual drivers is already transformed into a variable as total distance to work.
df_data_4$DistanceToWork_1<- NULL
df_data_4$DistanceToWork_2<- NULL
df_data_4$DistanceToWork_3<- NULL
df_data_4$DistanceToWork_4<- NULL
df_data_4$DistanceToWork_5<- NULL

# Engine_1 has a lot of data glitches, infeasible to treat and use. moreover it will have too many variables.
df_data_4$Engine_1<- NULL

# All zip codes belong to state of texas and have varied cities which will overfit classification model due to too many levels.
df_data_4$GaragedZIP_1<- NULL
df_data_4$Zip<- NULL

#missing datapoints more than 5 percent
(sum(is.na(df_data_4$MaritalStatus_3))/184)*100
(sum(is.na(df_data_4$MaritalStatus_4))/184)*100
(sum(is.na(df_data_4$MaritalStatus_5))/184)*100
df_data_4$MaritalStatus_3<- NULL
df_data_4$MaritalStatus_4<- NULL
df_data_4$MaritalStatus_5<- NULL

#Model_1/occupations treatment and usage is infeasible since there are too many levels.
df_data_4$Model_1<- NULL
df_data_4$Occupation_1<- NULL
df_data_4$Occupation_2<- NULL
df_data_4$Occupation_3<- NULL
df_data_4$Occupation_4<- NULL
df_data_4$Occupation_5<- NULL


#relations and gender determinants have too many nulls to be imputed and used.
(sum(is.na(df_data_4$Relation_1))/184)*100
(sum(is.na(df_data_4$Relation_2))/184)*100
(sum(is.na(df_data_4$Relation_3))/184)*100
(sum(is.na(df_data_4$Relation_4))/184)*100
(sum(is.na(df_data_4$Relation_5))/184)*100
df_data_4$Relation_1=NULL
df_data_4$Relation_2=NULL
df_data_4$Relation_3<- NULL
df_data_4$Relation_4<- NULL
df_data_4$Relation_5<- NULL

(sum(is.na(df_data_4$Sex_3))/184)*100
(sum(is.na(df_data_4$Sex_4))/184)*100
(sum(is.na(df_data_4$Sex_5))/184)*100
df_data_4$Sex_3<- NULL
df_data_4$Sex_4<- NULL
df_data_4$Sex_5<- NULL

#serial number is a useless information from perdiction point of view.
df_data_4$`Sr No`<- NULL

#Surcharge unit variables have one level/levels with same claim rates need to be removed from the study.
table(df_data_4$Surcharge1Unit_1)
table(df_data_4$Surcharge2Unit_1)
table(df_data_4$Surcharge3Unit_1,df_data_4$ClaimStatus)
17/(98+17)  #claim rate when third surcharge componet is not applicable
14/(47+47)  #claim rate when third surcharge componet is not applicable
df_data_4$Surcharge1Unit_1=NULL
df_data_4$Surcharge2Unit_1=NULL


### Transformation of variables #############################################################
#adding up voilation points of each policy record.
df_data_4$total_voilation_points=df_data_4$ViolPoints1Driver_1+df_data_4$ViolPoints1Driver_2+df_data_4$ViolPoints1Driver_3+df_data_4$ViolPoints1Driver_4+df_data_4$ViolPoints1Driver_5+
  df_data_4$ViolPoints2Driver_1+df_data_4$ViolPoints2Driver_2+df_data_4$ViolPoints2Driver_3+df_data_4$ViolPoints2Driver_4+df_data_4$ViolPoints2Driver_5+
  df_data_4$ViolPoints3Driver_1+df_data_4$ViolPoints3Driver_2+df_data_4$ViolPoints3Driver_3+df_data_4$ViolPoints3Driver_4+df_data_4$ViolPoints3Driver_5+
  df_data_4$ViolPoints4Driver_1+df_data_4$ViolPoints4Driver_2+df_data_4$ViolPoints4Driver_3+df_data_4$ViolPoints4Driver_4+df_data_4$ViolPoints4Driver_5+
  df_data_4$ViolPoints5Driver_1+df_data_4$ViolPoints5Driver_2+df_data_4$ViolPoints5Driver_3+df_data_4$ViolPoints5Driver_4+df_data_4$ViolPoints5Driver_5+
  df_data_4$ViolPoints6Driver_1+df_data_4$ViolPoints6Driver_2+df_data_4$ViolPoints6Driver_3+df_data_4$ViolPoints6Driver_4+df_data_4$ViolPoints6Driver_5+
  df_data_4$ViolPoints7Driver_1+df_data_4$ViolPoints7Driver_2+df_data_4$ViolPoints7Driver_3+df_data_4$ViolPoints7Driver_4+df_data_4$ViolPoints7Driver_5+
  df_data_4$ViolPoints8Driver_1+df_data_4$ViolPoints8Driver_2+df_data_4$ViolPoints8Driver_3+df_data_4$ViolPoints8Driver_4+df_data_4$ViolPoints8Driver_5

#Dropping individual components of voilation points in the data set, the summary has been captured in total_voilation_points attribute
df_data_4$ViolPoints1Driver_1<- NULL
df_data_4$ViolPoints2Driver_1<- NULL
df_data_4$ViolPoints3Driver_1<- NULL
df_data_4$ViolPoints4Driver_1<- NULL
df_data_4$ViolPoints5Driver_1<- NULL
df_data_4$ViolPoints6Driver_1<- NULL
df_data_4$ViolPoints7Driver_1<- NULL
df_data_4$ViolPoints8Driver_1<- NULL
df_data_4$ViolPoints1Driver_2<- NULL
df_data_4$ViolPoints2Driver_2<- NULL
df_data_4$ViolPoints3Driver_2<- NULL
df_data_4$ViolPoints4Driver_2<- NULL
df_data_4$ViolPoints5Driver_2<- NULL
df_data_4$ViolPoints6Driver_2<- NULL
df_data_4$ViolPoints7Driver_2<- NULL
df_data_4$ViolPoints8Driver_2<- NULL
df_data_4$ViolPoints1Driver_3<- NULL
df_data_4$ViolPoints2Driver_3<- NULL
df_data_4$ViolPoints3Driver_3<- NULL
df_data_4$ViolPoints4Driver_3<- NULL
df_data_4$ViolPoints5Driver_3<- NULL
df_data_4$ViolPoints6Driver_3<- NULL
df_data_4$ViolPoints7Driver_3<- NULL
df_data_4$ViolPoints8Driver_3<- NULL
df_data_4$ViolPoints1Driver_4<- NULL
df_data_4$ViolPoints2Driver_4<- NULL
df_data_4$ViolPoints3Driver_4<- NULL
df_data_4$ViolPoints4Driver_4<- NULL
df_data_4$ViolPoints5Driver_4<- NULL
df_data_4$ViolPoints6Driver_4<- NULL
df_data_4$ViolPoints7Driver_4<- NULL
df_data_4$ViolPoints8Driver_4<- NULL
df_data_4$ViolPoints1Driver_5<- NULL
df_data_4$ViolPoints2Driver_5<- NULL
df_data_4$ViolPoints3Driver_5<- NULL
df_data_4$ViolPoints4Driver_5<- NULL
df_data_4$ViolPoints5Driver_5<- NULL
df_data_4$ViolPoints6Driver_5<- NULL
df_data_4$ViolPoints7Driver_5<- NULL
df_data_4$ViolPoints8Driver_5<- NULL

#derriving a varible with a count of total declared excluded drivers in policies
df_data_4$totalExcludedDrivers=(ifelse(is.na(df_data_4$ExcludedDriverName_01), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_02), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_03), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_04), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_05), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_06), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_07), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_08), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_09), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_10), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_11), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_12), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_14), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_15), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_16), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_17), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_18), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_19), 0, 1)+
                                  ifelse(is.na(df_data_4$ExcludedDriverName_20), 0, 1))

# dropping individual Excluded driver name components, quantitative taken in totalExcludedDrivers variable
df_data_4$ExcludedDriverName_01=NULL
df_data_4$ExcludedDriverName_02=NULL
df_data_4$ExcludedDriverName_03=NULL
df_data_4$ExcludedDriverName_04=NULL
df_data_4$ExcludedDriverName_05=NULL
df_data_4$ExcludedDriverName_06=NULL
df_data_4$ExcludedDriverName_07=NULL
df_data_4$ExcludedDriverName_08=NULL
df_data_4$ExcludedDriverName_09=NULL
df_data_4$ExcludedDriverName_10=NULL
df_data_4$ExcludedDriverName_11=NULL
df_data_4$ExcludedDriverName_12=NULL
df_data_4$ExcludedDriverName_13=NULL
df_data_4$ExcludedDriverName_14=NULL
df_data_4$ExcludedDriverName_15=NULL
df_data_4$ExcludedDriverName_16=NULL
df_data_4$ExcludedDriverName_17=NULL
df_data_4$ExcludedDriverName_18=NULL
df_data_4$ExcludedDriverName_19=NULL
df_data_4$ExcludedDriverName_20=NULL

#text mining Make variable
df_data_4$Make_1=toupper(df_data_4$Make_1)
level_key <- c("ACURA"="ACURA","AMG"="AMG","AUDI"="AUDI","BMW"="BMW","BUIC"="BUICK","BUICK"="BUICK","CADDY"="CADILLAC","CADILLAC"="CADILLAC","CHEV"="CHEVROLET","CHEVERLET"="CHEVROLET","CHEVEROLET"="CHEVROLET","CHEVORLET"="CHEVROLET","CHEVROET"="CHEVROLET","CHEVROLET"="CHEVROLET","CHEVROLET`"="CHEVROLET","CHEVROLETQ"="CHEVROLET","CHEVY"="CHEVROLET","CHEVYVAN"="CHEVROLET","CHRYLER"="CHRYSLER","CHRYSLER"="CHRYSLER","CHRYSLR"="CHRYSLER","CUTLASS"="OLDSMOBILE","DAEWOO"="DAEWOO","DAEWOOD"="DAEWOO","DATS"="DATSUN","DODGE"="DODGE","EAGLE"="EAGLE","FORD"="FORD","FORED"="FORD","FORK"="FORD","G.M.C"="GMC","GENERALMO"="GMC","GEO"="GEO","GMA"="GMC","GMC"="GMC","HONDA"="HONDA","HUMDAI"="HYUNDAI","HUMMER"="HUMMER","HUNDAI"="HYUNDAI","HYANDI"="HYUNDAI","HYNDAI"="HYUNDAI","HYUDAI"="HYUNDAI","HYUNDAI"="HYUNDAI","HYUNDIA"="HYUNDAI","HYUNDY"="HYUNDAI","INFINIT"="INFINITI","INFINITI"="INFINITI","ISUZI"="ISUZU","ISUZU"="ISUZU","IZUSU"="ISUZU","JAGUAR"="JAGUAR","JEEP"="JEEP","JEEPAMGC"="JEEP","KIA"="KIA","KIAMOTORS"="KIA","LANDROVER"="LANDROVER","LEXUS"="LEXUS","LINC"="LINCOLN","LINCOLN"="LINCOLN","LINCON"="LINCOLN","MAZDA"="MAZDA","MERC"="MERCEDES","MERCEDE"="MERCEDES","MERCEDES"="MERCEDES","MERCEDESB"="MERCEDES","MERCEDES-B"="MERCEDES","MERCURY"="MERCURY","MINI"="MINI","MITS"="MITSUBISHI","MITSUBI"="MITSUBISHI","MITSUBISHI"="MITSUBISHI","NISS"="NISSAN","NISSAN"="NISSAN","NON-OWNERS"="NO-OWNER","NO-OWNERS"="NO-OWNER","OLDS"="OLDSMOBILE","OLDSMOBILE"="OLDSMOBILE","PICKUP"="DODGE","PLYM"="PLYMOUTH","PLYMOUT"="PLYMOUTH","PLYMOUTH"="PLYMOUTH","PLYMT"="PLYMOUTH","PONT"="PONTIAC","PONTIAC"="PONTIAC","PONTIC"="PONTIAC","PORSCHE"="PORSCHE","SAAB"="SAAB","SATURN"="SATURN","SCION"="SCION","SUBARU"="SUBARU","SUZUKI"="SUZUKI","TOYOTA"="TOYOTA","VAN"="FORD","VOLK"="VOLKSWAGEN","VOLKS"="VOLKSWAGEN","VOLKSWAGEN"="VOLKSWAGEN","VOLVO"="VOLKSWAGEN","VW"="VOLKSWAGEN","WRANGLER"="WRANGLER")
df_data_4$Make_1=recode(df_data_4$Make_1, !!!level_key)
df_data_4$Make_1=as.factor(df_data_4$Make_1)
table(df_data_4$Make_1,df_data_4$ClaimStatus)
df_data_4$Make_1<-ifelse(df_data_4$Make_1=="FORD"| df_data_4$Make_1=="CHEVROLET"
                                       | df_data_4$Make_1=="NISSAN","Popular_cars","others")

table(df_data_4$Make_1,df_data_4$ClaimStatus)
16/(16+57)
17/(17+94)


#cancellation type cannot be known beforehand, this has a direct coreation with claim status. hence being dropped from study.
table(df_data_4$CancellationType)
df_data_4$CancellationType=NULL

#checking data enrichment per column
(as.data.frame(sapply(df_data_4, function(x) sum(is.na(x))*100/184)))

#binning levels of billing term variable. if billing term is more than 1 then we bin them in '2' category.
table(df_data_4$Billing_Term,df_data_4$ClaimStatus)
df_data_4$Billing_Term=ifelse(df_data_4$Billing_Term==3 | df_data_4$Billing_Term==6 , 2, 1)
table(df_data_4$Billing_Term,df_data_4$ClaimStatus)

#binning Ammendments variable. If there are any ammendments, then we will bin them in '1' category
table(df_data_4$Amendment,df_data_4$ClaimStatus)
df_data_4$Amendment=ifelse(df_data_4$Amendment==0 , 0, 1)
table(df_data_4$Amendment,df_data_4$ClaimStatus)
#binning coverage liablity variable. If there are any coverage other than 20/40/15(mapped to '0'), then we will bin them in '1' category
table(df_data_4$CoverageLiability,df_data_4$ClaimStatus)
df_data_4$CoverageLiability=ifelse(df_data_4$CoverageLiability=='20/40/15' , 0, 1)
table(df_data_4$CoverageLiability,df_data_4$ClaimStatus)

#binning driver_assigned_1 variable. binning driver assignment in three buckect considering data balance, claim rates and logical basis.
#'0' mapped to 1 or 2 driver assignment of first unit
#'1' mapped to 3 assignment of first unit
#'2' mapped to 4 or 5 driver assignment of first unit
table(df_data_4$DriverAssigned_1,df_data_4$ClaimStatus)
df_data_4$DriverAssigned_1=ifelse(df_data_4$DriverAssigned_1==1 | df_data_4$DriverAssigned_1==2 , 0, 
                                  ifelse(df_data_4$DriverAssigned_1==4 | df_data_4$DriverAssigned_1==5,2,1))
table(df_data_4$DriverAssigned_1,df_data_4$ClaimStatus)

#dropping coveragePD_1 column since claim rate is same across.
table(df_data_4$CoveragePD_1,df_data_4$ClaimStatus)
14/(14+66)
19/(19+85)


#dropping CoveragePIP_CDW due to data imbalance being created.
table(df_data_4$CoveragePIP_CDW,df_data_4$ClaimStatus)
df_data_4$CoveragePIP_CDW=NULL

#dropping Coverage_UMBI due to data imbalance being created.
table(df_data_4$CoverageUMBI,df_data_4$ClaimStatus)
df_data_4$CoverageUMBI=NULL

#dropping rental_1 due to lack of data points in level '1' and similar claim rates
table(df_data_4$Rental_1,df_data_4$ClaimStatus)
1/6
32/(32+146)
df_data_4$CoverageUMBI=NULL

#dropping sex_1 attribute due to similar claim rates.
table(df_data_4$Sex_1,df_data_4$ClaimStatus)
12/(12+63)
21/(21+88)


#making sex_2 varibale open to other/unspecified genders. Observing claim rates
sum(table(df_data_4$Sex_2))
sum(is.na(df_data_4$Sex_2))
table(df_data_4$Sex_2,df_data_4$ClaimStatus)
21/(73+21)
12/(12+62)
0/16
df_data_4$Sex_2=as.factor(df_data_4$Sex_2)

#dropping towing_1 varibale since claim rates across levels is almost same and there is data imbalance in the levels
table(df_data_4$Towing_1,df_data_4$ClaimStatus)
1/5
32/(32+146)
df_data_4$Towing_1=NULL

#binning  units variable to one or more than one levels mapped to 1 and 2 respectively.
table(df_data_4$Units,df_data_4$ClaimStatus)
df_data_4$Units=as.integer(df_data_4$Units)
df_data_4$Units=ifelse(df_data_4$Units>1,2,1) 
table(df_data_4$Units,df_data_4$ClaimStatus)
6/(66+6)
27/(27+85)
df_data_4$Units=as.factor(df_data_4$Units)

#binning year_1 variable to 2000 and below or more than 2000 year mapped to 0 and 1 respectively.
table(df_data_4$Year_1,df_data_4$ClaimStatus)
df_data_4$Year_1=as.numeric(as.character(df_data_4$Year_1))
df_data_4$Year_1=ifelse(df_data_4$Year_1<=2000,0,1)
table(df_data_4$Year_1,df_data_4$ClaimStatus)
15/(15+52)
18/(18+99)

#dropping type variable, due to definition insufficiency and thus inability to bin and explanablity/relaiblity issues
table(df_data_4$Type,df_data_4$ClaimStatus)
df_data_4$Type=NULL

#binning total voilation points derrived variable based on claim rates.
table(df_data_4$total_voilation_points,df_data_4$ClaimStatus)
df_data_4$total_voilation_points=as.numeric(df_data_4$total_voilation_points)
df_data_4$total_voilation_points=(ifelse(df_data_4$total_voilation_points>0,1,0))
table(df_data_4$total_voilation_points,df_data_4$ClaimStatus)
26/(131+26)
7/(20+7)

#binning total excluded drivers derrived varibale based on claim rates.
table(df_data_4$totalExcludedDrivers,df_data_4$ClaimStatus)
df_data_4$totalExcludedDrivers=as.numeric(df_data_4$totalExcludedDrivers)
df_data_4$totalExcludedDrivers=(ifelse(df_data_4$totalExcludedDrivers>3,1,0))
table(df_data_4$totalExcludedDrivers,df_data_4$ClaimStatus)
14/(105+14)
19/(46+19)

#dropping no loss signed variable since claim rates across levels are same and due to data imbalance
table(df_data_4$NoLossSigned,df_data_4$ClaimStatus)
df_data_4$NoLossSigned=NULL

#Applying log transformation to total distance to work variable.
table(df_data_4$Total_Distance_To_Work)
histogram(df_data_4$Total_Distance_To_Work)
table(log10(df_data_4$Total_Distance_To_Work+1),df_data_4$ClaimStatus)
boxplot(log10(df_data_4$Total_Distance_To_Work+1))
histogram(log10(df_data_4$Total_Distance_To_Work+1))
df_data_4$Total_Distance_To_Work=log10(df_data_4$Total_Distance_To_Work+1)


#Observing marital status 2
table(df_data_4$MaritalStatus_2)
sum(table(df_data_4$MaritalStatus_2))
sum(is.na(df_data_4$MaritalStatus_2))
table(df_data_4$MaritalStatus_2,df_data_4$ClaimStatus)
28/(28+123)
5/(12+5)
0/16

# Transformation for age parameter
reference_year=df_data_4$DOB1 %m+% years(df_data_4$AgeUSdriving_1)

df_data_4$DOB4
x <- as.Date("1900-01-01")
y=x %m+% days(df_data_4$DOB4)
df_data_4$DOB4=as.POSIXct.Date(y)

df_data_4$DOB5
x <- as.Date("1900-01-01")
y=x %m+% days(df_data_4$DOB5)
y
df_data_4$DOB5=as.POSIXct.Date(y)



ref2=as.integer(round((reference_year - df_data_4$DOB2)/365.25,0))
ref3=as.integer(round((reference_year - df_data_4$DOB3)/365.25,0))
ref4=as.integer(round((reference_year - df_data_4$DOB4)/365.25,0))
ref5=as.integer(round((reference_year - df_data_4$DOB5)/365.25,0))
df_data_4$AgeUSdriving_2
df_data_4$AgeUSdriving_2=ifelse(df_data_4$AgeUSdriving_2==0,ref2,df_data_4$AgeUSdriving_2)
df_data_4$AgeUSdriving_3
df_data_4$AgeUSdriving_3=ifelse(df_data_4$AgeUSdriving_3==0,ref3,df_data_4$AgeUSdriving_3)
df_data_4$AgeUSdriving_4
df_data_4$AgeUSdriving_4=ifelse(df_data_4$AgeUSdriving_4==0,ref4,df_data_4$AgeUSdriving_4)
df_data_4$AgeUSdriving_5=ifelse(df_data_4$AgeUSdriving_5==0,ref5,df_data_4$AgeUSdriving_5)
df_data_4$AgeUSdriving_5=ifelse(df_data_4$Number_of_Driver==4,NA,df_data_4$AgeUSdriving_5)
subset(df_data_4, Number_of_Driver == 5 )$AgeUSdriving_5

df_data_4$DOB1=NULL
df_data_4$DOB2=NULL
df_data_4$DOB3=NULL
df_data_4$DOB4=NULL
df_data_4$DOB5=NULL

as.data.frame(sapply(df_data_4, function(x) sum(is.na(x))*100/184))


rm(x)
rm(y)
rm(level_key)
rm(ref2)
rm(ref3)
rm(ref4)
rm(ref5)
rm(reference_year)

#to factor conversion
df_data_4$Billing_Term=as.factor(df_data_4$Billing_Term)
df_data_4$Amendment=as.factor(df_data_4$Amendment)
df_data_4$CoverageLiability=as.factor(df_data_4$CoverageLiability)
df_data_4$DriverAssigned_1=as.factor(df_data_4$DriverAssigned_1)
df_data_4$MaritalStatus_2=as.factor(df_data_4$MaritalStatus_2)
df_data_4$Year_1=as.factor(df_data_4$Year_1)
df_data_4$total_voilation_points=as.factor(df_data_4$total_voilation_points)
df_data_4$totalExcludedDrivers=as.factor(df_data_4$totalExcludedDrivers)
df_data_4$Make_1=as.factor(df_data_4$Make_1)

### imputation of missing values#############################
summary(df_data_4)
md.pattern(df_data_4)
md.pairs(df_data_4)
impute=mice(df_data_4[-c(10)],m=3,seed=123)
impute$imp$AgeUSdriving_3
print(impute)
class(complete(impute,2))
a=complete(impute,2)
summary(a)
AgeUSdriving_5=df_data_4$AgeUSdriving_5
final_df=cbind(a,AgeUSdriving_5)
final_df$AgeUSdriving_5



#After Imputation
rm(impute)
rm(df_data)
rm(df_data_4)
rm(a)
rm(AgeUSdriving_5)
summary(final_df)


### Outlier treatment##########################################
boxplot(final_df$Premium)
histogram(final_df$Premium)
min(final_df$Premium)
log10(final_df$Premium)
histogram(log10(final_df$Premium))
final_df$Premium=log10(final_df$Premium)

final_df$AgeUSdriving_5=NULL


### Train-test split##########################################################
trainIndex <- createDataPartition(final_df$ClaimStatus,p=0.75,list=FALSE)
df_train <-(final_df[trainIndex,])
df_test <- final_df[-trainIndex,]
#verification of split
table(df_train$ClaimStatus)[2]/sum(table(df_train$ClaimStatus))*100
table(df_test$ClaimStatus)[2]/sum(table(df_test$ClaimStatus))*100



### Model Building ##################################################
#inital model
set.seed(100)
r.ctrl = rpart.control(minsplit=7, minbucket = 5, cp=0.01,xval = 10)
cart.model <- rpart(ClaimStatus ~ ., data = df_train, method = "class", control = r.ctrl)
printcp(cart.model)
#Prediction on Train --------------------------------------------(85,85,67)
predicted_train <- predict(cart.model, df_train, type="class")

confusionMatrix(predicted_train,
                df_train$ClaimStatus,
                mode="everything",positive = '1')
#Prediction on Test ---------------------------------------------(80,91,37)
predicted_test <- predict(cart.model, df_test, type="class") 
confusionMatrix(predicted_test,
                df_test$ClaimStatus,
                mode="everything",positive = '1')
varImp(cart.model)



#model with SMOTE
#creating smoted data
set.seed(100)
df_train_bal = SMOTE(ClaimStatus ~ ., df_train, perc.over = 100, perc.under=200, k=5)
prop.table(table(df_train_bal$ClaimStatus))
#Previous Train Data
table(df_train$ClaimStatus)
prop.table(table(df_train$ClaimStatus))
#Synthetic Train Data
table(df_train_bal$ClaimStatus)
prop.table(table(df_train_bal$ClaimStatus))
#smote CART Model
set.seed(100)
r.ctrl = rpart.control(minsplit=35, minbucket = 16, cp=0.1,xval = 10,maxdepth =6)
cart.model <- rpart(ClaimStatus ~ ., data = df_train_bal, method = "class", control = r.ctrl)
printcp(cart.model)
#Prediction on Train --------------------------------------------(85,85,67)
predicted_train <- predict(cart.model, df_train_bal, type="class")

confusionMatrix(predicted_train,
                df_train_bal$ClaimStatus,
                mode="everything",positive = '1')



#Prediction on Test ---------------------------------------------(80,91,37)
predicted_test <- predict(cart.model, df_test, type="class") 
confusionMatrix(predicted_test,
                df_test$ClaimStatus,
                mode="everything",positive = '1')
varImp(cart.model)

#---------------------------------Random Forest-------------------------------------#


# Evaluate Model performance

set.seed(130)
RF <- randomForest(ClaimStatus ~ ., data = df_train_bal, 
                   ntree=51, mtry = 4, nodesize = 51,
                   importance=TRUE,doBest = TRUE)

#print(RF)
#Prediction on Train --------------------------------------------(97,95,91)
predicted <- predict(RF, df_train_bal, type="class")

confusionMatrix(predicted,
                df_train_bal$ClaimStatus,
                mode="everything",positive = '1')

#Prediction on Test ---------------------------------------------(83,90,18)
predicted <- predict(RF, df_test, type="class")

confusionMatrix(predicted,
                df_test$ClaimStatus,
                mode="everything",positive = '1')







#----------------------------Multinomial Logistic Regression----------------------------------#
df_train_bal$Renewed=NULL
# df_train_bal$AgeUSdriving_2=NULL
# df_train_bal$AgeUSdriving_4=NULL
df_train_bal$Make_1=NULL
df_train_bal$MaritalStatus_2=NULL
df_train_bal$Surcharge3Unit_1=NULL
df_train_bal$Sex_1=NULL
df_train_bal$CoveragePD_1=NULL
df_train_bal$DriverAssigned_1=NULL
# df_train_bal$Sex_2=NULL



df_test$Renewed=NULL
# df_train_bal$AgeUSdriving_2=NULL
# df_train_bal$AgeUSdriving_4=NULL
df_test$Make_1=NULL
df_test$MaritalStatus_2=NULL
df_test$Surcharge3Unit_1=NULL
df_test$Sex_1=NULL
df_test$CoveragePD_1=NULL
df_test$DriverAssigned_1=NULL
# df_test$Sex_2=NULL

multinom <- multinom(ClaimStatus ~ ., data = df_train_bal, maxit=5)
summary(multinom)

#Prediction on Train
predicted=predict(multinom,df_train_bal,type="class")

confusionMatrix(predicted,
                df_train_bal$ClaimStatus,
                mode="everything",positive = '1')

#Prediction on Test 
predicted=predict(multinom,df_test,type="class")


confusionMatrix(predicted,
                df_test$ClaimStatus,
                mode="everything",positive = '1')


logit = glm(ClaimStatus~ .,data= df_train_bal, family = binomial(link = "logit"))



summary(logit)
varImp(logit)
vif(logit)

#--------------------------Model Performance Measures------------------------#


library(ROCR)
test.roc <- prediction(as.numeric(predicted), as.numeric(df_test$ClaimStatus))

plot(performance(test.roc, "tpr", "fpr"), 
     col = "red", main = "ROC Curve for train data")
abline(0, 1, lty = 8, col = "blue")

# AUC
test.auc = performance(test.roc, "auc")
test.area = as.numeric(slot(test.auc, "y.values"))
test.area

plot.roc((df_test$ClaimStatus),as.numeric(predicted))
auc(df_test$ClaimStatus,as.numeric(predicted))


# KS
ks.test <- performance(test.roc, "tpr", "fpr")
test.ks <- max(attr(ks.test, "y.values")[[1]] - (attr(ks.test, "x.values")[[1]]))
test.ks
#KS=49.03, indicates a good model.


# Gini
test.gini = (2 * test.area) - 1
test.gini

#The Loss for the model through GINI is 0.5094



