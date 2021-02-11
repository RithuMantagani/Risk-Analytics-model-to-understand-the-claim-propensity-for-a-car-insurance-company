#invoking necessary libraries
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
#Reding data into a dataframe
insurance_data = read.csv(file.choose())
#filtering data with number of drivers=1
insurance_data=filter(insurance_data, Number_of_Driver==1)

#Removing unwanted variables
Driver1_insurance_data=subset(insurance_data,select = -c(1,2,8:13,15:19,27:30,32,55:58,60:63,65:68,71:74,82:85,87:90,92:95,97:100,102:105,107:110,112:115,117:120,125))



#Changing variables to factor type
Driver1_insurance_data$ClaimFrequency<-as.factor(Driver1_insurance_data$ClaimFrequency)
Driver1_insurance_data$Billing_Term<-as.factor(Driver1_insurance_data$Billing_Term)
Driver1_insurance_data$DriverAssigned_1<-as.factor(Driver1_insurance_data$DriverAssigned_1)




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
table(Driver1_insurance_data$Towing_1)
Driver1_insurance_data1$Towing_1[Driver1_insurance_data1$Towing_1==50]<-0
Driver1_insurance_data1$Towing_1[Driver1_insurance_data1$Towing_1==70]<-0
Driver1_insurance_data1$Towing_1<-as.factor(Driver1_insurance_data1$Towing_1)

#treating outliers for year 1 variable

Driver1_insurance_data1$Year_1[Driver1_insurance_data1$Year_1==988]<-1998##logically
Driver1_insurance_data1$Year_1[Driver1_insurance_data1$Year_1==199]<-NA
Driver1_insurance_data1$Year_1[Driver1_insurance_data1$Year_1==-3]<-NA
Driver1_insurance_data1$Year_1[Driver1_insurance_data1$Year_1==0]<-NA
Driver1_insurance_data1$Year_1<-impute(Driver1_insurance_data1$Year_1,fun = median)
Driver1_insurance_data1$Year_1<-as.factor(Driver1_insurance_data1$Year_1)
summary(Driver1_insurance_data1$Year_1)

#treating Rental_1 variable for outliers, 0.25% of data indentified as outliers
table(Driver1_insurance_data$Rental_1)
Driver1_insurance_data1$Rental_1[Driver1_insurance_data1$Rental_1==20]<-0
Driver1_insurance_data1$Rental_1[Driver1_insurance_data1$Rental_1==25]<-0
Driver1_insurance_data1$Rental_1[Driver1_insurance_data1$Rental_1==35]<-0
Driver1_insurance_data1$Rental_1<-as.factor(Driver1_insurance_data1$Rental_1)


#Recoding the Make_1 and occupation variables

level_key <- c("ACURA"="ACURA","AMG"="AMG","AUDI"="AUDI","BMW"="BMW","BUIC"="BUICK","BUICK"="BUICK","CADDY"="CADILLAC","CADILLAC"="CADILLAC","CHEV"="CHEVROLET","CHEVERLET"="CHEVROLET","CHEVEROLET"="CHEVROLET","CHEVORLET"="CHEVROLET","CHEVROET"="CHEVROLET","CHEVROLET"="CHEVROLET","CHEVROLET`"="CHEVROLET","CHEVROLETQ"="CHEVROLET","CHEVY"="CHEVROLET","CHEVYVAN"="CHEVROLET","CHRYLER"="CHRYSLER","CHRYSLER"="CHRYSLER","CHRYSLR"="CHRYSLER","CUTLASS"="OLDSMOBILE","DAEWOO"="DAEWOO","DAEWOOD"="DAEWOO","DATS"="DATSUN","DODGE"="DODGE","EAGLE"="EAGLE","FORD"="FORD","FORED"="FORD","FORK"="FORD","G.M.C"="GMC","GENERALMO"="GMC","GEO"="GEO","GMA"="GMC","GMC"="GMC","HONDA"="HONDA","HUMDAI"="HYUNDAI","HUMMER"="HUMMER","HUNDAI"="HYUNDAI","HYANDI"="HYUNDAI","HYNDAI"="HYUNDAI","HYUDAI"="HYUNDAI","HYUNDAI"="HYUNDAI","HYUNDIA"="HYUNDAI","HYUNDY"="HYUNDAI","INFINIT"="INFINITI","INFINITI"="INFINITI","ISUZI"="ISUZU","ISUZU"="ISUZU","IZUSU"="ISUZU","JAGUAR"="JAGUAR","JEEP"="JEEP","JEEPAMGC"="JEEP","KIA"="KIA","KIAMOTORS"="KIA","LANDROVER"="LANDROVER","LEXUS"="LEXUS","LINC"="LINCOLN","LINCOLN"="LINCOLN","LINCON"="LINCOLN","MAZDA"="MAZDA","MERC"="MERCEDES","MERCEDE"="MERCEDES","MERCEDES"="MERCEDES","MERCEDESB"="MERCEDES","MERCEDES-B"="MERCEDES","MERCURY"="MERCURY","MINI"="MINI","MITS"="MITSUBISHI","MITSUBI"="MITSUBISHI","MITSUBISHI"="MITSUBISHI","NISS"="NISSAN","NISSAN"="NISSAN","NON-OWNERS"="NO-OWNER","NO-OWNERS"="NO-OWNER","OLDS"="OLDSMOBILE","OLDSMOBILE"="OLDSMOBILE","PICKUP"="DODGE","PLYM"="PLYMOUTH","PLYMOUT"="PLYMOUTH","PLYMOUTH"="PLYMOUTH","PLYMT"="PLYMOUTH","PONT"="PONTIAC","PONTIAC"="PONTIAC","PONTIC"="PONTIAC","PORSCHE"="PORSCHE","SAAB"="SAAB","SATURN"="SATURN","SCION"="SCION","SUBARU"="SUBARU","SUZUKI"="SUZUKI","TOYOTA"="TOYOTA","VAN"="FORD","VOLK"="VOLKSWAGEN","VOLKS"="VOLKSWAGEN","VOLKSWAGEN"="VOLKSWAGEN","VOLVO"="VOLKSWAGEN","VW"="VOLKSWAGEN","WRANGLER"="WRANGLER")
Driver1_insurance_data1$Make_1=recode(Driver1_insurance_data1$Make_1, !!!level_key)
Driver1_insurance_data1$Make_1=as.factor(Driver1_insurance_data1$Make_1)
#converting to factor varibable
Driver1_insurance_data1$NoLossSigned<-as.factor(Driver1_insurance_data1$NoLossSigned)

#Creating buckets for age and premium variable


#bining age varibale 
binning<-woe.binning(Driver1_insurance_data1,'ClaimStatus','AgeUSdriving_1',min.perc.total=0.01, min.perc.class=0.01,
                     stop.limit=0.1,event.class=1)

# Plot the binned variables
woe.binning.plot(binning)
#9 bins are formed
woe.binning.table(binning)
Driver1_insurance_data1$bins <- rbin_manual(Driver1_insurance_data1,Driver1_insurance_data1$ClaimStatus, insurance_data$AgeUSdriving_1,c(20,25,26))

#bining premimums
premium_binning <- woe.binning(Driver1_insurance_data1,'ClaimStatus',
                               'Premium',min.perc.total=0.01, min.perc.class=0.01,
                               stop.limit=0.1,event.class=1)
# Plot the binned variables
woe.binning.plot(premium_binning)
#9 bins are formed
woe.binning.table(premium_binning)

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
Driver1_insurance_Final$total_excld_drivers<-rowSums(Driver1_insurance_Final[30:49])
Driver1_insurance_Final<-Driver1_insurance_Final[-c(29:49)]
table(Driver1_insurance_Final$total_excld_drivers)
#install.packages("xlsx")

write.xlsx(Driver1_insurance_Final,file="C:\\Users\\Baba\\Desktop\\Capstone\\dataset.xlsx")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Spilting the dataset~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(Driver1_insurance_Final,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 =subset(Driver1_insurance_Final,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test1=subset(Driver1_insurance_Final, sample==FALSE)

"---------------------------------------------------------------------------------------"

attach(Driver1_insurance_Final)

#checking for corelation -creating matrix
set1<-data.frame(ClaimStatus,ClaimFrequency,Premium,Billing_Term,Renewed,AgeUSdriving_1,DistanceToWork_1,DriverAssigned_1)
set2<-data.frame(ClaimStatus,CoverageLiability,CoverageMP,CoveragePD_1,CoveragePD_1,CoveragePIP_CDW,CoveragePIP_CDW,CoverageUMBI)
set3<-data.frame(ClaimStatus,GaragedZIP_1,MaritalStatus_1,Rental_1,Sex_1,Surcharge1Unit_1,Surcharge2Unit_1,Surcharge3Unit_1)
set4<-data.frame(ClaimStatus,Towing_1,Units,VehicleInspected_1,Year_1,Make_1,Zip,NoLossSigned)
set5<-data.frame(ClaimStatus,Type,total_violpt,total_excld_drivers)

pairs.panels(set1, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             digit=4,
             density = FALSE,  # do not show density plots
             ellipses = FALSE, # do not show correlation ellipses
             cor=TRUE
)

pairs.panels(set2, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             digit=4,
             density = FALSE,  # do not show density plots
             ellipses = FALSE, # do not show correlation ellipses
             cor=TRUE
)
pairs.panels(set3, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             digit=4,
             density = FALSE,  # do not show density plots
             ellipses = FALSE, # do not show correlation ellipses
             cor=TRUE
)
pairs.panels(set4, 
             method = "pearson", # correlation method
             digit=4,
             hist.col = "#00AFBB",
             density = FALSE,  # do not show density plots
             ellipses = FALSE, # do not show correlation ellipses
             cor=TRUE
)
pairs.panels(set5, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             digit=4,
             density = FALSE,  # do not show density plots
             ellipses = FALSE, # do not show correlation ellipses
             cor=TRUE
)

#~~~~~~~~~~~~~~~~~~~~~~~normality plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
plot_normality(Driver1_insurance_Final,Premium)
plot_normality(Driver1_insurance_Final,AgeUSdriving_1)
plot_normality(Driver1_insurance_Final,total_violpt)
plot_normality(Driver1_insurance_Final,total_excld_drivers)
#~~~~~~~~~~~~~~~~~~~~~~EDA continued~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
status <- target_by(Driver1_insurance_Final, ClaimStatus)
ClaimStatus_Premium <- relate(status, Premium)
ClaimStatus_Premium
plot(ClaimStatus_Premium)


ClaimStatus_Frequency <- relate(status, ClaimFrequency)
ClaimStatus_Frequency
summary(ClaimStatus_Frequency)
plot(ClaimStatus_Frequency)

ClaimStatus_billingTerm <- relate(status, Billing_Term)
ClaimStatus_billingTerm
summary(ClaimStatus_billingTerm)
plot(ClaimStatus_billingTerm)

ClaimStatus_Renewed <- relate(status, Renewed)
ClaimStatus_Renewed
summary(ClaimStatus_Renewed)
plot(ClaimStatus_Renewed)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~table summary~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
table(ClaimStatus, Billing_Term)
table(Driver1_insurance_Final$total_excld_drivers,ClaimStatus)
table(Sex_1,ClaimStatus)
table(MaritalStatus_1,ClaimStatus)

