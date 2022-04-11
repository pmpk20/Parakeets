#### Parakeets Invasive Species Paper ####
## Function: Sets up the data
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 25/03/2022
## TODO: translate exampledatabase <- data.frame(dataForR) to this case



#------------------------------
# Replication Information: ####
#------------------------------

# R version 4.1.2 (2021-11-01)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# [1] LC_COLLATE=English_United Kingdom.1252 
# other attached packages:
#   [1] ggridges_0.5.3 ggplot2_3.3.5  magrittr_2.0.2 dplyr_1.0.7    apollo_0.2.7 


#------------------------------
# Data Cleaning: ####
#------------------------------

rm(list=ls())
## Import Data:
load("dataForR.rdata")


#---- Renaming:
## Rename population attribute levels:
colnames(dataForR)[which(names(dataForR)=="sdecr")] <- "PopSmallDecrease"
colnames(dataForR)[which(names(dataForR)=="ldecr")] <- "PopLargeDecrease"
colnames(dataForR)[which(names(dataForR)=="sincr")] <- "PopSmallIncrease"

## Rename management attribute levels:
colnames(dataForR)[which(names(dataForR)=="lethal")] <- "ManagementLethal"
colnames(dataForR)[which(names(dataForR)=="deterr")] <- "ManagementDeterrent"
colnames(dataForR)[which(names(dataForR)=="noaction")] <- "ManagementNothing"

## Misc fixes:
colnames(dataForR)[which(names(dataForR)=="id")] <- "ID"
colnames(dataForR)[which(names(dataForR)=="cost")] <- "Cost"
colnames(dataForR)[which(names(dataForR)=="cs")] <- "Task"
colnames(dataForR)[which(names(dataForR)=="alt")] <- "Alternative"
colnames(dataForR)[which(names(dataForR)=="choice")] <- "Choice"
colnames(dataForR)[which(names(dataForR)=="consorg")] <- "Charity"
colnames(dataForR)[which(names(dataForR)=="genderm")] <- "Gender"
colnames(dataForR)[which(names(dataForR)=="netherl")] <- "Netherlands"

colnames(dataForR)[which(names(dataForR)=="awareias")] <- "InvasiveAware"
colnames(dataForR)[which(names(dataForR)=="awaredef")] <- "InvasiveDefinition"
colnames(dataForR)[which(names(dataForR)=="thinkias")] <- "InvasiveThink"


## Protest Responses
colnames(dataForR)[which(names(dataForR)=="reasona")] <- "ProtestReason"
colnames(dataForR)[which(names(dataForR)=="choseno")] <- "AlwaysNo"
colnames(dataForR)[which(names(dataForR)=="conbird")] <- "AttendanceSpecies"
colnames(dataForR)[which(names(dataForR)=="connat")] <- "AttendanceNative"
colnames(dataForR)[which(names(dataForR)=="conpop")] <- "AttendancePopulation"
colnames(dataForR)[which(names(dataForR)=="concon")] <- "AttendanceManagement"
colnames(dataForR)[which(names(dataForR)=="contax")] <- "AttendanceTax"



## Other Fixes That Were Irritating Me:
colnames(dataForR)[which(names(dataForR)=="birdsong")] <- "P4Listen"
colnames(dataForR)[which(names(dataForR)=="watchb")] <- "P4Watch"
colnames(dataForR)[which(names(dataForR)=="nuisance")] <- "P4Nuisance"

colnames(dataForR)[which(names(dataForR)=="ranka")] <- "RankingGoose"
colnames(dataForR)[which(names(dataForR)=="rankb")] <- "RankingCrow"
colnames(dataForR)[which(names(dataForR)=="rankc")] <- "RankingGreylag"
colnames(dataForR)[which(names(dataForR)=="rankd")] <- "RankingCarrion"
colnames(dataForR)[which(names(dataForR)=="ranke")] <- "RankingPigeon"
colnames(dataForR)[which(names(dataForR)=="rankf")] <- "RankingParakeet"

colnames(dataForR)[which(names(dataForR)=="reca")] <- "GooseRecognise"
colnames(dataForR)[which(names(dataForR)=="seena")] <- "GooseSeen"
colnames(dataForR)[which(names(dataForR)=="recc")] <- "GreylagRecognise"
colnames(dataForR)[which(names(dataForR)=="seenc")] <- "GreylagSeen"
colnames(dataForR)[which(names(dataForR)=="recd")] <- "CarrionRecognise"
colnames(dataForR)[which(names(dataForR)=="seend")] <- "CarrionSeen"
colnames(dataForR)[which(names(dataForR)=="recb")] <- "CrowRecognise"
colnames(dataForR)[which(names(dataForR)=="seenb")] <- "CrowSeen"
colnames(dataForR)[which(names(dataForR)=="rece")] <- "PigeonRecognise"
colnames(dataForR)[which(names(dataForR)=="seene")] <- "PigeonSeen"
colnames(dataForR)[which(names(dataForR)=="recf")] <- "ParakeetRecognise"
colnames(dataForR)[which(names(dataForR)=="seenf")] <- "ParakeetSeen"




#----- Coding attributes:

dataForR$PopSmallDecreaseA <- dataForR$PopSmallDecrease[dataForR$Alternative==1]
dataForR$PopSmallDecreaseB <- dataForR$PopSmallDecrease[dataForR$Alternative==2]
dataForR$PopSmallDecreaseC <- dataForR$PopSmallDecrease[dataForR$Alternative==3]

dataForR$PopLargeDecreaseA <- dataForR$PopLargeDecrease[dataForR$Alternative==1]
dataForR$PopLargeDecreaseB <- dataForR$PopLargeDecrease[dataForR$Alternative==2]
dataForR$PopLargeDecreaseC <- dataForR$PopLargeDecrease[dataForR$Alternative==3]

dataForR$PopSmallIncreaseA <- dataForR$PopSmallIncrease[dataForR$Alternative==1]
dataForR$PopSmallIncreaseB <- dataForR$PopSmallIncrease[dataForR$Alternative==2]
dataForR$PopSmallIncreaseC <- dataForR$PopSmallIncrease[dataForR$Alternative==3]

dataForR$ManagementLethalA <- dataForR$ManagementLethal[dataForR$Alternative==1]
dataForR$ManagementLethalB <- dataForR$ManagementLethal[dataForR$Alternative==2]
dataForR$ManagementLethalC <- dataForR$ManagementLethal[dataForR$Alternative==3]

dataForR$ManagementDeterrentA <- dataForR$ManagementDeterrent[dataForR$Alternative==1]
dataForR$ManagementDeterrentB <- dataForR$ManagementDeterrent[dataForR$Alternative==2]
dataForR$ManagementDeterrentC <- dataForR$ManagementDeterrent[dataForR$Alternative==3]

dataForR$ManagementNothingA <- dataForR$ManagementNothing[dataForR$Alternative==1]
dataForR$ManagementNothingB <- dataForR$ManagementNothing[dataForR$Alternative==2]
dataForR$ManagementNothingC <- dataForR$ManagementNothing[dataForR$Alternative==3]

dataForR$CostA <- dataForR$Cost[dataForR$Alternative==1]
dataForR$CostB <- dataForR$Cost[dataForR$Alternative==2]
dataForR$CostC <- dataForR$Cost[dataForR$Alternative==3]

#----- Trimming dataset
dataForR <- dataForR[dataForR$Choice==1,]
dataForR <- dataForR[order(dataForR$ID),]
database <- data.frame(dataForR)

## Export Data For Apollo:
# write.csv(database,"database_Parakeets_2022_02_15.csv")
write.csv(database,"database_Parakeets_2022_03_28.csv")


