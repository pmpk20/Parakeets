#### Parakeets Invasive Species Paper ####
## Function: Estimates a basic MNL model
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 28/03/2022
## TODO: more complex models

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
# Setup Environment: ####
#------------------------------

rm(list = ls())
# setwd("Z:/Parakeets")


library(apollo)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)
library(reshape2)

database <- data.frame(read.csv("database_Parakeets_2022_03_28.csv"))
database <- database[database$rprotest==0,]
# database <- database[database$bird==6,] ## Testing parakeets only
# database = subset(database,database$bird==6)
#### Estimate MNL ####


apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Parakeets_MNL_2022_03_25",
  modelDescr      = "Parakeets_MNL_2022_03_25",
  indivID         = "ID"
)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_A      = 0,asc_B=0,asc_C=0,

              b_PopSmallDecrease=0,
              b_PopLargeDecrease=0,
              b_PopSmallIncrease=0,
              b_ManagementLethal=0,
              b_ManagementDeterrent=0,
              b_ManagementNothing=0,
              b_Cost=0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_B","asc_C")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()

  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["A"]]  = asc_A
  V[["B"]]  =asc_B+b_Cost *(CostB+
    b_PopSmallDecrease*(PopSmallDecreaseB==1)+
    b_PopLargeDecrease*(PopLargeDecreaseB==1)+
    b_PopSmallIncrease*(PopSmallIncreaseB==1)+
    b_ManagementLethal*(ManagementLethalB==1)+
    b_ManagementDeterrent*(ManagementDeterrentB==1)+
    b_ManagementNothing*(ManagementNothingB==1))
  
  V[["C"]]  = asc_C+b_Cost *(CostC+
    b_PopSmallDecrease*(PopSmallDecreaseC==1)+
    b_PopLargeDecrease*(PopLargeDecreaseC==1)+
    b_PopSmallIncrease*(PopSmallIncreaseC==1)+
    b_ManagementLethal*(ManagementLethalC==1)+
    b_ManagementDeterrent*(ManagementDeterrentC==1)+
    b_ManagementNothing*(ManagementNothingC==1))

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A=1, B=2, C=3), 
    avail         = list(A=1, B=1, C=1), 
    choiceVar     = Alternative,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

Parakeets_MNL_2022_03_25 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(Parakeets_MNL_2022_03_25,modelOutput_settings = list(printPVal=TRUE))
Parakeets_MNL_2022_03_25$estimate



# -1*(Parakeets_MNL_2022_03_25$estimate/Parakeets_MNL_2022_03_25$estimate["b_Cost"])


# apollo_saveOutput(Parakeets_MNL_2022_03_25,saveOutput_settings = list(printPVal=TRUE))
# 
# 
# apollo_deltaMethod(Parakeets_MNL_2022_03_25,
#                    deltaMethod_settings = list(operation="ratio",
#                                                parName1="b_PopSmallDecrease",
#                                                parName2="b_Cost"))