#### Parakeets Invasive Species Paper ####
## Function: Estimates a basic MXL model
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 25/02/2022
## TODO: investigate if we need draws for each level


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

database <- data.frame(read.csv("database_Parakeets_2022_02_15.csv"))
# database <- database[database$rprotest==0, ] ## Remove protestors
database <- database[database$bird==6, ] ## Remove protestors

#------------------------------
# Setup Apollo: ####
#------------------------------

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Parakeeets_MXL_Levels_2022_03_25",
  modelDescr      = "Parakeeets_MXL_Levels_2022_03_25",
  indivID         = "ID",
  mixing=TRUE,
  nCores=10
)


#------------------------------
# Define Parameters: ####
#------------------------------

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_A      = 0,
              asc_B      = 0,
              asc_C      = 0,
              mu_PopSmallDecrease=1,
              mu_PopLargeDecrease=1,
              mu_PopSmallIncrease=1,
              mu_ManagementLethal=1,
              mu_ManagementDeterrent=1,
              mu_ManagementNothing=1,
              mu_Cost=-3,
              sig_PopSmallDecrease=0,
              sig_PopLargeDecrease=0,
              sig_PopSmallIncrease=0,
              sig_ManagementLethal=0,
              sig_ManagementDeterrent=0,
              sig_ManagementNothing=0,
              sig_Cost=-0.01)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_B","asc_C")



### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "pmc",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("Draws_PopSmallDecrease" , "Draws_PopLargeDecrease","Draws_PopSmallIncrease",
                     "Draws_ManagementLethal","Draws_ManagementDeterrent","Draws_ManagementNothing",
                     "draws_Cost" ),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)
### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Cost"]] = -exp(mu_Cost + sig_Cost * draws_Cost )
  randcoeff[["b_PopSmallDecrease"]] =  (mu_PopSmallDecrease+    sig_PopSmallDecrease*Draws_PopSmallDecrease)
  randcoeff[["b_PopLargeDecrease"]] =  (mu_PopLargeDecrease+    sig_PopLargeDecrease*Draws_PopLargeDecrease)
  randcoeff[["b_PopSmallIncrease"]] =  ( mu_PopSmallIncrease+    sig_PopSmallIncrease*Draws_PopSmallIncrease)
  randcoeff[["b_ManagementLethal"]] =  (mu_ManagementLethal+    sig_ManagementLethal*Draws_ManagementLethal)
  randcoeff[["b_ManagementDeterrent"]] =  ( mu_ManagementDeterrent+    sig_ManagementDeterrent*Draws_ManagementDeterrent)
  randcoeff[["b_ManagementNothing"]] =  (mu_ManagementNothing+    sig_ManagementNothing*Draws_ManagementNothing)
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()


#------------------------------
# Establish Likelihood Function: ####
#------------------------------


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["A"]]  = asc_A 
  
  
  V[["B"]]  = asc_B  +  b_Cost *((CostB)+
    (b_PopSmallDecrease*(PopSmallDecreaseB==1))+
    (b_PopLargeDecrease*(PopLargeDecreaseB==1))+
    (b_PopSmallIncrease*(PopSmallIncreaseB==1))+
    (b_ManagementLethal*(ManagementLethalB==1))+
    (b_ManagementDeterrent*(ManagementDeterrentB==1))+
    (b_ManagementNothing*(ManagementNothingB==1)))
  
  V[["C"]]  = asc_C  + b_Cost *((CostC)+
    (b_PopSmallDecrease*(PopSmallDecreaseC==1))+
    (b_PopLargeDecrease*(PopLargeDecreaseC==1))+
    (b_PopSmallIncrease*(PopSmallIncreaseC==1))+
    (b_ManagementLethal*(ManagementLethalC==1))+
    (b_ManagementDeterrent*(ManagementDeterrentC==1))+
    (b_ManagementNothing*(ManagementNothingC==1)))
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A=1, B=2, C=3), 
    avail         = list(A=1, B=1, C=1), 
    choiceVar     = Alternative,
    utilities     = V
  )
  
  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#------------------------------
# Estimation: ####
#------------------------------

## Actually estimates the model
Parakeeets_MXL_Levels_2022_03_25 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

## Model output and results here alongside saving information
apollo_modelOutput(Parakeeets_MXL_Levels_2022_03_25,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(Parakeeets_MXL_Levels_2022_03_25,saveOutput_settings = list(printPVal=TRUE))
saveRDS(Parakeeets_MXL_Levels_2022_03_25, file="Parakeeets_MXL_Levels_2022_03_25.rds")


#------------------------------
# Summarise WTP: ####
#------------------------------

Model <- readRDS("Parakeeets_MXL_Levels_2022_03_25.rds") ## Enter model of interest RDS here
Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25 <- apollo_conditionals(Model,apollo_probabilities,apollo_inputs )
write.csv(Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25,"Parakeeets_MXL_Levels_2022_03_25_WTP.csv")


Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25Summary <-data.frame(cbind("b_PopSmallDecrease"=Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25$b_PopSmallDecrease$post.mean,
                                                    "b_PopLargeDecrease"=Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25$b_PopLargeDecrease$post.mean,
                                                    "b_PopSmallIncrease"=Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25$b_PopSmallIncrease$post.mean,
                                                    "b_ManagementLethal"=Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25$b_ManagementLethal$post.mean,
                                                    "b_ManagementDeterrent"=Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25$b_ManagementDeterrent$post.mean,
                                                    "b_ManagementNothing"=Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25$b_ManagementNothing$post.mean))
apollo_sink()
Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25Summary %>% summarise(across(everything(),list(mean)))
apollo_sink()

ggsave(melt(Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25Summary) %>% ggplot(aes(x=value,y=variable,group=variable,fill=variable))+
         geom_density_ridges()+geom_vline(xintercept=0,linetype='dashed')+
         scale_x_continuous(name="mWTP in pounds.", limits=c(-15,15),breaks = seq(-15,15,2)), 
       device = "jpeg",
       filename = "Parakeeets_MXL_Levels_2022_03_25_WTP_2022_03_25_DensityPlot.jpeg",
       width=20,height=15,units = "cm",dpi=500)