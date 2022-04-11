#### Parakeets Paper ####
## Function: Estimates a 3-class LCM with city and type covariates
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 25/03/2022
## TODO: add covariates



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
database$weights=1


apollo_initialise()

apollo_control = list(
  modelName  = "Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28",
  modelDescr = "Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28",
  indivID    = "ID",
  nCores     = 10,
  noValidation=TRUE,noDiagnostics=TRUE
)


apollo_beta = c(
  asc_A_Class1 = 0,  asc_A_Class2 = 0,asc_A_Class3=0,
  b_PopSmallDecrease_Class1=1,
  b_PopLargeDecrease_Class1=1,
  b_PopSmallIncrease_Class1=1,
  b_ManagementLethal_Class1=1,
  b_ManagementDeterrent_Class1=1,
  b_ManagementNothing_Class1=1,
  b_Cost_Class1=-1,
  b_PopSmallDecrease_Class2=2,
  b_PopLargeDecrease_Class2=2,
  b_PopSmallIncrease_Class2=2,
  b_ManagementLethal_Class2=2,
  b_ManagementDeterrent_Class2=2,
  b_ManagementNothing_Class2=2,
  b_Cost_Class2=-2,
  b_PopSmallDecrease_Class3=3,
  b_PopLargeDecrease_Class3=3,
  b_PopSmallIncrease_Class3=3,
  b_ManagementLethal_Class3=3,
  b_ManagementDeterrent_Class3=3,
  b_ManagementNothing_Class3=3,
  b_Cost_Class3=-3,
  delta_Class1 = 0.5,
  delta_Class2 = 1.1,
  delta_Class3 = 0,
  gamma_info_Class1=0,
  gamma_country_Class1=0,
  gamma_info_Class2=1,
  gamma_country_Class2=1
  
)


apollo_fixed = c("delta_Class3")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["asc_A"]] = list(asc_A_Class1,asc_A_Class2,asc_A_Class3)
  lcpars[["b_Cost"]] = list(b_Cost_Class1,b_Cost_Class2,b_Cost_Class3)
  lcpars[["b_PopSmallDecrease"]] =list(b_PopSmallDecrease_Class1,b_PopSmallDecrease_Class2,b_PopSmallDecrease_Class3)
  lcpars[["b_PopLargeDecrease"]] =list(b_PopLargeDecrease_Class1,b_PopLargeDecrease_Class2,b_PopLargeDecrease_Class3)
  lcpars[["b_PopSmallIncrease"]] =list( b_PopSmallIncrease_Class1,b_PopSmallIncrease_Class2,b_PopSmallIncrease_Class3)
  lcpars[["b_ManagementLethal"]] =list(b_ManagementLethal_Class1,b_ManagementLethal_Class2,b_ManagementLethal_Class3)
  lcpars[["b_ManagementDeterrent"]] = list( b_ManagementDeterrent_Class1,b_ManagementDeterrent_Class2,b_ManagementDeterrent_Class3)
  lcpars[["b_ManagementNothing"]] = list(b_ManagementNothing_Class1,b_ManagementNothing_Class2,b_ManagementNothing_Class3)
  
  V=list()
  V[["Class1"]] = delta_Class1+
    (gamma_info_Class1*info)+
    (gamma_country_Class1*country)
  V[["Class2"]] = delta_Class2+
    (gamma_info_Class2*info)+
    (gamma_country_Class2*country)
  V[["Class3"]] = delta_Class3
  
  classAlloc_settings = list(
    classes      = c(Class1=1, Class2=2,Class3=3), 
    utilities    = V
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}


apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities
  P = list()
  
  
  mnl_settings = list(
    alternatives  = c(A=1, B=2, C=3), 
    avail         = list(A=1, B=1, C=1), 
    choiceVar     = Alternative)
  
  ### Loop over classes
for(s in 1:length(pi_values)){
    V = list()
    V[["A"]]  = asc_A[[s]] 
    
    V[["B"]]  = (b_Cost[[s]] *(CostB))+
      (b_PopSmallDecrease[[s]]*(PopSmallDecreaseB==1))+
      (b_PopLargeDecrease[[s]]*(PopLargeDecreaseB==1))+
      (b_PopSmallIncrease[[s]]*(PopSmallIncreaseB==1))+
      (b_ManagementLethal[[s]]*(ManagementLethalB==1))+
      (b_ManagementDeterrent[[s]]*(ManagementDeterrentB==1))+
      (b_ManagementNothing[[s]]*(ManagementNothingB==1))
    
    V[["C"]]  = (b_Cost[[s]] *(CostC))+
      (b_PopSmallDecrease[[s]]*(PopSmallDecreaseC==1))+
      (b_PopLargeDecrease[[s]]*(PopLargeDecreaseC==1))+
      (b_PopSmallIncrease[[s]]*(PopSmallIncreaseC==1))+
      (b_ManagementLethal[[s]]*(ManagementLethalC==1))+
      (b_ManagementDeterrent[[s]]*(ManagementDeterrentC==1))+
      (b_ManagementNothing[[s]]*(ManagementNothingC==1))
    
    mnl_settings$utilities = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
  }
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28 = apollo_lcEM(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  lcEM_settings = list(EMMaxIterations = 100)
)


# Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28 = apollo_estimate(apollo_beta, apollo_fixed, 
#                                                                 apollo_probabilities, apollo_inputs)

apollo_modelOutput(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28,saveOutput_settings = list(printPVal=TRUE))

saveRDS(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28,"Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28.rds") 

# 
Model <- readRDS("Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28.rds")
# 
# 
Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_Conditional <- apollo_lcConditionals(model = Model, apollo_probabilities, apollo_inputs)
saveRDS(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_Conditional,"Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_Conditional.rds")

saveRDS(data.frame("LCM3ClassProb"=apply(round(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_Conditional[,2:4],4),1,which.max)),
        "Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_Classes.rds")

Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP <- apollo_lcUnconditionals(model = Model, apollo_probabilities, apollo_inputs)
saveRDS(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP,"Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP.rds")
# 
# 
# 
# #### Further Tests ####
# Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP
Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP <-readRDS("Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP.rds")
ClassProbs <- data.frame(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$pi_values)
PIValues <- (Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$pi_values)
Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$pi_values <- NULL
WTP_Class1 <- cbind("b_PopSmallDecrease"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_PopSmallDecrease[1]),
                    "b_PopLargeDecrease"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_PopLargeDecrease[1]),
                    "b_PopSmallIncrease"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_PopSmallIncrease[1]),
                    "b_ManagementLethal"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_ManagementLethal[1]),
                    "b_ManagementDeterrent"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_ManagementDeterrent[1]),
                    "b_ManagementNothing"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_ManagementNothing[1]),
                    "b_Cost"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_Cost[1]))


WTP_Class2 <- cbind("b_PopSmallDecrease"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_PopSmallDecrease[2]),
                    "b_PopLargeDecrease"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_PopLargeDecrease[2]),
                    "b_PopSmallIncrease"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_PopSmallIncrease[2]),
                    "b_ManagementLethal"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_ManagementLethal[2]),
                    "b_ManagementDeterrent"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_ManagementDeterrent[2]),
                    "b_ManagementNothing"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_ManagementNothing[2]),
                    "b_Cost"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_Cost[2]))


WTP_Class3 <- cbind("b_PopSmallDecrease"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_PopSmallDecrease[3]),
                    "b_PopLargeDecrease"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_PopLargeDecrease[3]),
                    "b_PopSmallIncrease"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_PopSmallIncrease[3]),
                    "b_ManagementLethal"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_ManagementLethal[3]),
                    "b_ManagementDeterrent"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_ManagementDeterrent[3]),
                    "b_ManagementNothing"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_ManagementNothing[3]),
                    "b_Cost"=as.numeric(Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_UCWTP$b_Cost[3]))



Model <- readRDS("Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28.rds")



LCM3C_SD_Class1 <- cbind("SD_PopSmallDecrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PopSmallDecrease_Class1/b_Cost_Class1")))["Robust s.e."]),
                         "SD_PopLargeDecrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PopLargeDecrease_Class1/b_Cost_Class1")))["Robust s.e."]),
                         "SD_PopSmallIncrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PopSmallIncrease_Class1/b_Cost_Class1")))["Robust s.e."]),
                         "SD_ManagementLethal"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_ManagementLethal_Class1/b_Cost_Class1")))["Robust s.e."]),
                         "SD_ManagementDeterrent"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_ManagementDeterrent_Class1/b_Cost_Class1")))["Robust s.e."]),
                         "SD_ManagementNothing"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_ManagementNothing_Class1/b_Cost_Class1")))["Robust s.e."]),
                         "SD_Cost"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Cost_Class1")))["Robust s.e."]))


LCM3C_SD_Class2 <- cbind("SD_PopSmallDecrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PopSmallDecrease_Class2/b_Cost_Class2")))["Robust s.e."]),
                         "SD_PopLargeDecrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PopLargeDecrease_Class2/b_Cost_Class2")))["Robust s.e."]),
                         "SD_PopSmallIncrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PopSmallIncrease_Class2/b_Cost_Class2")))["Robust s.e."]),
                         "SD_ManagementLethal"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_ManagementLethal_Class2/b_Cost_Class2")))["Robust s.e."]),
                         "SD_ManagementDeterrent"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_ManagementDeterrent_Class2/b_Cost_Class2")))["Robust s.e."]),
                         "SD_ManagementNothing"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_ManagementNothing_Class2/b_Cost_Class2")))["Robust s.e."]),
                         "SD_Cost"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Cost_Class2")))["Robust s.e."]))


LCM3C_SD_Class3 <- cbind("SD_PopSmallDecrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PopSmallDecrease_Class3/b_Cost_Class3")))["Robust s.e."]),
                         "SD_PopLargeDecrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PopLargeDecrease_Class3/b_Cost_Class3")))["Robust s.e."]),
                         "SD_PopSmallIncrease"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_PopSmallIncrease_Class3/b_Cost_Class3")))["Robust s.e."]),
                         "SD_ManagementLethal"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_ManagementLethal_Class3/b_Cost_Class3")))["Robust s.e."]),
                         "SD_ManagementDeterrent"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_ManagementDeterrent_Class3/b_Cost_Class3")))["Robust s.e."]),
                         "SD_ManagementNothing"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_ManagementNothing_Class3/b_Cost_Class3")))["Robust s.e."]),
                         "SD_Cost"=data.frame(apollo_deltaMethod(Model,deltaMethod_settings = list(expression=c("b_Cost_Class3")))["Robust s.e."]))


ThreeClassSD <-rbind(LCM3C_SD_Class1,
                     LCM3C_SD_Class2,
                     LCM3C_SD_Class3)

rownames(ThreeClassSD) <- c("Class1SD","Class2SD","Class3SD")
ThreeClassSD <- cbind(ThreeClassSD,data.frame("Class"=c("Class1","Class2","Class3")))
colnames(ThreeClassSD)<-    c("PopSmallDecrease",
                              "PopLargeDecrease",
                              "PopSmallIncrease",
                              "ManagementLethal",
                              "ManagementDeterrent",
                              "ManagementNothing",
                              "Cost","Class")





ThreeClassWTP <-rbind(round(-1*(WTP_Class1/WTP_Class1[7]),3),
                      round(-1*(WTP_Class2/WTP_Class2[7]),3),
                      round(-1*(WTP_Class3/WTP_Class3[7]),3))

rownames(ThreeClassWTP) <- c("Class1","Class2","Class3")
ThreeClassWTP <- cbind(ThreeClassWTP[, 1:6], data.frame("Cost" = rbind(WTP_Class1[7], WTP_Class2[7], WTP_Class3[7])))
ThreeClassWTP <- cbind(ThreeClassWTP,data.frame("Class"=c("Class1","Class2","Class3")))
colnames(ThreeClassWTP)<-    c("PopSmallDecrease",
                               "PopLargeDecrease",
                               "PopSmallIncrease",
                               "ManagementLethal",
                               "ManagementDeterrent",
                               "ManagementNothing",
                               "Cost","Class")



MeanAndSD_3C<- cbind(melt(ThreeClassWTP,id.vars="Class"),melt(ThreeClassSD,id.vars="Class"))
MeanAndSD_3C <- data.frame(cbind(MeanAndSD_3C[,1:3],"SD"=MeanAndSD_3C[,6]))




ggsave(
  ggplot(data=MeanAndSD_3C,aes(x=variable,y=value,fill=variable))+
    geom_bar(stat="identity")+
    geom_text(aes(label=round(value,2),y=value/2),colour="black")+
    geom_errorbar(aes(ymin = value - SD, ymax = value + SD)) +
    facet_wrap( ~ Class,scales="free_x",labeller = as_labeller(
      c(FacetLabels)))+scale_fill_manual(
        "Attributes",
        values = c("PopSmallDecrease" = "dodgerblue1",
                   "PopLargeDecrease" = "dodgerblue4",
                   "PopSmallIncrease" = "blue4",
                   "ManagementLethal" = "red4",
                   "ManagementDeterrent" = "gold3",
                   "ManagementNothing" = "springgreen4",
                   "Cost" = "darkgrey"
        )
      ) + ggtitle("3-Class Model: Attribute WTP.") + 
    ylab("Mean Attribute WTP in #py") +
    theme(legend.position = "bottom") +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))+
    coord_flip()+
    geom_hline(yintercept = 0),
  device = "jpeg",
  filename = "Parakeets_LCM_3Class_AllCovariates_EM_2022_03_28_BarHError.jpeg",
  width=30,height=25,units = "cm",dpi=500)