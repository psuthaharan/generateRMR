rm(list=ls())
setwd(getwd())

# load libs
library(dplyr)
library(rstatix)
library(xlsx)

# load demo into df via API
source("getSurvey.R")

# create RMR extracts
source("generateRMR.R")
generateRMR(site = "Yale")
generateRMR(site = "Emory")
generateRMR(site = "NU")
generateRMR(site = "Temple")
generateRMR(site = "UCI")
generateRMR(site = "UGA")
generateRMR(site = "UMBC")

# create RPPR extracts
source("generateRPPRAssist.R")
yale_assist_df <- generateRPPRassist(site = "Yale")
uga_emory_assist_df <- generateRPPRassist(site = "Emory")
nu_assist_df<- generateRPPRassist(site = "NU")
temple_assist_df <- generateRPPRassist(site = "Temple")
umbc_uci_assist_df <- generateRPPRassist(site = "UCI")
uga_emory_assist_df <- generateRPPRassist(site = "UGA")
umbc_uci_assist_df <- generateRPPRassist(site = "UMBC")



# yale_df <- generateRPPR(site = "Yale")
# uga_emory_df <- generateRPPR(site = "Emory")
# nu_df<- generateRPPR(site = "NU")
# temple_df <- generateRPPR(site = "Temple")
# umbc_uci_df <- generateRPPR(site = "UCI")
# uga_emory_df <- generateRPPR(site = "UGA")
# umbc_uci_df <- generateRPPR(site = "UMBC")
