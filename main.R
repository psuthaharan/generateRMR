rm(list=ls())
setwd(getwd())

# load dependencies
source("generateRMR.R")
source("generateRPPR.R")

# main
# generateRMR(site = "Yale")
# generateRMR(site = "Emory")
# generateRMR(site = "NU")
# generateRMR(site = "Temple")
# generateRMR(site = "UCI")
# generateRMR(site = "UGA")
# generateRMR(site = "UMBC")


yale_df <- generateRPPR(site = "Yale")
emory_df <- generateRPPR(site = "Emory")
nu_df<- generateRPPR(site = "NU")
temple_df <- generateRPPR(site = "Temple")
uci_df <- generateRPPR(site = "UCI")
uga_df <- generateRPPR(site = "UGA")
umbc_df <- generateRPPR(site = "UMBC")
