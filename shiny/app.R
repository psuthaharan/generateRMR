rm(list=ls())
setwd(getwd())

library(shiny)

# load dependencies
source("../generateRMR.R")
source("../generateRPPR.R")

source("server.R")
source("ui.R")

# main
# generateRMR(site = "Yale")
# generateRMR(site = "Emory")
# generateRMR(site = "NU")
# generateRMR(site = "Temple")
# generateRMR(site = "UCI")
# generateRMR(site = "UGA")
# generateRMR(site = "UMBC")

# generateRPPR(site = "Yale")
yale_df <- generateRPPR(site = "Yale")
# generateRPPR(site = "Emory")
emory_df <- generateRPPR(site = "Emory")
# generateRPPR(site = "NU")
nu_df<- generateRPPR(site = "NU")
# generateRPPR(site = "Temple")
temple_df <- generateRPPR(site = "Temple")
# generateRPPR(site = "UCI")
uci_df <- generateRPPR(site = "UCI")
# generateRPPR(site = "UGA")
uga_df <- generateRPPR(site = "UGA")
# generateRPPR(site = "UMBC")
umbc_df <- generateRPPR(site = "UMBC")

shinyApp(ui = ui, server = server)

