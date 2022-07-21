library(qualtRics)


# sensitive info for api key
source("secrets.R")

# store qualtrics API credentials
qualtRics::qualtrics_api_credentials(api_key = apiKey, 
                                     base_url = baseUrl,
                                     install = TRUE,
                                     overwrite = TRUE)

# return all surveyIds into dataframe
# surveys <- all_surveys() 

surveyId <- "SV_9nK9whLLeyk4jMF"

# create dataframe from capr demographics from qualtrics API
demo <- fetch_survey(surveyID = surveyId,
                     verbose = FALSE,
                     label = FALSE, # both of these must be set to false to import numeric
                     convert = FALSE, # both of these must be set to false to import numeric
                     force_request = TRUE)