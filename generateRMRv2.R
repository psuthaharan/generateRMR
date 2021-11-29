# function: generateRMR()
# input: site
# output: table per site

rm(list=ls())
setwd(getwd())

library(dplyr)
library(rstatix)

demo <- read.csv('CAPR+Demographics.csv')

#demo_dat <- demo[, grepl("Demog.1_", names(demo))]
#demo_dat <- demo[,c(12:16,19,72:73)]
demo_dat <- demo[,c(11:16,18,73:74)]
demo_dat_cleaned <- demo_dat[-c(1:2),]
colnames(demo_dat_cleaned) <- c("asian",
                                "alaska_native",
                                "american_indian",
                                "black_or_african_american",
                                "white_or_caucasian",
                                "native_hawaiian_or_other_pacific_islander",
                                "hispanic", # 4 - Not of Hispanic or Latino, 1 - Of Hispanic or Latino
                                #"sex_at_birth_qualtrics",
                                #"src_subject_id",
                                "site",
                                "sex_at_birth_omnibus")
# recode hispanic
demo_dat_cleaned$hispanic <- ifelse(demo_dat_cleaned$hispanic == 4,"Not of Hispanic or Latino",
                                               ifelse(demo_dat_cleaned$hispanic == 1, "Of Hispanic or Latino",""))


# integrity check - checking if qualtrics sex aligns with omnibus sex
# demo_dat_cleaned$sex_integrity_check <- ifelse(demo_dat_cleaned$sex_at_birth_qualtrics == 1 & demo_dat_cleaned$sex_at_birth_omnibus == "M","TRUE",
#                                                ifelse(demo_dat_cleaned$sex_at_birth_qualtrics == 2 & demo_dat_cleaned$sex_at_birth_omnibus == "F","TRUE","FALSE"))


generateRMR <- function(site){
  
  # organize data in long format for summary
  df <- demo_dat_cleaned[which(demo_dat_cleaned$site == site),] %>%
    gather(key = "race", value = "count", asian, alaska_native, american_indian, black_or_african_american, white_or_caucasian, native_hawaiian_or_other_pacific_islander) %>%
    convert_as_factor(race,hispanic,sex_at_birth_omnibus)
  
  # filter rows with counts
  df <- df[which(df$count == 1),]
  
  # summarize counts
  race_summary <- df %>%
    group_by(race,hispanic,sex_at_birth_omnibus) %>%
    summarise(count = n())
  
  colnames(race_summary) <- c("race","hispanic","sex","count")
  
  
  #return(race_summary)
  write.csv(race_summary,paste('rmr/rmr_',site,'.csv'))
  
}
