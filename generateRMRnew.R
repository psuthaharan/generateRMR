# function: generateRMR()
# input: site
# output: table per site

#rm(list=ls())
# setwd(getwd())

library(dplyr)
library(rstatix)



demo_new <- read.csv('CAPR+Demographics_killme.csv')

# select columns of interest
demo_dat_cleaned <- demo_new[-c(1:2),c(11,13,66:67)]



colnames(demo_dat_cleaned) <- c("race",
                                "hispanic", # 4 - Not of Hispanic or Latino, 1 - Of Hispanic or Latino
                                "site",
                                "sex_at_birth_omnibus")
# recode race
demo_dat_cleaned$race <- ifelse(demo_dat_cleaned$race == 1,"race_asian",
                                ifelse(demo_dat_cleaned$race == 2,"race_alaska_native",
                                       ifelse(demo_dat_cleaned$race == 3,"race_american_indian",
                                              ifelse(demo_dat_cleaned$race == 4,"race_black_or_african_american",
                                                     ifelse(demo_dat_cleaned$race == 5,"race_white_or_caucasian",
                                    ifelse(demo_dat_cleaned$race == 6, "race_native_hawaiian_or_other_pacific_islander",
                                           ifelse(nchar(demo_dat_cleaned$race) >= 3, "More Than Once Race", "Unknown or Not Reported")))))))
# recode hispanic
demo_dat_cleaned$hispanic <- ifelse(demo_dat_cleaned$hispanic == 4,"Not of Hispanic or Latino",
                                               ifelse(demo_dat_cleaned$hispanic == 1, "Of Hispanic or Latino",""))

generateRMR <- function(site) {
  
  # organize data in long format for summary
  # df <- demo_dat_cleaned[which(demo_dat_cleaned$site == site),] %>%
  #   gather(key = "race", value = "count", asian, alaska_native, american_indian, black_or_african_american, white_or_caucasian, native_hawaiian_or_other_pacific_islander) %>%
  #   convert_as_factor(race,hispanic,sex_at_birth_omnibus)
  # 
  # # filter rows with counts
  # df <- df[which(df$count == 1),]
  
  # summarize counts
  race_summary <- demo_dat_cleaned[which(demo_dat_cleaned$site == site),]  %>% # pipe
    group_by(race,hispanic,sex_at_birth_omnibus) %>%
    summarise(count = n())
  
  colnames(race_summary) <- c("race","hispanic","sex","count")
  #View(race_summary)
  
  ## calculate
  # 1) Actual: Total Recruitment
  # 2) Actual: Racial Minority Recruitment
  # 3) Actual: Hispanic Ethnicity Recruitment
  


  actual_recruitment_df <- t(data_frame(total_recruitment = sum(race_summary$count),
                                        racial_minority = sum(race_summary$count) - sum(race_summary[which(race_summary$race == "race_white_or_caucasian"),]$count),
                                        hispanic_ethnicity = sum(race_summary[which(race_summary$hispanic == "Of Hispanic or Latino"),]$count)
                                        )
                             )

  colnames(actual_recruitment_df) <- c("count")
  row.names(actual_recruitment_df) <- c("Actual: Total Recruitment","Actual: Racial Minority Recruitment", "Actual: Hispanic Ethnicity Recruitment")

  
  #return(race_summary)
  ## we need this for the annual reporting
  #write.csv(race_summary,paste('rmr/rmr_',tolower(site),'.csv'),row.names = FALSE)
  write.csv(actual_recruitment_df,paste('rmr/',tolower(site),'_',Sys.Date(),'.csv', sep = ''), row.names = TRUE)
  
  
}
