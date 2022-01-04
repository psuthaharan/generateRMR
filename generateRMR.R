# function: generateRMR()
# input: site
# output: table per site

# rm(list=ls())
# setwd(getwd())

library(dplyr)
library(rstatix)

demo <- read.csv('CAPR+Demographics.csv')

demo_dat <- demo[,c(11:16,18,73:74)]
demo_dat_cleaned <- demo_dat[-c(1:2),]
colnames(demo_dat_cleaned) <- c("race_asian",
                                "race_alaska_native",
                                "race_american_indian",
                                "race_black_or_african_american",
                                "race_white_or_caucasian",
                                "race_native_hawaiian_or_other_pacific_islander",
                                "hispanic", # 4 - Not of Hispanic or Latino, 1 - Of Hispanic or Latino
                                "site",
                                "sex_at_birth_omnibus")
# recode hispanic
demo_dat_cleaned$hispanic <- ifelse(demo_dat_cleaned$hispanic == 4,"Not of Hispanic or Latino",
                                               ifelse(demo_dat_cleaned$hispanic == 1, "Of Hispanic or Latino",""))


demo_dat_cleaned$count <- rowSums(as.data.frame(sapply(demo_dat_cleaned[, grepl("race_", names(demo_dat_cleaned))], as.numeric)), na.rm = TRUE)

# if count = 1, then grab single race column
# else if count > 1, then label as "More Than One Race"
# else label as "Unknown or Not Reported"

race_vector <- c()

for (i in 1:nrow(demo_dat_cleaned)){
race_vector[i] <- ifelse(demo_dat_cleaned[i,]$count == 1,colnames(demo_dat_cleaned)[1:6][which.max(demo_dat_cleaned[i,1:6])],
                                ifelse(demo_dat_cleaned[i,]$count > 1, "More Than One Race","Unknown or Not Reported"))
}

demo_dat_cleaned$race <- race_vector
demo_dat_cleaned$race <- as.factor(demo_dat_cleaned$race)
demo_dat_cleaned$race <- factor(demo_dat_cleaned$race, levels = c("race_american_indian",
                                                                  "race_asian",
                                                                  "race_native_hawaiian_or_other_pacific_islander",
                                                                  "race_black_or_african_american",
                                                                  "race_white_or_caucasian",
                                                                  "More Than One Race",
                                                                  "Unknown or Not Reported"
                                                                  ))


levels(demo_dat_cleaned$race) <- c("American Indian",
                                   "Asian",
                                   "Native Hawaiian or Other Pacific Islander",
                                   "Black or African American",
                                   "White",
                                   "More Than Once Race",
                                   "Unknown or Not Reported"
                                   )


generateRMR <- function(site){
  
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
                                        racial_minority = sum(race_summary$count) - sum(race_summary[which(race_summary$race == "White"),]$count),
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
