# function: generateRMR()
# input: site
# output: table per site

rm(list=ls())
setwd(getwd())

library(dplyr)
library(rstatix)
library(xlsx)

demo <- read.csv('CAPR+Demographics.csv')

#demo_dat <- demo[, grepl("Demog.1_", names(demo))]
#demo_dat <- demo[,c(12:16,19,72:73)]
demo_dat <- demo[,c(11:16,18,70,73:74)]
demo_dat_cleaned <- demo_dat[-c(1:2),]
colnames(demo_dat_cleaned) <- c("race_asian",
                                "race_alaska_native",
                                "race_american_indian",
                                "race_black_or_african_american",
                                "race_white_or_caucasian",
                                "race_native_hawaiian_or_other_pacific_islander",
                                "hispanic", # 4 - Not Hispanic or Latino, 1 - Hispanic or Latino
                                "interview_age",
                                "site",
                                "sex_at_birth_omnibus")
# recode hispanic
demo_dat_cleaned$hispanic <- ifelse(demo_dat_cleaned$hispanic == 4,"Not Hispanic or Latino",
                                    ifelse(demo_dat_cleaned$hispanic == 1, "Hispanic or Latino",""))


demo_dat_cleaned$count <- rowSums(as.data.frame(sapply(demo_dat_cleaned[, grepl("race_", names(demo_dat_cleaned))], as.numeric)), na.rm = TRUE)

# if count = 1, then grab single race column
# else if count > 1, then label as "More Than One Race"
# else label as "Unknown or Not Reported"

race_vector <- c()

for (i in 1:nrow(demo_dat_cleaned)){
  race_vector[i] <- ifelse(demo_dat_cleaned[i,]$count == 1,colnames(demo_dat_cleaned)[1:6][which.max(demo_dat_cleaned[i,1:6])],
                           ifelse(demo_dat_cleaned[i,]$count > 1, "More than one race","Unknown"))
}

demo_dat_cleaned$race <- race_vector
demo_dat_cleaned$race <- as.factor(demo_dat_cleaned$race)
demo_dat_cleaned$race <- factor(demo_dat_cleaned$race, levels = c("race_american_indian",
                                                                  "race_asian",
                                                                  "race_native_hawaiian_or_other_pacific_islander",
                                                                  "race_black_or_african_american",
                                                                  "race_white_or_caucasian",
                                                                  "More than one race",
                                                                  "Unknown"
))

# add race composite race columns
demo_dat_cleaned["race_more_than_one_race"] <- 0
demo_dat_cleaned["race_unknown_or_not_reported"] <- 0

# months column
demo_dat_cleaned["age_unit"] <- "Months"

# iterate over data frame to remove multiple races and add to composite race columns
for (i in 1:nrow(demo_dat_cleaned)){
  if (demo_dat_cleaned[i,]$race == "More than one race") {
    demo_dat_cleaned[i,]$race_american_indian = ''
    demo_dat_cleaned[i,]$race_asian = ''
    demo_dat_cleaned[i,]$race_native_hawaiian_or_other_pacific_islander = ''
    demo_dat_cleaned[i,]$race_black_or_african_american = ''
    demo_dat_cleaned[i,]$race_white_or_caucasian = ''
    demo_dat_cleaned[i,]$race_more_than_one_race = 1
  } else if (demo_dat_cleaned[i,]$race == "Unknown"){
    demo_dat_cleaned[i,]$race_unknown_or_not_reported = 1
  }

}


levels(demo_dat_cleaned$race) <- c("American Indian",
                                   "Asian",
                                   "Hawaiian",
                                   "Black",
                                   "White",
                                   "More than one race",
                                   "Unknown"
)

generateRPPRassist <- function(site){
  # site <-"UMBC"
  # organize data in long format for summary
  
  if (site == "UMBC" | site == "UCI") {
    df <- demo_dat_cleaned[which(demo_dat_cleaned$site == "UMBC" | demo_dat_cleaned$site == "UCI"),] %>%
      gather(key = "race", value = "count", race_asian, race_alaska_native, race_american_indian, race_black_or_african_american, race_white_or_caucasian, race_native_hawaiian_or_other_pacific_islander,race_more_than_one_race,race_unknown_or_not_reported) %>%
      convert_as_factor(race,hispanic,sex_at_birth_omnibus)
  } else if (site == "UGA" | site == "Emory") {
    df <- demo_dat_cleaned[which(demo_dat_cleaned$site == "UGA" | demo_dat_cleaned$site == "Emory"),] %>%
      gather(key = "race", value = "count", race_asian, race_alaska_native, race_american_indian, race_black_or_african_american, race_white_or_caucasian, race_native_hawaiian_or_other_pacific_islander,race_more_than_one_race,race_unknown_or_not_reported) %>%
      convert_as_factor(race,hispanic,sex_at_birth_omnibus)
  } else {
  df <- demo_dat_cleaned[which(demo_dat_cleaned$site == site),] %>%
    gather(key = "race", value = "count", race_asian, race_alaska_native, race_american_indian, race_black_or_african_american, race_white_or_caucasian, race_native_hawaiian_or_other_pacific_islander,race_more_than_one_race,race_unknown_or_not_reported) %>%
    convert_as_factor(race,hispanic,sex_at_birth_omnibus)
  }
  
  # filter rows with counts
  df <- df[which(df$count == 1),]
  
  # new = old (backwards!)
  df <- rename(df, ethnicity = hispanic)
  df <- rename(df, gender = sex_at_birth_omnibus)
  df <- rename(df, age = interview_age)
  
  
  # remove count column
  df <- subset(df, select = -c(7))
  
  
 # rename values in 'race' column
  df$race <- ifelse(df$race == 'race_asian', 'Asian',
                    ifelse(df$race == 'race_american_indian','American Indian',
                           ifelse(df$race == 'race_black_or_african_american', 'Black',
                                  ifelse(df$race == 'race_white_or_caucasian', 'White',
                                         ifelse(df$race == 'race_native_hawaiian_or_other_pacific_islander', 'Hawaiian',
                                                ifelse(df$race == 'race_more_than_one_race', 'More than one race',
                                                       ifelse(df$race == 'race_unknown_or_not_reported', 'Unknown','')))))))

  # rename values in 'gender' column
  df$gender <- ifelse(df$gender == 'M', 'Male',
                    ifelse(df$gender == 'F','Female',''))  
  
  

  
  if (site == "UMBC" | site == "UCI") {
    # remove site column and reorder remaining columns
    df = subset(df, select = c(race,ethnicity,gender,age,age_unit) )
    write.csv(df,paste('assist/','UMBC & UCI','.csv',sep=''),row.names = FALSE)
  } else if (site == "UGA" | site == "Emory") {
    # remove site column and reorder remaining columns
    df = subset(df, select = c(race,ethnicity,gender,age,age_unit) )
    write.csv(df,paste('assist/','UGA & Emory','.csv',sep=''),row.names = FALSE)
  } else {
    # remove site column and reorder remaining columns
    df = subset(df, select = c(race,ethnicity,gender,age,age_unit) )
    write.csv(df,paste('assist/',site,'.csv',sep=''),row.names = FALSE)
  }

  
  return(rppr_assist = df)

  
  
}
