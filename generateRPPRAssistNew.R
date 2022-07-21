# function: generateRMR()
# input: site
# output: table per site

# rm(list=ls())
# setwd(getwd())

demo_new <- read.csv('CAPR+Demographics_killme.csv')

# select columns of interest
demo_dat_cleaned <- demo_new[-c(1:2),c(11,13,63,66:67)]



colnames(demo_dat_cleaned) <- c("race",
                                "ethnicity", # 4 - Not of Hispanic or Latino, 1 - Of Hispanic or Latino
                                "age",
                                "site",
                                "gender")
# recode race
demo_dat_cleaned$race <- ifelse(demo_dat_cleaned$race == 1,"race_asian",
                                ifelse(demo_dat_cleaned$race == 2,"race_alaska_native",
                                       ifelse(demo_dat_cleaned$race == 3,"race_american_indian",
                                              ifelse(demo_dat_cleaned$race == 4,"race_black_or_african_american",
                                                     ifelse(demo_dat_cleaned$race == 5,"race_white_or_caucasian",
                                                            ifelse(demo_dat_cleaned$race == 6, "race_native_hawaiian_or_other_pacific_islander",
                                                                   ifelse(nchar(demo_dat_cleaned$race) >= 3, "More Than Once Race", "Unknown or Not Reported")))))))
# recode hispanic
demo_dat_cleaned$ethnicity <- ifelse(demo_dat_cleaned$ethnicity == 4,"Not of Hispanic or Latino",
                                    ifelse(demo_dat_cleaned$ethnicity == 1, "Of Hispanic or Latino",""))


# months column
demo_dat_cleaned["age_unit"] <- "Months"


# levels(demo_dat_cleaned$race) <- c("American Indian",
#                                    "Asian",
#                                    "Hawaiian",
#                                    "Black",
#                                    "White",
#                                    "More than one race",
#                                    "Unknown"
# )

generateRPPRassist <- function(site){
  # site <-"UMBC"
  # organize data in long format for summary
  
  if (site == "UMBC" | site == "UCI") {
    df <- demo_dat_cleaned[(demo_dat_cleaned$site == "UMBC" | demo_dat_cleaned$site == "UCI"),]
  } else if (site == "UGA" | site == "Emory") {
    df <- demo_dat_cleaned[(demo_dat_cleaned$site == "UGA" | demo_dat_cleaned$site == "Emory"),]
  } else {
    df <- demo_dat_cleaned[(demo_dat_cleaned$site == site),]
  }
  
  
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
