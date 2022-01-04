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


generateRPPR <- function(site){
  
  # organize data in long format for summary
  df <- demo_dat_cleaned[which(demo_dat_cleaned$site == "Yale"),] %>%
    gather(key = "race", value = "count", asian, alaska_native, american_indian, black_or_african_american, white_or_caucasian, native_hawaiian_or_other_pacific_islander) %>%
    convert_as_factor(race,hispanic,sex_at_birth_omnibus)
  
  # filter rows with counts
  df <- df[which(df$count == 1),]
  
  # summarize counts
  race_summary <- df %>%
    group_by(race,hispanic,sex_at_birth_omnibus) %>%
    summarise(count = n())
  
  colnames(race_summary) <- c("race","hispanic","sex","count")
  
  ### format to Vijay's pretty table
  
  # View(race_summary)
  
  b3 <- nrow(df[which(df$race=='american_indian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  c3 <- nrow(df[which(df$race=='american_indian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  d3 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  e3 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Hispanic or Latino'),])
  f3 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Hispanic or Latino'),])
  g3 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Hispanic or Latino'),])
  h3 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  i3 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  j3 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  
  b4 <- nrow(df[which(df$race=='asian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  c4 <- nrow(df[which(df$race=='asian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  d4 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  e4 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Hispanic or Latino'),])
  f4 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Hispanic or Latino'),])
  g4 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Hispanic or Latino'),])
  h4 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  i4 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  j4 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  b5 <- nrow(df[which(df$race=='american_indian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  c5 <- nrow(df[which(df$race=='american_indian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  d5 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  e5 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Hispanic or Latino'),])
  f5 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Hispanic or Latino'),])
  g5 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Hispanic or Latino'),])
  h5 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  i5 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  j5 <- nrow(df[which(df$race == 'american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  b6 <- nrow(df[which(df$race=='black_or_african_american' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  c6 <- nrow(df[which(df$race=='black_or_african_american' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  d6 <- nrow(df[which(df$race == 'black_or_african_american' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  e6 <- nrow(df[which(df$race == 'black_or_african_american' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Hispanic or Latino'),])
  f6 <- nrow(df[which(df$race == 'black_or_african_american' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Hispanic or Latino'),])
  g6 <- nrow(df[which(df$race == 'black_or_african_american' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Hispanic or Latino'),])
  h6 <- nrow(df[which(df$race == 'black_or_african_american' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  i6 <- nrow(df[which(df$race == 'black_or_african_american' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  j6 <- nrow(df[which(df$race == 'black_or_african_american' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  b7 <- nrow(df[which(df$race=='white_or_caucasian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  c7 <- nrow(df[which(df$race=='white_or_caucasian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  d7 <- nrow(df[which(df$race == 'white_or_caucasian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  e7 <- nrow(df[which(df$race == 'white_or_caucasian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Hispanic or Latino'),])
  f7 <- nrow(df[which(df$race == 'white_or_caucasian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Hispanic or Latino'),])
  g7 <- nrow(df[which(df$race == 'white_or_caucasian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Hispanic or Latino'),])
  h7 <- nrow(df[which(df$race == 'white_or_caucasian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  i7 <- nrow(df[which(df$race == 'white_or_caucasian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  j7 <- nrow(df[which(df$race == 'white_or_caucasian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  # b8 <- nrow(df[which(df$race=='asian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  # c8 <- nrow(df[which(df$race=='asian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  # d8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  # e8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Hispanic or Latino'),])
  # f8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Hispanic or Latino'),])
  # g8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Hispanic or Latino'),])
  # h8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  # i8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  # j8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  table <- data.frame(xxx = c('','','','',''),
                      female_not_hispanic = c(b3,b4,b5,b6,b7),
                      male_not_hispanic = c(c3,c4,c5,c6,c7),
                      not_reported_not_hispanic = c(d3,d4,d5,d6,d7),
                      female_hispanic = c(e3,e4,e5,e6,e7),
                      male_hispanic = c(f3,f4,f5,f6,f7),
                      not_reported_hispanic = c(g3,g4,g5,g6,g7),
                      female_unknown = c(h3,h4,h5,h6,h7),
                      male_unknown = c(i3,i4,i5,i6,i7),
                      not_reported_unknown = c(j3,j4,j5,j6,j7))
  
  row.names(table) <- c("American Indian","Asian","Native Hawaiian or Pacific Islander",
                        "Black or African American","White")
  # ,"More Than One Race","Unknown or Not Reported","Totals")
  
  
  # b3 <- race_summary[which(race_summary$race=='american_indian' && race_summary$sex=='F' && race_summary$hispanic=='Not of Hispanic or Latino'),]$count
  # c3 <- race_summary[which(race_summary$race=='american_indian' && race_summary$sex=='M' && race_summary$hispanic=='Not of Hispanic or Latino'),]$count
  # 
  
  
  #return(race_summary)
  # site = "Yale"
  # write.csv(race_summary,paste('rppr/rppr_',site,'.csv'),row.names = FALSE)

  write.csv(table,paste('rppr/rppr_',site,'.csv'),row.names = TRUE)
  
  
}
