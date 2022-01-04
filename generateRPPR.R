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

generateRPPR <- function(site){

  # organize data in long format for summary
  df <- demo_dat_cleaned[which(demo_dat_cleaned$site == site),] %>%
    gather(key = "race", value = "count", race_asian, race_alaska_native, race_american_indian, race_black_or_african_american, race_white_or_caucasian, race_native_hawaiian_or_other_pacific_islander) %>%
    convert_as_factor(race,hispanic,sex_at_birth_omnibus)
  
  # filter rows with counts
  df <- df[which(df$count == 1),]
  
  # summarize counts
  race_summary <- df %>%
    group_by(race,hispanic,sex_at_birth_omnibus) %>%
    summarise(count = n())
  
  colnames(race_summary) <- c("race","hispanic","sex","count")
  #print(1)
  
  ### format to Vijay's pretty table
  
  # View(race_summary)
  
  a1 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  b1 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  c1 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  d1 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Of Hispanic or Latino'),])
  e1 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Of Hispanic or Latino'),])
  f1 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Of Hispanic or Latino'),])
  g1 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  h1 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  i1 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])

  
  a2 <- nrow(df[which(df$race == 'race_asian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  b2 <- nrow(df[which(df$race == 'race_asian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  c2 <- nrow(df[which(df$race == 'race_asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  d2 <- nrow(df[which(df$race == 'race_asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Of Hispanic or Latino'),])
  e2 <- nrow(df[which(df$race == 'race_asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Of Hispanic or Latino'),])
  f2 <- nrow(df[which(df$race == 'race_asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Of Hispanic or Latino'),])
  g2 <- nrow(df[which(df$race == 'race_asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  h2 <- nrow(df[which(df$race == 'race_asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  i2 <- nrow(df[which(df$race == 'race_asian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  a3 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  b3 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  c3 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  d3 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Of Hispanic or Latino'),])
  e3 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Of Hispanic or Latino'),])
  f3 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Of Hispanic or Latino'),])
  g3 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  h3 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  i3 <- nrow(df[which(df$race == 'race_american_indian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  a4 <- nrow(df[which(df$race == 'race_black_or_african_american' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  b4 <- nrow(df[which(df$race == 'race_black_or_african_american' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  c4 <- nrow(df[which(df$race == 'race_black_or_african_american' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  d4 <- nrow(df[which(df$race == 'race_black_or_african_american' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Of Hispanic or Latino'),])
  e4 <- nrow(df[which(df$race == 'race_black_or_african_american' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Of Hispanic or Latino'),])
  f4 <- nrow(df[which(df$race == 'race_black_or_african_american' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Of Hispanic or Latino'),])
  g4 <- nrow(df[which(df$race == 'race_black_or_african_american' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  h4 <- nrow(df[which(df$race == 'race_black_or_african_american' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  i4 <- nrow(df[which(df$race == 'race_black_or_african_american' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  a5 <- nrow(df[which(df$race == 'race_white_or_caucasian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  b5 <- nrow(df[which(df$race == 'race_white_or_caucasian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  c5 <- nrow(df[which(df$race == 'race_white_or_caucasian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  d5 <- nrow(df[which(df$race == 'race_white_or_caucasian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Of Hispanic or Latino'),])
  e5 <- nrow(df[which(df$race == 'race_white_or_caucasian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Of Hispanic or Latino'),])
  f5 <- nrow(df[which(df$race == 'race_white_or_caucasian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Of Hispanic or Latino'),])
  g5 <- nrow(df[which(df$race == 'race_white_or_caucasian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  h5 <- nrow(df[which(df$race == 'race_white_or_caucasian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  i5 <- nrow(df[which(df$race == 'race_white_or_caucasian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])

  # b6 <- nrow(df[which(df$race=='asian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  # c6 <- nrow(df[which(df$race=='asian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  # d6 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  # e6 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Of Hispanic or Latino'),])
  # f6 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Of Hispanic or Latino'),])
  # g6 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Of Hispanic or Latino'),])
  # h6 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  # i6 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  # j6 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])

  # b7 <- nrow(df[which(df$race=='asian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  # c7 <- nrow(df[which(df$race=='asian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  # d7 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  # e7 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Of Hispanic or Latino'),])
  # f7 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Of Hispanic or Latino'),])
  # g7 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Of Hispanic or Latino'),])
  # h7 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  # i7 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  # j7 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  # b8 <- nrow(df[which(df$race=='asian' & df$sex_at_birth_omnibus=='F' & df$hispanic=='Not of Hispanic or Latino'),])
  # c8 <- nrow(df[which(df$race=='asian' & df$sex_at_birth_omnibus=='M' & df$hispanic=='Not of Hispanic or Latino'),])
  # d8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  # e8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Of Hispanic or Latino'),])
  # f8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Of Hispanic or Latino'),])
  # g8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Of Hispanic or Latino'),])
  # h8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  # i8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  # j8 <- nrow(df[which(df$race == 'asian' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  table <- data.frame(xxx = c('','','','',''),
                      female_not_hispanic = c(a1,a2,a3,a4,a5),
                      male_not_hispanic = c(b1,b2,b3,b4,b5),
                      not_reported_not_hispanic = c(c1,c2,c3,c4,c5),
                      female_hispanic = c(d1,d2,d3,d4,d5),
                      male_hispanic = c(e1,e2,e3,e4,e5),
                      not_reported_hispanic = c(f1,f2,f3,f4,f5),
                      female_unknown = c(g1,g2,g3,g4,g5),
                      male_unknown = c(h1,h2,h3,h4,h5),
                      not_reported_unknown = c(i1,i2,i3,i4,i5))
  

  
  row.names(table) <- c("American Indian","Asian","Native Hawaiian or Pacific Islander",
                        "Black or African American","White")
  # ,"More Than One Race","Unknown or Not Reported","Totals")
  
  
  # b3 <- race_summary[which(race_summary$race=='american_indian' && race_summary$sex=='F' && race_summary$hispanic=='Not of Hispanic or Latino'),]$count
  # c3 <- race_summary[which(race_summary$race=='american_indian' && race_summary$sex=='M' && race_summary$hispanic=='Not of Hispanic or Latino'),]$count
  # 
  
  
  return(rppr_table = table)
  # site = "Yale"
  # write.csv(race_summary,paste('rppr/rppr_',site,'.csv'),row.names = FALSE)

  #write.csv(table,paste('rppr/rppr_',site,'.csv'),row.names = TRUE)
  
  
}
