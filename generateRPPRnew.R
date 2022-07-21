# function: generateRPPR()
# input: site
# output: table per site

# rm(list=ls())
# setwd(getwd())

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
                                                                   ifelse(nchar(demo_dat_cleaned$race) >= 3, "race_more_than_one_race", "race_unknown_or_not_reported")))))))
# recode hispanic
demo_dat_cleaned$hispanic <- ifelse(demo_dat_cleaned$hispanic == 4,"Not of Hispanic or Latino",
                                    ifelse(demo_dat_cleaned$hispanic == 1, "Of Hispanic or Latino",""))

generateRPPR <- function(site){
  # site <-"Temple"
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
  
  # summarize counts
  race_summary <- df %>%
    group_by(race,hispanic,sex_at_birth_omnibus) %>%
    summarise(count = n())
  
  colnames(race_summary) <- c("race","hispanic","sex","count")
  #print(1)
  
  ### format to Vijay's pretty table
  
  View(race_summary)
  
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

  a6 <- nrow(df[which(df$race == 'race_more_than_one_race' & df$sex_at_birth_omnibus == 'F' & df$hispanic=='Not of Hispanic or Latino'),])
  b6 <- nrow(df[which(df$race == 'race_more_than_one_race' & df$sex_at_birth_omnibus == 'M' & df$hispanic=='Not of Hispanic or Latino'),])
  c6 <- nrow(df[which(df$race == 'race_more_than_one_race' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  d6 <- nrow(df[which(df$race == 'race_more_than_one_race' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Of Hispanic or Latino'),])
  e6 <- nrow(df[which(df$race == 'race_more_than_one_race' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Of Hispanic or Latino'),])
  f6 <- nrow(df[which(df$race == 'race_more_than_one_race' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Of Hispanic or Latino'),])
  g6 <- nrow(df[which(df$race == 'race_more_than_one_race' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  h6 <- nrow(df[which(df$race == 'race_more_than_one_race' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  i6 <- nrow(df[which(df$race == 'race_more_than_one_race' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])

  a7 <- nrow(df[which(df$race == 'race_unknown_or_not_reported' & df$sex_at_birth_omnibus =='F' & df$hispanic=='Not of Hispanic or Latino'),])
  b7 <- nrow(df[which(df$race == 'race_unknown_or_not_reported' & df$sex_at_birth_omnibus =='M' & df$hispanic=='Not of Hispanic or Latino'),])
  c7 <- nrow(df[which(df$race == 'race_unknown_or_not_reported' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Not of Hispanic or Latino'),])
  d7 <- nrow(df[which(df$race == 'race_unknown_or_not_reported' & df$sex_at_birth_omnibus == 'F' & df$hispanic == 'Of Hispanic or Latino'),])
  e7 <- nrow(df[which(df$race == 'race_unknown_or_not_reported' & df$sex_at_birth_omnibus == 'M' & df$hispanic == 'Of Hispanic or Latino'),])
  f7 <- nrow(df[which(df$race == 'race_unknown_or_not_reported' & df$sex_at_birth_omnibus == '' & df$hispanic == 'Of Hispanic or Latino'),])
  g7 <- nrow(df[which(df$race == 'race_unknown_or_not_reported' & df$sex_at_birth_omnibus == 'F' & df$hispanic == ''),])
  h7 <- nrow(df[which(df$race == 'race_unknown_or_not_reported' & df$sex_at_birth_omnibus == 'M' & df$hispanic == ''),])
  i7 <- nrow(df[which(df$race == 'race_unknown_or_not_reported' & df$sex_at_birth_omnibus == '' & df$hispanic == ''),])
  
  a8 <- sum(a1,a2,a3,a4,a5,a6,a7)
  b8 <- sum(b1,b2,b3,b4,b5,b6,b7)
  c8 <- sum(c1,c2,c3,c4,c5,c6,c7)
  d8 <- sum(d1,d2,d3,d4,d5,d6,d7)
  e8 <- sum(e1,e2,e3,e4,e5,e6,e7)
  f8 <- sum(f1,f2,f3,f4,f5,f6,f7)
  g8 <- sum(g1,g2,g3,g4,g5,g6,g7)
  h8 <- sum(h1,h2,h3,h4,h5,h6,h7)
  i8 <- sum(i1,i2,i3,i4,i5,i6,i7)

  
  # xxx = ''
  
  table <- data.frame(female_not_hispanic = c(a1,a2,a3,a4,a5,a6,a7,a8),
                      male_not_hispanic = c(b1,b2,b3,b4,b5,b6,b7,b8),
                      not_reported_not_hispanic = c(c1,c2,c3,c4,c5,c6,c7,c8),
                      female_hispanic = c(d1,d2,d3,d4,d5,d6,d7,d8),
                      male_hispanic = c(e1,e2,e3,e4,e5,e6,e7,e8),
                      not_reported_hispanic = c(f1,f2,f3,f4,f5,f6,f7,f8),
                      female_unknown = c(g1,g2,g3,g4,g5,g6,g7,g8),
                      male_unknown = c(h1,h2,h3,h4,h5,h6,h7,h8),
                      not_reported_unknown = c(i1,i2,i3,i4,i5,i6,i7,i8)
                      )
  
  colnames(table) <- c("Female\n Not Hispanic","Male\n Not Hispanic","Not Reported\n Not Hispanic","Female\n Hispanic","Male\n  Hispanic","Not Reported\n  Hispanic","Female\n Unknown","Male\n Unknown","Not Reported\n Unknown")
  
  row.names(table) <- c("American Indian","Asian","Native Hawaiian or Pacific Islander",
                        "Black or African American","White or Caucasian","More Than One Race",
                        "Unknown or Not Reported","Totals")
  
  if (site == "UMBC" | site == "UCI") {
    write.csv(table,paste('rppr/','UMBC & UCI','.csv',sep=''),row.names = TRUE)
  } else if (site == "UGA" | site == "Emory") {
    write.csv(table,paste('rppr/','UGA & Emory','.csv',sep=''),row.names = TRUE)
  } else {
    write.csv(table,paste('rppr/',site,'.csv',sep=''),row.names = TRUE)
  }

  
  return(rppr_table = table)

  
  
}
