library(cesR)
library(labelled)
library(tidyverse)
# load the raw survey data
get_ces("ces2019_web")
# convert values to the type of factor
ces2019_web <- to_factor(ces2019_web)
head(ces2019_web)
# select some useful variables
ces2019_web <- ces2019_web %>% select(cps19_yob, cps19_gender, cps19_province, cps19_education, cps19_votechoice) %>% filter(cps19_votechoice!="NA")

# read the raw census data
data <- read_csv('98-402-X2016010-T1-CANPR-eng.csv')
# pivot data
educ_cols_count <- c("Total - Highest certificate, diploma or degree (2016 counts)"                              
                     ,"No certificate, diploma or degree (2016 counts)"                                           
                     ,"Secondary (high) school diploma or equivalency certificate (2016 counts)"                  
                     ,"Apprenticeship or trades certificate or diploma (2016 counts)"                             
                     ,"College, CEGEP or other non-university certificate or diploma (2016 counts)"               
                     ,"University certificate or diploma below bachelor level (2016 counts)")
data_pivot <- data %>% select(c("Age","Sex","Geographic name", educ_cols_count)) %>% 
  pivot_longer(cols=educ_cols_count, names_to="education",values_to="total_count")
# delete repeatable observations
census_data <- data_pivot %>% filter(Age!="All ages, 15-plus" & Age!="25 to 64" & Sex!="Both sexes")
# cannot load the France, download raw data and use excel change it
survey_data <- read.csv("survey_data1.csv")

# change age to the numeric type
survey_data$cps19_age <- as.numeric(survey_data$cps19_age)
# separate different age into age groups
survey_data$cps19_age[survey_data$cps19_age >= 55 & survey_data$cps19_age <= 64] <- "55 to 64"
survey_data$cps19_age[survey_data$cps19_age >= 45 & survey_data$cps19_age <= 54] <- "45 to 54"
survey_data$cps19_age[survey_data$cps19_age >= 35 & survey_data$cps19_age <= 44] <- "35 to 44"
survey_data$cps19_age[survey_data$cps19_age >= 25 & survey_data$cps19_age <= 34] <- "25 to 34"
survey_data$cps19_age[survey_data$cps19_age >= 18 & survey_data$cps19_age <= 24] <- "18 to 24"
survey_data$cps19_age[survey_data$cps19_age >= 65] <- "65 or above"

# delete repeatable observations
survey_data <- survey_data %>% filter(cps19_gender!="Other (e.g. Trans, non-binary, two-spirit, gender-queer)")
# choose the useful columns
survey_data <- survey_data %>% select(c("cps19_gender", "cps19_province", "cps19_education", "cps19_votechoice", "cps19_age"))
# rename column names 
survey_data$cps19_gender[survey_data$cps19_gender=="A woman"] <- "Female"
survey_data$cps19_gender[survey_data$cps19_gender=="A man"] <- "Male"

# sort the education level
survey_data$cps19_education[survey_data$cps19_education=="Completed elementary school" |
                              survey_data$cps19_education=="Some secondary/ high school"] <- "No certificate, diploma or degree (2016 counts)"
survey_data$cps19_education[survey_data$cps19_education=="Completed secondary/ high school" | 
                              survey_data$cps19_education=="Some technical, community college, CEGEP, College Classique"] <- "Secondary (high) school diploma or equivalency certificate (2016 counts)"
survey_data$cps19_education[survey_data$cps19_education=="Completed technical, community college, CEGEP, College Classique"] <- "College, CEGEP or other non-university certificate or diploma (2016 counts)"
survey_data$cps19_education[survey_data$cps19_education=="Some university"] <- "University certificate or diploma below bachelor level (2016 counts)"
survey_data$cps19_education[survey_data$cps19_education=="Bachelor's degree" |
                              survey_data$cps19_education=="Master's degree"] <- "Apprenticeship or trades certificate or diploma (2016 counts)"
survey_data$cps19_education[survey_data$cps19_education=="Professional degree or doctorate"] <- "Total - Highest certificate, diploma or degree (2016 counts)"

# add new columns that represent whether this person vote or not
survey_data <- survey_data %>% 
  mutate(vote_green=
           ifelse(cps19_votechoice=="Green Party", 1, 0))
survey_data <- survey_data %>% 
  mutate(vote_lib=
           ifelse(cps19_votechoice=="Liberal Party", 1, 0))
survey_data <- survey_data %>% 
  mutate(vote_con=
           ifelse(cps19_votechoice=="Conservative Party", 1, 0))
survey_data <- survey_data %>% 
  mutate(vote_ndp=
           ifelse(cps19_votechoice=="ndp", 1, 0))
survey_data <- survey_data %>% 
  mutate(vote_bloc=
           ifelse(cps19_votechoice=="Bloc Québécois", 1, 0))

# rename column names
names(survey_data)[names(survey_data)=="cps19_gender"] <- "Sex"
names(survey_data)[names(survey_data)=="cps19_age"] <- "Age"
names(survey_data)[names(survey_data)=="cps19_education"] <- "education"

# simplify the names of provinces 
survey_data$cps19_province[survey_data$cps19_province=="Newfoundland and Labrador"] <- "NL"
survey_data$cps19_province[survey_data$cps19_province=="Prince Edward Island"] <- "PE"
survey_data$cps19_province[survey_data$cps19_province=="Nova Scotia"] <- "NS"
survey_data$cps19_province[survey_data$cps19_province=="New Brunswick"] <- "NB"
survey_data$cps19_province[survey_data$cps19_province=="Quebec"] <- "QC"
survey_data$cps19_province[survey_data$cps19_province=="Ontario"] <- "ON"
survey_data$cps19_province[survey_data$cps19_province=="Manitoba"] <- "MB"
survey_data$cps19_province[survey_data$cps19_province=="Saskatchewan"] <- "SK"
survey_data$cps19_province[survey_data$cps19_province=="Alberta"] <- "AB"
survey_data$cps19_province[survey_data$cps19_province=="British Columbia"] <- "BC"
survey_data$cps19_province[survey_data$cps19_province=="Yukon"] <- "YT"
survey_data$cps19_province[survey_data$cps19_province=="Northwest Territories"] <- "NWT"
survey_data$cps19_province[survey_data$cps19_province=="Nunavut"] <- "NV"
names(survey_data)[names(survey_data)=="cps19_province"] <- "Province"

# simplify the names of provinces 
census_data <- census_data %>% filter(`Geographic name`!="Canada")
census_data$`Geographic name`[census_data$`Geographic name`=="Newfoundland and Labrador"] <- "NL"
census_data$`Geographic name`[census_data$`Geographic name`=="Prince Edward Island"] <- "PE"
census_data$`Geographic name`[census_data$`Geographic name`=="Nova Scotia"] <- "NS"
census_data$`Geographic name`[census_data$`Geographic name`=="New Brunswick"] <- "NB"
census_data$`Geographic name`[census_data$`Geographic name`=="Quebec"] <- "QC"
census_data$`Geographic name`[census_data$`Geographic name`=="Ontario"] <- "ON"
census_data$`Geographic name`[census_data$`Geographic name`=="Manitoba"] <- "MB"
census_data$`Geographic name`[census_data$`Geographic name`=="Saskatchewan"] <- "SK"
census_data$`Geographic name`[census_data$`Geographic name`=="Alberta"] <- "AB"
census_data$`Geographic name`[census_data$`Geographic name`=="British Columbia"] <- "BC"
census_data$`Geographic name`[census_data$`Geographic name`=="Yukon"] <- "YT"
census_data$`Geographic name`[census_data$`Geographic name`=="Northwest Territories"] <- "NWT"
census_data$`Geographic name`[census_data$`Geographic name`=="Nunavut"] <- "NV"
names(census_data)[names(census_data)=="Geographic name"] <- "Province"

write_csv(survey_data, "survey_data.csv")
write_csv(census_data, "census_data.csv")
