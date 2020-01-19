
library("tidyverse")
library("here")
library(readxl)


CSEC_DATA = read_excel(here("/../csec_data.xlsx"))

csec_fields = read_csv(here("/../csec_fields.csv"))

csec_options = read_csv(here("/../csec_options.csv"))

CSEC_DATA = CSEC_DATA %>% filter( !(unique_id == "Unique Code")  )

CSEC_DATA = CSEC_DATA %>% rename(names = fname) %>% separate(names, c("fname", "lname"), extra = "merge", fill = "right")

#Gender
#View(table(CSEC_DATA$gender, useNA = "always"))

CSEC_DATA_CLEAN = CSEC_DATA %>% mutate(gender = case_when( (gender == "Male") ~ "1",
                                  (gender == "Female") ~ "2",
                                 TRUE ~ "0")
            )

#Disability
#View(table(CSEC_DATA_CLEAN$disability, useNA = "always"))

CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(disability = case_when( disability == "Disable" ~ "1",
                                                           TRUE ~ "0") )

#intake_quarter
#table(CSEC_DATA$intake_quarter, useNA = "always")

CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(intake_quarter = case_when( intake_quarter == "Quarter 1" ~ "1",
                                                                   intake_quarter == "Quarter 2" ~ "2",
                                                                   intake_quarter == "Quarter 3" ~ "3",
                                                                   intake_quarter == "Quarter 4" ~ "4",
                                                               TRUE ~ "0") )
#county
#table(CSEC_DATA$county, useNA = "always")
#county_options = csec_options %>% filter(field == "county")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(county = case_when( county == "Mombasa" ~ "2",
                                                                   county == "Kilifi South" ~ "3",
                                                                   TRUE ~ "0") )

#subcounty
#table(CSEC_DATA$subcounty, useNA = "always")
#subcounty_options = csec_options %>% filter(field == "subcounty")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(subcounty = case_when( subcounty == "Kisauni" ~ "2",
                                                           subcounty == "Mtwapa" ~ "8",
                                                           subcounty == "Mvita" ~ "7",
                                                           subcounty == "Nyali" ~ "3",
                                                           TRUE ~ "0") )
#child_status
#table(CSEC_DATA$child_status, useNA = "always")
#child_status_options = csec_options %>% filter(field == "child_status")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(child_status = case_when( child_status == "Exploited" ~ "3",
                                                                       child_status == "Vulnerable" ~ "2",
                                                                 TRUE ~ "0") )
#school_status
#table(CSEC_DATA$school_status, useNA = "always")
#school_status_options = csec_options %>% filter(field == "school_status")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(school_status = case_when( school_status == "in-school" | school_status == "In-school" ~ "1",
                                                                        school_status == "out-school" | school_status == "Out-school" | school_status == "Out of school"  ~ "2",
                                                                       TRUE ~ "0") )


#level_education
#table(CSEC_DATA_CLEAN$level_education, useNA = "always")
#level_education_options = csec_options %>% filter(field == "level_education")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(level_education = case_when( level_education == "Primary" ~ "1",
                                                                          level_education == "Secondary" ~ "2",
                                                                          level_education == "Tvet" ~ "3",
                                                                       TRUE ~ "0") )

#counseling
#table(CSEC_DATA_CLEAN$counseling, useNA = "always")
#counseling_options = csec_options %>% filter(field == "counseling")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(counseling = case_when( counseling == "counselled" | counseling == "Counselled"  ~ "1",
                                                                       TRUE ~ "0") )

#education_support
#table(CSEC_DATA_CLEAN$education_support, useNA = "always")
#education_support_options = csec_options %>% filter(field == "education_support")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(education_support = case_when( education_support == "In Process" | education_support == "In Progress" | education_support == "School Fees"  ~ "1",
                                                                       TRUE ~ "0") )

#accelerated_learning
#table(CSEC_DATA$accelerated_learning, useNA = "always")
#accelerated_learning_options = csec_options %>% filter(field == "accelerated_learning")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(accelerated_learning = case_when( accelerated_learning == "Received" ~ "1",
                                                                       TRUE ~ "0") )

#medical_care
#table(CSEC_DATA$medical_care, useNA = "always")
#medical_care_options = csec_options %>% filter(field == "medical_care")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(medical_care = case_when( medical_care == "Received" ~ "1",
                                                                               TRUE ~ "0") )

#rescued_csec
#table(CSEC_DATA_CLEAN$rescued_csec, useNA = "always")
#rescued_csec_options = csec_options %>% filter(field == "rescued_csec")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(rescued_csec = case_when( rescued_csec == "Rescued" ~ "1",
                                                                       TRUE ~ "0") )

#family_counseling
#table(CSEC_DATA$family_counseling, useNA = "always")
#family_counseling_options = csec_options %>% filter(field == "family_counseling")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(family_counseling = case_when( family_counseling == "Received" ~ "1",
                                                                       TRUE ~ "0") )

#provided_income
#table(CSEC_DATA_CLEAN$provided_income, useNA = "always")
#provided_income_options = csec_options %>% filter(field == "provided_income")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(provided_income = case_when( provided_income == "Disability support" ~ "3",
                                                                          provided_income == "Mother support" ~ "4",
                                                                          provided_income == "Pending Received child mothers support" ~ "5",
                                                                          provided_income == "Support Package for children" ~ "6",
                                                                       TRUE ~ "0") )

#legal_support
#table(CSEC_DATA_CLEAN$legal_support, useNA = "always")
#legal_support_options = csec_options %>% filter(field == "legal_support")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(legal_support = case_when( legal_support == "Received" ~ "1",
                                                                       TRUE ~ "0") )

#parent_guard_relationship
#table(CSEC_DATA_CLEAN$parent_guard_relationship, useNA = "always")
#parent_guard_relationship_options = csec_options %>% filter(field == "parent_guard_relationship")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(parent_guard_relationship = case_when( parent_guard_relationship == "Aunt" ~ "1",
                                                                                    parent_guard_relationship == "Brother" ~ "2",
                                                                                    parent_guard_relationship == "Cousin" ~ "3",
                                                                                    parent_guard_relationship == "Father" ~ "4",
                                                                                    parent_guard_relationship == "Grandfather" ~ "5",
                                                                                    parent_guard_relationship == "Grandmother" ~ "6",
                                                                                    parent_guard_relationship == "Guardian" ~ "7",
                                                                                    parent_guard_relationship == "Mother" ~ "8",
                                                                                    
                                                                                    parent_guard_relationship == "Neighbour" ~ "9",
                                                                                    parent_guard_relationship == "Sister" ~ "10",
                                                                                    parent_guard_relationship == "Uncle" ~ "11",
                                                                                    parent_guard_relationship == "N/A" ~ "12",
                                                                                    
                                                                       TRUE ~ "0") )

#parenting_skills_training
#table(CSEC_DATA$parenting_skills_training, useNA = "always")
#parenting_skills_training_options = csec_options %>% filter(field == "parenting_skills_training")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(parenting_skills_training = case_when( parenting_skills_training == "Received" ~ "1",
                                                                        TRUE ~ "0") )

#family_training
#table(CSEC_DATA_CLEAN$family_training, useNA = "always")
#family_training_options = csec_options %>% filter(field == "family_training")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(family_training = case_when( family_training == "Trained" ~ "1",
                                                                          family_training == "Not Trained" ~ "2",
                                                                       TRUE ~ "0") )

#family_income
#table(CSEC_DATA_CLEAN$family_income, useNA = "always")
#family_income_options = csec_options %>% filter(field == "family_income")
CSEC_DATA_CLEAN = CSEC_DATA_CLEAN %>% mutate(family_income = case_when( family_income == "Start kit" | family_income == "Start Kit" | family_income == "Start Up"  ~ "1",
                                                                       TRUE ~ "0") )

write_csv(CSEC_DATA_CLEAN, here("/../CSEC_DATA_CLEAN.csv") )


