#------------------------------------------------------------------------------
# Project title: Is early childhood education associated with better midlife 
# cognition, especially for children facing socioeconomic marginalization?

# Script 2 of 5 

# Code for cleaning NLSY exposure and covariate data
# Code author: Whitney Wells
# Code reviewer: Jilly Hebert
# Last updated: Feb 26, 2026

# Sections:
# 1. Load data files

# 2a. General variable cleaning 
# 2b. Parent variable cleaning 

#------------------------------------------------------------------------------

library(tidyverse)
library(here)


#------------------------------------------------------------------------------
# 1. Load data
#------------------------------------------------------------------------------
preschool_temp <-read.csv(here("Data", "preschool_v4.csv"))
# This dataset is obtained from NLSY: https://www.nlsinfo.org/investigator/pages/login


#------------------------------------------------------------------------------
# 2a. General variable cleaning 
#------------------------------------------------------------------------------

preschool <- preschool_temp %>% rename(case_id = CASEID, year_birth_orig = Q1.3_A.Y, usborn_orig = FAM.2A, 
                                       southernborn_orig = FAM.POB, mom_ed_orig = HGC.MOTHER, 
                                       dad_ed_orig = HGC.FATHER, race_orig = SAMPLE_RACE, 
                                       sex = SAMPLE_SEX, headstart_orig = Q3.31, preschool_orig = Q3.32,
                                       rural_orig = FAM.6, poverty_orig = POVSTATUS,
                                       mom_pay_orig = FAM.9, dad_pay_orig = FAM.11,
                                       mom_occ_orig = FAM.9A, dad_occ_orig = FAM.11A,
                                       mom_usborn_orig = FAM.15, dad_usborn_orig = FAM.21,
                                       cog_year_orig = COGNITION_SOURCEYR, 
                                       parent_role = FAM.7)

###### Clean Exposure:
#------------------------------------------------------------------------------
preschool$headstart <- ifelse(preschool$headstart_orig == -4, 0, preschool$headstart_orig)
# Valid skip of -4 is for individuals who were born before 1962 (1957-62 in data) and weren't shown question. 
# Have assumed none of those individuals attended headstart and coded as no, because Headstart began in 1964
preschool$headstart <- ifelse(preschool$headstart < 0, NA, preschool$headstart)
#table(preschool$headstart, preschool$headstart_orig, useNA="always")
#table(preschool$headstart, useNA="always")
# 3857 Missing is due to Refuse, Don't know, or Non-interview

preschool$preschool <- ifelse(preschool$preschool_orig == -4, 0, preschool$preschool_orig)
# Valid skip of -4 is for individuals who attended Head Start. (Though there are also 3 individuals who coded -4 for Head Start...)
# Have assumed none of those individuals attended preschool and coded as no.
preschool$preschool <- ifelse(preschool$preschool < 0, NA, preschool$preschool)
#table(preschool$preschool, preschool$preschool_orig, useNA="always")
#table(preschool$preschool, useNA="always")
# 3883 Missing is due to Don't know and Non-interview

#Create combined 3-level variable for Head Start or Preschool or None:
preschool <- preschool %>%
  dplyr::mutate(presch_headstart = case_when((headstart == 0 & preschool == 0)  ~ 0, # No preschool or Head Start
                                             headstart == 1 ~ 1, # Head start
                                             preschool == 1 ~ 2)) # Preschool
#table(preschool$presch_headstart, useNA="always")
preschool$presch_headstart <- factor(preschool$presch_headstart,
                                levels=c(0,1,2),
                                labels=c("No Preschool/Head Start", "Head Start",
                                         "Preschool"))



###### Clean Demographics:
#------------------------------------------------------------------------------
preschool$year_birth = preschool$year_birth_orig + 1900
preschool$year_birth_c1957 = preschool$year_birth - 1957 # Will keep original version for use in Table 1.

preschool <- preschool %>%
  dplyr::mutate(race = case_when(race_orig == 1 ~ 3, #Hispanic
                                 race_orig == 2 ~ 2, #Black
                                 race_orig == 3 ~ 1, #White
                                 is.na(race_orig) ~ NA)) #No missing
#table(preschool$race, useNA="always")

preschool$race <- factor(preschool$race,
                    levels=c(1,2,3),
                    labels=c("White",
                             "Black",
                             "Hispanic"))
preschool$sex <- factor(preschool$sex,
                   levels=c(1,2),
                   labels=c("Male",
                            "Female"))


preschool$usborn <- ifelse(preschool$usborn_orig < 0, NA, preschool$usborn_orig)
preschool$usborn <- ifelse(preschool$usborn == 2, 0, preschool$usborn)
#table(preschool$usborn, preschool$usborn_orig, useNA="always")
# Only 1 missing; invalid skip

preschool$southernborn <- ifelse(preschool$usborn==0, 2, preschool$southernborn_orig)
#subset_data <- preschool[preschool$southernborn_orig == -4, ]
#if (nrow(subset_data) > 0) {
#  table(subset_data$usborn_orig, useNA="always")
#} else {
#  cat("None")
#}
# Codebook states that -4 is valid skip, which should be based on being born in the US. 
# But 156 people coded as -4 were born in the US, so I'm using US born instead of -4 to recode to 'Born outside US'
preschool$southernborn <- ifelse(preschool$southernborn < 0, NA, preschool$southernborn)
#table(preschool$southernborn, preschool$southernborn_orig, useNA="always")
#table(preschool$southernborn, useNA="always")
# 217 missing made up of: Valid skip born inside US, Invalid skip, Don't know

preschool$southernborn <- factor(preschool$southernborn,
                            levels=c(0,1,2),
                            labels=c("Non-south",
                                     "South",
                                     "Outside US"))

preschool$rural <- ifelse(preschool$rural_orig < 0, NA, preschool$rural_orig)
#table(preschool$rural, preschool$rural_orig, useNA="always")
# 40 missing made up of: Invalid skip
preschool <- preschool %>%
  dplyr::mutate(rural_bin = case_when((rural == 1) ~ 1, 
                                      (rural == 2 | rural == 3) ~ 2, 
                                      (is.na(rural)) ~ NA))
#table(preschool$rural, preschool$rural_bin, useNA="always")

preschool$rural_bin <- factor(preschool$rural_bin,
                     levels=c(1,2),
                     labels=c("In town or city",
                              "In country or farm"))

preschool$poverty <- ifelse(preschool$poverty_orig < 0, NA, preschool$poverty_orig)
#table(preschool$poverty, preschool$poverty_orig, useNA="always")
# 2491 missing made up of: Invalid skip




###### Clean parent role indicators:
#------------------------------------------------------------------------------
preschool$mom_notpresent <- ifelse((preschool$parent_role == 15 | preschool$parent_role == 19 | preschool$parent_role == 25 | preschool$parent_role == 35 | 
                                      preschool$parent_role == 45 | preschool$parent_role == 55 | preschool$parent_role == 80 | preschool$parent_role == 90), 1, 0)
preschool$mom_notpresent <- ifelse(preschool$parent_role==-3, NA, preschool$mom_notpresent)
# NLSY used this question to capture the 'adult woman' that it then asked questions about "mother's" employment.
# The above options correspond to No adult woman, Some other arrangement, or On my own.

preschool$dad_notpresent <- ifelse((preschool$parent_role == 51 | preschool$parent_role == 52 | preschool$parent_role == 53 | preschool$parent_role == 54 | 
                                      preschool$parent_role == 55 | preschool$parent_role == 80 | preschool$parent_role == 90 | preschool$parent_role == 91 |
                                      preschool$parent_role == 93 ), 1, 0)
preschool$dad_notpresent <- ifelse(preschool$parent_role==-3, NA, preschool$dad_notpresent)
# The above options correspond to No adult man, Some other arrangement, or On my own.

preschool$neverknew_mom <- ifelse(preschool$mom_usborn_orig==3, 1, 0)
preschool$neverknew_dad <- ifelse(preschool$dad_usborn_orig==3, 1, 0)


###### Cognition year:
#------------------------------------------------------------------------------
preschool$cog_year <- ifelse(preschool$cog_year_orig < 0, NA, preschool$cog_year_orig)

preschool$cog_year <- factor(preschool$cog_year)

saveRDS(preschool, file=here("Data", "preschool.rds"))





#------------------------------------------------------------------------------
# 2b. Parent variable cleaning - code valid skips as missing (to impute over, using
# the parent presence indicators to inform imputation)
#------------------------------------------------------------------------------
preschool = readRDS(file=here("Data", "preschool.rds"))

#Recode parent nativity:
#------------------------------------------------------------------------------
preschool$mom_usborn <- ifelse(preschool$mom_usborn_orig < 0, NA, preschool$mom_usborn_orig)
preschool$mom_usborn <- ifelse(preschool$mom_usborn ==3, NA, preschool$mom_usborn)
#table(preschool$mom_usborn, preschool$mom_usborn_orig, useNA="always")
# 20 missing = Refuse, Don't know, Invalid skip
# 28 are coded level 3 for 'never knew mom' -> Have recoded these to be missing

preschool$dad_usborn <- ifelse(preschool$dad_usborn_orig < 0, NA, preschool$dad_usborn_orig)
preschool$dad_usborn <- ifelse(preschool$dad_usborn ==3, NA, preschool$dad_usborn)
#table(preschool$dad_usborn, preschool$dad_usborn_orig, useNA="always")
# 72 missing = Refuse, Don't know, Invalid skip
# 230 are coded level 3 for 'never knew dad' -> Have recoded these to be missing

preschool$mom_usborn <- factor(preschool$mom_usborn,
                               levels=c(1,2),
                               labels=c("In the US",
                                        "Other country"))
preschool$dad_usborn <- factor(preschool$dad_usborn,
                               levels=c(1,2),
                               labels=c("In the US",
                                        "Other country"))


#Recode parent education:
#------------------------------------------------------------------------------
preschool$mom_ed <- ifelse(preschool$mom_ed_orig < 0, NA, preschool$mom_ed_orig)
# 27 are coded as Valid skip; all of these state "never knew mom" for mom_usborn. -> Have recoded these to be missing
#table(preschool$mom_ed, preschool$mom_ed_orig, useNA="always")
# 781 NA, made up of Don't know, Invalid skip, and Refused. 

preschool$dad_ed <- ifelse(preschool$dad_ed_orig < 0, NA, preschool$dad_ed_orig)
# 230 coded as -4, who all responded "Never knew dad" for dad_USborn. -> Have recoded these to be missing
#table(preschool$dad_ed, preschool$dad_ed_orig, useNA="always")
# 1576 missing, made up of Don't know, Invalid skip, Refused

# Among the 1150 who are missing due to "Don't know", 495 of these reported dad not present. (Among 2309 who reported dad not present, 638 were missing on dad's education)
# ---> Decided to leave these as missing and will impute over these
#subset_data <- preschool[preschool$dad_notpresent==1, ]
#if (nrow(subset_data) > 0) {
#  table(subset_data$dad_ed, useNA="always")
#} else {
#  cat("None")
#}


#Recode parent job variables:
#------------------------------------------------------------------------------
preschool$mom_pay <- ifelse(preschool$mom_pay <0, NA, preschool$mom_pay)
#table(preschool$mom_pay, preschool$mom_pay_orig, useNA="always")
# 199 valid skips, all for mom not present. -> Have recoded these to be missing
# 113 missing, for Invalid skip, Don't know, Refused

preschool$dad_pay <- ifelse(preschool$dad_pay <0, NA, preschool$dad_pay)
#table(preschool$dad_pay, preschool$dad_pay_orig, useNA="always")
# 2309 valid skips, all for dad not present. -> Have recoded these to be missing
# 121 missing, for Invalid skip, Don't know

preschool$dad_pay <- factor(preschool$dad_pay,
                            levels=c(0,1),
                            labels=c("Dad unemployed",
                                     "Dad employed"))
preschool$mom_pay <- factor(preschool$mom_pay,
                            levels=c(0,1),
                            labels=c("Mom unemployed",
                                     "Mom employed"))

# Recode parent occupation:
#------------------------------------------------------------------------------
#table(preschool$dad_occ_orig, useNA="always")
#subset_data <- preschool[preschool$dad_occ_orig==-4, ]
#if (nrow(subset_data) > 0) {
#  table(subset_data$dad_pay, useNA="always")
#} else {
#  cat("None")
#}
preschool <- preschool %>%
  dplyr::mutate(dad_skilled = case_when((dad_occ_orig<=300 & dad_occ_orig>=0) ~ 1,
                                        (dad_occ_orig > 300) ~ 0,
                                        (dad_occ_orig==-4 & dad_pay=="Dad unemployed") ~ 2,
                                        (dad_occ_orig<0) ~ NA))
#table(preschool$dad_occ_orig, preschool$dad_skilled, useNA="always")
# 3154 Valid skips, all are either dad unemployed or dad not in HH at age 14. -> Have recoded these to be missing unless it was for unemployment
# 720 missing, all are either Invalid skip or Don't know



preschool <- preschool %>%
  dplyr::mutate(mom_skilled = case_when((mom_occ_orig<=300 & mom_occ_orig>=0) ~ 1,
                                        (mom_occ_orig > 300) ~ 0,
                                        (mom_occ_orig==-4 & mom_pay=="Mom unemployed") ~ 2,
                                        (mom_occ_orig<0) ~ NA))
#table(preschool$mom_occ_orig, preschool$mom_skilled, useNA="always")
# 6112 Valid skips, all are either mom unemployed or not in HH at age 14. -> Have recoded these to be missing unless it was for unemployment
# 385 missing, all are either Invalid skip or Don't know or Refused


preschool$dad_skilled <- factor(preschool$dad_skilled,
                                levels=c(0,1,2),
                                labels=c("Dad unskilled",
                                         "Dad skilled",
                                         "Dad unemployed"))
preschool$mom_skilled <- factor(preschool$mom_skilled,
                                levels=c(0,1,2),
                                labels=c("Mom unskilled",
                                         "Mom skilled",
                                         "Mom unemployed"))

# Subset and save:
#------------------------------------------------------------------------------
preschool_sub <- subset(preschool, select = c(case_id, presch_headstart, year_birth_c1957, cog_year,
                                              sex, race, southernborn, usborn, rural_bin, poverty,
                                              mom_notpresent, dad_notpresent, neverknew_mom, neverknew_dad,
                                              mom_usborn, dad_usborn, mom_pay, dad_pay, mom_skilled, dad_skilled, mom_ed, dad_ed))
summary(preschool_sub)
saveRDS(preschool_sub, file=here("Data", "preschool_indicatormissing.rds"))