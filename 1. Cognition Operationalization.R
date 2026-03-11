#------------------------------------------------------------------------------
# Project title: Is early childhood education associated with better midlife 
# cognition, especially for children facing socioeconomic marginalization?

# Script 1 of 5

# This script is included from Jilly, see details:

# Code for operationalizing midlife cognition variables and composite measures
# Author: Jilly Hebert
# Last updated: Mar 11, 2024

# Lines 17-22: Load libraries and data
# Lines 24-68: Rename raw variables     # WW added
# Lines 70-262: Clean raw cognition measures
#------------------------------------------------------------------------------
#Load libraries 
library(tidyverse)
library(here) # WW added

#Load data
raw_cog_download <- read.csv(here("Data", "raw_cognitive_measures.csv"))

#-------------------------------------------------------------------------------
# Rename raw variables     # WW added
#-------------------------------------------------------------------------------

raw_cog_download <- raw_cog_download %>% 
  rename(case_id = CASEID, cog_year = COGNITION_SOURCEYR,
         recall_A1 = `COGNITION_4A_1.000001`, recall_A2 = `COGNITION_4A_1.000002`, recall_A3 = `COGNITION_4A_1.000003`,
         recall_A4 = `COGNITION_4A_1.000004`, recall_A5 = `COGNITION_4A_1.000005`, recall_A6 = `COGNITION_4A_1.000006`,
         recall_A7 = `COGNITION_4A_1.000007`, recall_A8 = `COGNITION_4A_1.000008`, recall_A9 = `COGNITION_4A_1.000009`,
         recall_A10 = `COGNITION_4A_1.000010`,
         recall_B1 = `COGNITION_4B_1.000001`, recall_B2 = `COGNITION_4B_1.000002`, recall_B3 = `COGNITION_4B_1.000003`,
         recall_B4 = `COGNITION_4B_1.000004`, recall_B5 = `COGNITION_4B_1.000005`, recall_B6 = `COGNITION_4B_1.000006`,
         recall_B7 = `COGNITION_4B_1.000007`, recall_B8 = `COGNITION_4B_1.000008`, recall_B9 = `COGNITION_4B_1.000009`,
         recall_B10 = `COGNITION_4B_1.000010`,
         recall_C1 = `COGNITION_4C_1.000001`, recall_C2 = `COGNITION_4C_1.000002`, recall_C3 = `COGNITION_4C_1.000003`,
         recall_C4 = `COGNITION_4C_1.000004`, recall_C5 = `COGNITION_4C_1.000005`, recall_C6 = `COGNITION_4C_1.000006`,
         recall_C7 = `COGNITION_4C_1.000007`, recall_C8 = `COGNITION_4C_1.000008`, recall_C9 = `COGNITION_4C_1.000009`,
         recall_C10 = `COGNITION_4C_1.000010`,
         recall_D1 = `COGNITION_4D_1.000001`, recall_D2 = `COGNITION_4D_1.000002`, recall_D3 = `COGNITION_4D_1.000003`,
         recall_D4 = `COGNITION_4D_1.000004`, recall_D5 = `COGNITION_4D_1.000005`, recall_D6 = `COGNITION_4D_1.000006`,
         recall_D7 = `COGNITION_4D_1.000007`, recall_D8 = `COGNITION_4D_1.000008`, recall_D9 = `COGNITION_4D_1.000009`,
         recall_D10 = `COGNITION_4D_1.000010`,
         bc20 = COGNITION_6C, bc20_2 = COGNITION_6F, bc86 = COGNITION_6J, bc86_2 = COGNITION_6N,
         ser7_100 = COGNITION_7A, ser7_93 = COGNITION_7B, ser7_86 = COGNITION_7C, ser7_79 = COGNITION_7D, ser7_72 = COGNITION_7E,
         drecall_A1 = `COGNITION_8A_1.000001`, drecall_A2 = `COGNITION_8A_1.000002`, drecall_A3 = `COGNITION_8A_1.000003`,
         drecall_A4 = `COGNITION_8A_1.000004`, drecall_A5 = `COGNITION_8A_1.000005`, drecall_A6 = `COGNITION_8A_1.000006`,
         drecall_A7 = `COGNITION_8A_1.000007`, drecall_A8 = `COGNITION_8A_1.000008`, drecall_A9 = `COGNITION_8A_1.000009`,
         drecall_A10 = `COGNITION_8A_1.000010`,
         drecall_B1 = `COGNITION_8B_1.000001`, drecall_B2 = `COGNITION_8B_1.000002`, drecall_B3 = `COGNITION_8B_1.000003`,
         drecall_B4 = `COGNITION_8B_1.000004`, drecall_B5 = `COGNITION_8B_1.000005`, drecall_B6 = `COGNITION_8B_1.000006`,
         drecall_B7 = `COGNITION_8B_1.000007`, drecall_B8 = `COGNITION_8B_1.000008`, drecall_B9 = `COGNITION_8B_1.000009`,
         drecall_B10 = `COGNITION_8B_1.000010`,
         drecall_C1 = `COGNITION_8C_1.000001`, drecall_C2 = `COGNITION_8C_1.000002`, drecall_C3 = `COGNITION_8C_1.000003`,
         drecall_C4 = `COGNITION_8C_1.000004`, drecall_C5 = `COGNITION_8C_1.000005`, drecall_C6 = `COGNITION_8C_1.000006`,
         drecall_C7 = `COGNITION_8C_1.000007`, drecall_C8 = `COGNITION_8C_1.000008`, drecall_C9 = `COGNITION_8C_1.000009`,
         drecall_C10 = `COGNITION_8C_1.000010`,
         drecall_D1 = `COGNITION_8D_1.000001`, drecall_D2 = `COGNITION_8D_1.000002`, drecall_D3 = `COGNITION_8D_1.000003`,
         drecall_D4 = `COGNITION_8D_1.000004`, drecall_D5 = `COGNITION_8D_1.000005`, drecall_D6 = `COGNITION_8D_1.000006`,
         drecall_D7 = `COGNITION_8D_1.000007`, drecall_D8 = `COGNITION_8D_1.000008`, drecall_D9 = `COGNITION_8D_1.000009`,
         drecall_D10 = `COGNITION_8D_1.000010`)

raw_cog_download <- raw_cog_download %>%
  dplyr::select(!c(SAMPLE_ID, SAMPLE_RACE, SAMPLE_SEX))

write.csv(raw_cog_download, here("Data", "nlsy_cognition.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Clean cognition characteristics
#-------------------------------------------------------------------------------
raw_cog <- read.csv(here("Data", "nlsy_cognition.csv"))

cog_sub <- raw_cog %>%
  dplyr::filter(cog_year != -4)

#Immediate recall  (check which ones didn't complete)
recall_a <- cog_sub %>%
  dplyr::select(case_id, recall_A1, recall_A2, recall_A3, recall_A4, 
                recall_A5, recall_A6, recall_A7, recall_A8, recall_A9,
                recall_A10) %>%
  dplyr::filter(recall_A1 != -4) #Filters out people who didn't get A list
recall_a[recall_a < 0] <- NA #Change all refused/don't know/invalid skip to NA

recall_a <- recall_a %>%
  rowwise() %>%
  dplyr::mutate(recall_total = sum(across(starts_with("recall_"))))
table(recall_a$recall_total, useNA = "always")


recall_b <- cog_sub %>%
  dplyr::select(case_id, recall_B1, recall_B2, recall_B3, recall_B4, 
                recall_B5, recall_B6, recall_B7, recall_B8, recall_B9,
                recall_B10) %>%
  dplyr::filter(recall_B1 != -4) #Filters out people who didn't get B list
recall_b[recall_b < 0] <- NA #Change all all refused/don't know/invalid skip to NA

recall_b <- recall_b %>%
  rowwise() %>%
  dplyr::mutate(recall_total = sum(across(starts_with("recall_"))))


recall_c <- cog_sub %>%
  dplyr::select(case_id, recall_C1, recall_C2, recall_C3, recall_C4, 
                recall_C5, recall_C6, recall_C7, recall_C8, recall_C9,
                recall_C10) %>%
  dplyr::filter(recall_C1 != -4) #Filters out people who didn't get C list
recall_c[recall_c < 0] <- NA #Change all refused/don't know/invalid skip to NA

recall_c <- recall_c %>%
  rowwise() %>%
  dplyr::mutate(recall_total = sum(across(starts_with("recall_"))))


recall_d <- cog_sub %>%
  dplyr::select(case_id, recall_D1, recall_D2, recall_D3, recall_D4, 
                recall_D5, recall_D6, recall_D7, recall_D8, recall_D9,
                recall_D10) %>%
  dplyr::filter(recall_D1 != -4) #Filters out people who didn't get D list
recall_d[recall_d < 0] <- NA #Change all refused/don't know/invalid skip to NA

recall_d <- recall_d %>%
  rowwise() %>%
  dplyr::mutate(recall_total = sum(across(starts_with("recall_"))))


recall <- data.frame(rbind(cbind(recall_a$case_id, recall_a$recall_total),
                           cbind(recall_b$case_id, recall_b$recall_total),
                           cbind(recall_c$case_id, recall_c$recall_total),
                           cbind(recall_d$case_id, recall_d$recall_total)))
names(recall) <- c("case_id", "recall")
table(recall$recall, useNA = "always")

recall_check <- recall %>%
  group_by(case_id) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 1) #3 people with double tests
recall <- distinct(recall) #1 person was the same
recall_drop <- recall %>%
  dplyr::filter((case_id == 6716 & recall == 5) |
                  (case_id == 8683 & recall == 3)) #Kept max obs
recall <- anti_join(recall, recall_drop, by = c("case_id", "recall"))
# recall_check2 <- recall %>%
#   group_by(case_id) %>%
#   dplyr::mutate(count = n()) %>%
#   dplyr::filter(count > 1) #Good


#Delayed word recall
drecall_a <- cog_sub %>%
  dplyr::select(case_id, drecall_A1, drecall_A2, drecall_A3, drecall_A4, 
                drecall_A5, drecall_A6, drecall_A7, drecall_A8, drecall_A9,
                drecall_A10) %>%
  dplyr::filter(drecall_A1 != -4) #Filters out people who didn't get A list
drecall_a[drecall_a < 0] <- NA #Change all refused/don't know/invalid skip to NA

drecall_a <- drecall_a %>%
  rowwise() %>%
  dplyr::mutate(drecall_total = sum(across(starts_with("drecall_"))))


drecall_b <- cog_sub %>%
  dplyr::select(case_id, drecall_B1, drecall_B2, drecall_B3, drecall_B4, 
                drecall_B5, drecall_B6, drecall_B7, drecall_B8, drecall_B9,
                drecall_B10) %>%
  dplyr::filter(drecall_B1 != -4) #Filters out people who didn't get B list
drecall_b[drecall_b < 0] <- NA #Change all refused/don't know/invalid skip to NA

drecall_b <- drecall_b %>%
  rowwise() %>%
  dplyr::mutate(drecall_total = sum(across(starts_with("drecall_"))))


drecall_c <- cog_sub %>%
  dplyr::select(case_id, drecall_C1, drecall_C2, drecall_C3, drecall_C4, 
                drecall_C5, drecall_C6, drecall_C7, drecall_C8, drecall_C9,
                drecall_C10) %>%
  dplyr::filter(drecall_C1 != -4) #Filters out people who didn't get C list
drecall_c[drecall_c < 0] <- NA #Change all refused/don't know/invalid skip to NA

drecall_c <- drecall_c %>%
  rowwise() %>%
  dplyr::mutate(drecall_total = sum(across(starts_with("drecall_"))))


drecall_d <- cog_sub %>%
  dplyr::select(case_id, drecall_D1, drecall_D2, drecall_D3, drecall_D4, 
                drecall_D5, drecall_D6, drecall_D7, drecall_D8, drecall_D9,
                drecall_D10) %>%
  dplyr::filter(drecall_D1 != -4) #Filters out people who didn't get D list
drecall_d[drecall_d < 0] <- NA #Change all refused/don't know/invalid skip to NA

drecall_d <- drecall_d %>%
  rowwise() %>%
  dplyr::mutate(drecall_total = sum(across(starts_with("drecall_"))))


drecall <- data.frame(rbind(cbind(drecall_a$case_id, drecall_a$drecall_total),
                            cbind(drecall_b$case_id, drecall_b$drecall_total),
                            cbind(drecall_c$case_id, drecall_c$drecall_total),
                            cbind(drecall_d$case_id, drecall_d$drecall_total)))
names(drecall) <- c("case_id", "drecall")
table(drecall$drecall, useNA = "always")
drecall_check <- drecall %>%
  group_by(case_id) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 1) #3 people with NA score and real score, keep real score
drecall_drop <- drecall_check %>%
  dplyr::filter(is.na(drecall)) #Kept actual obs
drecall <- anti_join(drecall, drecall_drop, by = c("case_id", "drecall"))
# drecall_check2 <- drecall %>%
#   group_by(case_id) %>%
#   dplyr::mutate(count = n()) %>%
#   dplyr::filter(count > 1) #Good



#Ser7
ser7 <- cog_sub %>%
  dplyr::select(case_id, ser7_100, ser7_93, ser7_86, ser7_79, ser7_72)
ser7[ser7 < 0] <- NA #Change all refused/don't know/invalid skip to NA

ser7$cat100 <- ser7$ser7_100
ser7$cat100 <- ifelse(ser7$cat100 == 93, 1, ser7$cat100)
ser7$cat100 <- ifelse(ser7$cat100 != 1, 0, ser7$cat100)

ser7$cat93 <- ifelse((ser7$ser7_100 - 7) == ser7$ser7_93, 1, 0)
ser7$cat86 <- ifelse((ser7$ser7_93 - 7) == ser7$ser7_86, 1, 0)
ser7$cat79 <- ifelse((ser7$ser7_86 - 7) == ser7$ser7_79, 1, 0)
ser7$cat72 <- ifelse((ser7$ser7_79 - 7) == ser7$ser7_72, 1, 0)

ser7 <- ser7 %>%
  rowwise() %>%
  dplyr::mutate(ser7_total = sum(across(starts_with("cat"))))
ser7_clean <- ser7 %>%
  dplyr::select(case_id, ser7_total)
table(ser7_clean$ser7_total, useNA = "always")


#Backward counting
cog_sub <- cog_sub %>%
  dplyr::mutate(bc86_adj = case_when(bc86 == 5 | bc86 == 6 ~ 0, #Incorrect/start over
                                     bc86_2 == 1 ~ 0, #Correct 2nd try
                                     bc86 == 1 ~ 1)) #Correct 1st try

cog_sub <- cog_sub %>%
  dplyr::mutate(bc20_adj = case_when(bc20 == 5 | bc20 == 6 ~ 0, #Incorrect/start over
                                     bc20_2 == 1 ~ 0, #Correct 2nd try
                                     bc20 == 1 ~ 1)) #Correct 1st try

bc_clean <- cog_sub %>%
  dplyr::select(case_id, bc86_adj, bc20_adj)


#Add cognition back
cog <- recall
cog <- full_join(cog, drecall, by = "case_id")
cog <- full_join(cog, ser7_clean, by = "case_id")
cog <- full_join(cog, bc_clean, by = "case_id")
summary(cog)
saveRDS(cog, file=here("Data", "cognitive_raw_cleaned.rds")) # WW added
