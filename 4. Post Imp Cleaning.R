#------------------------------------------------------------------------------
# Project title: Is early childhood education associated with better midlife 
# cognition, especially for children facing socioeconomic marginalization?

# Script 4 of 5

# Code for cleaning imputed dataset
# Code author: Whitney Wells
# Code reviewer: Jilly Hebert
# Last updated: Feb 26, 2026 

# Sections:
# 1a. Set up function for composite cognition measures following imputation
# 1b. Helper function to label variables
# 1c. Set up function to create index of cSES, and additional post-imputation variable manipulation

# 2a. Create dataset for main analysis (with imputation) (eFig 1)
# 2b. Create dataset for sensitivity analysis excluding people born before 1960
# 2c. Additional data cleaning for complete case analysis


#------------------------------------------------------------------------------

library(here)
library(tidyverse)
library(labelled)
library(Hmisc)


#------------------------------------------------------------------------------
# 1a. Set up function for composite cognition measures following imputation
#------------------------------------------------------------------------------
# Code from Jilly's "Cognition Operationalization" script - moved to this script
# "Create cognition measures (memory, attention, overall 1, and overall 2)"

# Loops through each imputation sample to create z-score by sample
# Set up function here - then will call it separately for imputed and for complete case later

sample_loop <- function(data) {
  data <- data %>% rename("ser7" = "ser7_total", "bc86" = "bc86_adj", "bc20" = "bc20_adj", "imp" = ".imp")
  # Change all raw outcomes to numeric
  numeric_vars <- c("recall", "drecall", "ser7", "bc86", "bc20")
  data[numeric_vars] <- lapply(data[numeric_vars], as.character) #Ensures that factors stay as their original number (0/1) instead of keeping factor value (1/2)
  data[numeric_vars] <- lapply(data[numeric_vars], as.numeric)
  
  final <- data.frame()
  sample <- unique(data$imp)
  
  for (i in sort(sample)) {
    
    subset <- data %>%
      dplyr::filter(imp == i)
    subset$mice_sample <- i
    
    #Generate outcome 1 (z-memory): Memory
    ##Average recall and delayed word recall then z-score
    subset$memory <- (subset$recall + subset$drecall)/2 #Average memory
    
    memory_mean <- mean(subset$memory, na.rm = T) #mean of average
    memory_sd <- sd(subset$memory, na.rm = T) #sd of average
    subset$z_memory <- (subset$memory - memory_mean)/memory_sd #z-score memory
    
    
    #Generate outcome 2 (z-attention): Attention
    ##Z-score serial 7s, backwards counting from 86, and backwards counting from 20, then average and z-score again
    ser7_mean <- mean(subset$ser7, na.rm = T)
    ser7_sd <- sd(subset$ser7, na.rm = T)
    subset$z_ser7 <- (subset$ser7 - ser7_mean)/ser7_sd #z-score serial 7s
    
    bc86_mean <- mean(subset$bc86, na.rm = T)
    bc86_sd <- sd(subset$bc86, na.rm = T)
    subset$z_bc86 <- (subset$bc86 - bc86_mean)/bc86_sd #z-score bc86
    
    bc20_mean <- mean(subset$bc20, na.rm = T)
    bc20_sd <- sd(subset$bc20, na.rm = T)
    subset$z_bc20 <- (subset$bc20 - bc20_mean)/bc20_sd #z-score bc20
    
    subset$attention <- (subset$z_ser7 + subset$z_bc20 + subset$z_bc86)/3
    attention_mean <- mean(subset$attention, na.rm = T)
    attention_sd <- sd(subset$attention, na.rm = T)
    subset$z_attention <- (subset$attention - attention_mean)/attention_sd #z-score attention
    
    
    #Generate outcome 3 (z_global): z-standardized midlife global summary score 1 
    ##Average z-scored memory and attention then z-score
    subset$global <- (subset$z_memory + subset$z_attention)/2
    global_mean <- mean(subset$global, na.rm = T)
    global_sd <- sd(subset$global, na.rm = T)
    subset$z_global <- (subset$global - global_mean)/global_sd #z-score overall global
    
    final <- rbind(final, subset)
    
  }
  
  return(final)
  
}


#------------------------------------------------------------------------------
# 1b. Helper function to label variables
#------------------------------------------------------------------------------
# Define the variable labels
var_labels <- c(
  year_birth = "Year of birth",
  presch_headstart = "Preschool or Headstart",
  year_birth_c1957 = "Year of birth",
  sex = "Sex",
  race = "Race/ethnicity",
  southernborn = "Birthplace",
  mom_usborn = "Mom nativity",
  dad_usborn = "Dad nativity",
  poverty = "Poverty prior year",
  rural_bin = "Rural residence",
  mom_ed_cat = "Mom education",
  dad_ed_cat = "Dad education",
  mom_pay = "Mom employment",
  dad_pay = "Dad employment",
  mom_skilled = "Mom occupation",
  dad_skilled = "Dad occupation",
  mom_absent = "Mom presence",
  dad_absent = "Dad presence",
  z_memory = "Memory z-score",
  z_attention = "Attention z-score",
  z_cogfunc = "Cognition z-score",
  raw_cogfunc = "Raw cognition score",
  cSEScount_tri = "Childhood SES index",
  cSEScount_tri_completecase = "Child SES index",
  z_global = "Cognition score"
)

# Create a function to label variables in a dataframe
label_variables <- function(df) {
  labels <- as.list(var_labels[match(names(df), names(var_labels))])
  label(df) <- labels
  return(df)
}


#------------------------------------------------------------------------------
# 1c. Set up function to create index of cSES, and additional post-imputation variable manipulation
#------------------------------------------------------------------------------
post_imp_data_cleaning <- function(df) {
  # Create cSES indexes:
  # Main index variable:
  df$cSEScount <- (df$poverty == 1) +
    (df$dad_pay == "Dad unemployed") +
    (df$mom_ed < 8) +
    (df$dad_ed <8) +
    (df$neverknew_dad==1 | df$dad_notpresent==1)
  #head(df[, c("poverty", "dad_pay", "mom_ed", "dad_ed", "neverknew_dad", "dad_notpresent", "cSEScount")], 20)
  
  df <- df %>%
    dplyr::mutate(cSEScount_tri = case_when((cSEScount ==0) ~ 1,
                                            (cSEScount >0 & cSEScount<3 ) ~ 2,
                                            (cSEScount >=3 ) ~ 3))
  
  df$cSEScount <- factor(df$cSEScount)
  
  # Main trichotomized variable:
  df$cSEScount_tri <- factor(df$cSEScount_tri,
                             levels=c(1,2,3),
                             labels=c("Lower disadvantage",
                                      "Medium disadvantage",
                                      "Higher disadvantage"))
  
  
  
  # Relabel levels that I want relabeled
  levels(df$poverty) <- c("Not in poverty", "In poverty")
  levels(df$mom_notpresent) <- c("Mom present", "Mom not present")
  levels(df$dad_notpresent) <- c("Dad present", "Dad not present")
  levels(df$neverknew_mom) <- c("Knew mom", "Never knew mom")
  levels(df$neverknew_dad) <- c("Knew dad", "Never knew dad")
  levels(df$presch_headstart) <- c("Neither", "Head Start", "Preschool")
  
  # Create composite variable that captures both whether parent absent and if never knew parent:
  df <- df %>%
    dplyr::mutate(dad_absent = case_when((dad_notpresent == "Dad not present" | neverknew_dad == "Never knew dad") ~ 1,
                                         is.na(dad_notpresent) ~ NA,
                                         (dad_notpresent == "Dad present" & neverknew_dad == "Knew dad") ~ 0))
  
  df <- df %>%
    dplyr::mutate(mom_absent = case_when((mom_notpresent == "Mom not present" | neverknew_mom == "Never knew mom") ~ 1,
                                         is.na(mom_notpresent) ~ NA,
                                         (mom_notpresent == "Mom present" & neverknew_mom == "Knew mom") ~ 0))
  
  df$mom_absent <- factor(df$mom_absent,
                          levels=c(0,1),
                          labels=c("Mom present",
                                   "Mom absent or never known"))
  
  df$dad_absent <- factor(df$dad_absent,
                          levels=c(0,1),
                          labels=c("Dad present",
                                   "Dad absent or never known"))
  
  # Create 4-level version of categorical parent education:
  df <- df %>%
    dplyr::mutate(mom_ed_cat = case_when((mom_ed <8) ~ 1,
                                         (mom_ed >=8 & mom_ed <12) ~ 2,
                                         (mom_ed == 12 ) ~ 3,
                                         (mom_ed > 12 ) ~ 4))
  df$mom_ed_cat <- factor(df$mom_ed_cat,
                          levels=c(1,2,3,4),
                          labels=c("Mom < 8th grade",
                                   "Mom 8th to < HS",
                                   "Mom Highschool",
                                   "Mom > Highschool"))
  
  df <- df %>%
    dplyr::mutate(dad_ed_cat = case_when((dad_ed <8) ~ 1,
                                         (dad_ed >=8 & dad_ed <12) ~ 2,
                                         (dad_ed == 12 ) ~ 3,
                                         (dad_ed > 12 ) ~ 4))
  df$dad_ed_cat <- factor(df$dad_ed_cat,
                          levels=c(1,2,3,4),
                          labels=c("Dad < 8th grade",
                                   "Dad 8th to < HS",
                                   "Dad Highschool",
                                   "Dad > Highschool"))
  # Create racesex variable:
  df <- df %>%
    dplyr::mutate(racesex = case_when((sex == "Male" & race =="White") ~ 1, # Male White
                                      (sex == "Male" & race =="Black") ~ 2, # Male Black
                                      (sex == "Male" & race =="Hispanic") ~ 3, # Male Hispanic
                                      (sex == "Female" & race =="White") ~ 4, # Female White
                                      (sex == "Female" & race =="Black") ~ 5, # Female Black
                                      (sex == "Female" & race =="Hispanic") ~ 6, # Female Hispanic
                                      (is.na(sex) | is.na(race)) ~ NA))
  #table(df_rds$racesex, useNA="always")
  df$racesex <- factor(df$racesex,
                       levels=c(1,2,3,4,5,6),
                       labels=c("Male White",
                                "Male Black",
                                "Male Hispanic",
                                "Female White",
                                "Female Black",
                                "Female Hispanic"))
  
  # Create year of birth version for use in Table 1:
  df$year_birth = df$year_birth_c1957 + 1957 
  
  return(df)
}



#------------------------------------------------------------------------------
# 2a. Create dataset for main analysis (with imputation) (eFig 1)
#------------------------------------------------------------------------------
# Perform post-imputation cleaning:
# Read in the imputed dataset without re-running the imputation:
df_imp <- readRDS(here("Data", "df_imp_allyears.rds")) 
df_imp_cleaned <- post_imp_data_cleaning(df_imp)


#Separately run sample loop (to create the cognitive measures) on the imputed data and not imputed data:
##Imputed data:
df_imp_only = subset(df_imp_cleaned, .imp>0) 
df_imp_only_cog <- sample_loop(df_imp_only) #Takes ~5min

#Not imputed data:
df_orig_only = subset(df_imp_cleaned, .imp==0)
df_orig_only_cog <- sample_loop(df_orig_only)

#Combine back together:
df_imp_combined <- rbind(df_imp_only_cog, df_orig_only_cog)
df_imp_combined <- df_imp_combined %>% rename(".imp" = "imp")

#Subset and label variables:
df_imp_combined_sub <- subset(df_imp_combined, select = -c(bc86, bc20, recall, drecall, ser7, memory, z_ser7, z_bc86, z_bc20, attention, mice_sample, global))
df_imp_combined_sub <- label_variables(df_imp_combined_sub)

saveRDS(df_imp_combined_sub, file=here("Data", "df_imp_final.rds"))


#------------------------------------------------------------------------------
# 2b. Create dataset for sensitivity analysis excluding people born before 1960
#------------------------------------------------------------------------------
# Perform post-imputation cleaning:
# Read in the imputed dataset without re-running the imputation:
df_imp_exclpre1960 <- readRDS(here("Data", "df_imp_exclpre1960.rds"))
df_imp_exclpre1960_cleaned <- post_imp_data_cleaning(df_imp_exclpre1960)


#Separately run sample loop (to create the cognitive measures) on the imputed data and not imputed data:
##Imputed data:
df_exclpre1960_imp_only = subset(df_imp_exclpre1960_cleaned, .imp>0)
df_exclpre1960_imp_only_cog <- sample_loop(df_exclpre1960_imp_only) #Takes ~5min

#Not imputed data:
df_exclpre1960_orig_only = subset(df_imp_exclpre1960_cleaned, .imp==0)
df_exclpre1960_orig_only_cog <- sample_loop(df_exclpre1960_orig_only)

#Combine back together:
df_exclpre1960_imp_combined <- rbind(df_exclpre1960_imp_only_cog, df_exclpre1960_orig_only_cog)
df_exclpre1960_imp_combined <- df_exclpre1960_imp_combined %>% rename(".imp" = "imp")

#Subset and label variables:
df_exclpre1960_imp_combined_sub <- subset(df_exclpre1960_imp_combined, select = -c(bc86, bc20, recall, drecall, ser7, memory, z_ser7, z_bc86, z_bc20, attention, mice_sample, global))
df_exclpre1960_imp_combined_sub <- label_variables(df_exclpre1960_imp_combined_sub)


saveRDS(df_exclpre1960_imp_combined_sub, file=here("Data", "df_imp_final_exclpre1960.rds"))

#------------------------------------------------------------------------------
# 2c. Additional data cleaning for complete case analysis
#------------------------------------------------------------------------------
# Read in imputed, cleaned dataset with cognitive measure processing, to subset for complete case analysis:
df_rds = readRDS(file=here("Data", "df_imp_final.rds"))
df_noimps = subset(df_rds, .imp==0) #Restrict to only non-imputed (doing this way 
#rather than using pre-imputation data given post-imputation data transformations)

#All observations of father absent are dropped due to missingness. Examine where this is coming from:
dad_absent_rows <- df_noimps %>% filter(dad_absent == "Dad absent or never known")
sapply(dad_absent_rows %>% 
         select(year_birth_c1957, race, sex, southernborn, mom_usborn, dad_usborn, 
                rural_bin, poverty, mom_pay, dad_pay, mom_skilled, dad_skilled, 
                mom_ed, dad_ed), function(x) mean(is.na(x)))


## Re-examine missingness to recode the parent variables to avoid excluding all the individuals with absent parents from the complete case analysis:
# Merge back in original variables for examining this:
raw_data = readRDS(file=here("Data", "preschool.rds"))
raw_vars <- raw_data %>%
  select(case_id, dad_occ_orig, mom_occ_orig, dad_pay_orig, mom_pay_orig, mom_usborn_orig, dad_usborn_orig,
         mom_ed_orig, dad_ed_orig)
raw_vars <- label_variables(raw_vars)

df_merged <- df_noimps %>%
  left_join(raw_vars, by = "case_id")

# Review individuals who are missing on parent variables:
##Parent nativity:
table(df_merged$mom_usborn_orig, useNA="always") 
# 11 people are level 3, which is 'never knew mom'
table(df_merged$dad_usborn_orig, useNA="always") 
# 130 people are level 3, which is 'never knew dad'

#Recode new versions:
df_merged <- df_merged %>%
  mutate(mom_usborn_cc = case_when(
    mom_usborn == "In the US" ~ "In the US",
    mom_usborn == "Other country" ~ "Other country",
    mom_usborn_orig == 3 ~ "Never knew mom",
    TRUE ~ NA
  ) %>% as.factor())
# table(df_merged$mom_usborn_cc, df_merged$mom_usborn_orig, useNA="always")

df_merged <- df_merged %>%
  mutate(dad_usborn_cc = case_when(
    dad_usborn == "In the US" ~ "In the US",
    dad_usborn == "Other country" ~ "Other country",
    dad_usborn_orig == 3 ~ "Never knew dad",
    TRUE ~ NA
  ) %>% as.factor())
# table(df_merged$dad_usborn_cc, df_merged$dad_usborn_orig, useNA="always")

##Parent education:
table(df_merged$mom_ed_orig, df_merged$neverknew_mom, useNA="always") 
table(df_merged$mom_ed_cat, df_merged$neverknew_mom, useNA="always") 
# 10 people who are missing on mom education were valid skips and are "never knew mom". (Note 1 person who was 'never knew mom' has valid education)
table(df_merged$dad_ed_orig, df_merged$neverknew_dad, useNA="always") 
table(df_merged$dad_ed_cat, df_merged$neverknew_dad, useNA="always") 
# 130 people who are missing on dad education were valid skips and are "never knew dad"

#Recode new versions:
df_merged <- df_merged %>%
  mutate(mom_ed_cat_cc = case_when(
    mom_ed_cat == "Mom < 8th grade" ~ "Mom < 8th grade",
    mom_ed_cat == "Mom 8th to < HS" ~ "Mom 8th to < HS",
    mom_ed_cat == "Mom Highschool" ~ "Mom Highschool",
    mom_ed_cat == "Mom > Highschool" ~ "Mom > Highschool",
    mom_ed_orig == -4 ~ "Never knew mom",
    TRUE ~ NA
  ) %>% as.factor())
# table(df_merged$mom_ed_cat_cc, df_merged$neverknew_mom, useNA="always")

df_merged <- df_merged %>%
  mutate(dad_ed_cat_cc = case_when(
    dad_ed_cat == "Dad < 8th grade" ~ "Dad < 8th grade",
    dad_ed_cat == "Dad 8th to < HS" ~ "Dad 8th to < HS",
    dad_ed_cat == "Dad Highschool" ~ "Dad Highschool",
    dad_ed_cat == "Dad > Highschool" ~ "Dad > Highschool",
    dad_ed_orig == -4 ~ "Never knew dad",
    TRUE ~ NA
  ) %>% as.factor())
# table(df_merged$dad_ed_cat_cc, df_merged$neverknew_dad, useNA="always")


##Parent occupation:
table(df_merged$mom_pay_orig, df_merged$mom_notpresent, useNA="always") 
# 102 people who are missing were valid skips and are "mom not present"
# An additional 1 who was 'invalid skip' also have mom not present - coding these also to be 'mom not present' level
table(df_merged$dad_pay_orig, df_merged$dad_notpresent, useNA="always")
# 1329 people who are missing were valid skips and are "dad not present"
# An additional 40 who were 'invalid skip' also have dad not present - coding these also to be 'dad not present' level

#Recode new versions:
df_merged <- df_merged %>%
  mutate(mom_pay_cc = case_when(
    mom_pay == "Mom unemployed" ~ "Mom unemployed",
    mom_pay == "Mom employed" ~ "Mom employed",
    mom_notpresent == "Mom not present"  ~ "Mom not present",
    TRUE ~ NA
  ) %>% as.factor())
# table(df_merged$mom_pay_cc, df_merged$mom_notpresent, useNA="always")
# table(df_merged$mom_pay_cc, df_merged$mom_pay, useNA="always")

df_merged <- df_merged %>%
  mutate(dad_pay_cc = case_when(
    dad_pay == "Dad unemployed" ~ "Dad unemployed",
    dad_pay == "Dad employed" ~ "Dad employed",
    dad_notpresent == "Dad not present" ~ "Dad not present",
    TRUE ~ NA
  ) %>% as.factor())
# table(df_merged$dad_pay_cc, df_merged$dad_notpresent, useNA="always")
# table(df_merged$dad_pay_cc, df_merged$dad_pay, useNA="always")


##Parent skilled:
table(df_merged$mom_occ_orig, df_merged$mom_notpresent, useNA="always") 
# 102 people who are missing were valid skips and are "mom not present"
# An additional 1 who was 'invalid skip' also have mom not present - coding these also to be 'mom not present' level
table(df_merged$dad_occ_orig, df_merged$dad_notpresent, useNA="always")
# 1330 people who are missing were valid skips and are "dad not present"
# An additional 40 who were 'invalid skip' also have dad not present - coding these also to be 'dad not present' level

#Recode new versions:
df_merged <- df_merged %>%
  mutate(mom_skilled_cc = case_when(
    mom_skilled == "Mom unskilled" ~ "Mom unskilled",
    mom_skilled == "Mom skilled" ~ "Mom skilled",
    mom_skilled == "Mom unemployed" ~ "Mom unemployed",
    mom_notpresent == "Mom not present" ~ "Mom not present",
    TRUE ~ NA
  ) %>% as.factor())
# table(df_merged$mom_skilled_cc, df_merged$mom_notpresent, useNA="always")
# table(df_merged$mom_skilled_cc, df_merged$mom_skilled, useNA="always")

df_merged <- df_merged %>%
  mutate(dad_skilled_cc = case_when(
    dad_skilled == "Dad unskilled" ~ "Dad unskilled",
    dad_skilled == "Dad skilled" ~ "Dad skilled",
    dad_skilled == "Dad unemployed" ~ "Dad unemployed",
    dad_notpresent == "Dad not present" ~ "Dad not present",
    TRUE ~ NA
  ) %>% as.factor())
# table(df_merged$dad_skilled_cc, df_merged$dad_notpresent, useNA="always")
# table(df_merged$dad_skilled_cc, df_merged$dad_skilled, useNA="always")


# Restrict to complete case analysis:
## Note: No imssingness on year of birth, race, or sex.
df_cc = df_merged %>% drop_na(c(year_birth_c1957, race, sex, southernborn, mom_usborn_cc, dad_usborn_cc,
                                rural_bin, poverty, mom_pay_cc, dad_pay_cc, mom_skilled_cc, dad_skilled_cc,
                                mom_ed_cat_cc, dad_ed_cat_cc)) 


# Create a version of cSES index for sensitivity analysis for complete case analysis:
df_cc$cSEScount_completecase <- rowSums(cbind(
  df_cc$poverty == "In poverty",
  df_cc$dad_pay_cc == "Dad unemployed",
  df_cc$mom_ed_cat_cc == "Mom < 8th grade",
  df_cc$dad_ed_cat_cc == "Dad < 8th grade",
  df_cc$neverknew_dad == "Never knew dad" | df_cc$dad_notpresent == "Dad not present"
), na.rm = TRUE)
# selected_cols <-c("poverty", "dad_pay", "mom_ed", "dad_ed", "neverknew_dad", "dad_notpresent", "cSEScount_completecase")
# na_rows_selected <- df_cc[, selected_cols]
# View(na_rows_selected)
# table(df_cc$cSEScount_completecase, useNA="always")

df_cc <- df_cc %>%
  dplyr::mutate(cSEScount_tri_completecase = case_when((cSEScount_completecase ==0) ~ 1,
                                                       (cSEScount_completecase >0 & cSEScount_completecase<3 ) ~ 2,
                                                       (cSEScount_completecase >=3 ) ~ 3))

df_cc$cSEScount_completecase <- factor(df_cc$cSEScount_completecase)

df_cc$cSEScount_tri_completecase <- factor(df_cc$cSEScount_tri_completecase,
                                           levels=c(1,2,3),
                                           labels=c("Lower disadvantage",
                                                    "Medium disadvantage",
                                                    "Higher disadvantage"))
table(df_cc$cSEScount_tri_completecase, useNA="always")

# Save dataset:
saveRDS(df_cc, file=here("Data", "df_completecase.rds"))
