#------------------------------------------------------------------------------
# Project title: Is early childhood education associated with better midlife 
# cognition, especially for children facing socioeconomic marginalization?

# Script 3 of 5 

# Code for merging in outcome data and performing imputation
# Code author: Whitney Wells
# Code reviewer: Jilly Hebert
# Last updated: Feb 26, 2026

# Sections:
# 1a. Set up data files for merging
# 1b. Restrict to analytical sample (for imputation)
# 1c. Restrict to analytical sample (for imputation, for sensitivity analysis excluding those born before 1960)

# 2. Set up function for imputation

# 3a. Create imputed dataset for main analysis (eFig 1)
# 3b. Create imputed dataset for sensitivity analysis excluding people born before 1960


#------------------------------------------------------------------------------

library(here)
library(tidyverse)
library(glue)
library(png)
library(rsvg)
library(DiagrammeR)
library(DiagrammeRsvg)
library(mice)

#------------------------------------------------------------------------------
# 1a. Set up data files for merging
#------------------------------------------------------------------------------

cog_raw <- readRDS(here("Data", "cognitive_raw_cleaned.rds"))

preschool_indicatormissing <- readRDS(here("Data", "preschool_indicatormissing.rds"))

#------------------------------------------------------------------------------
# 1b. Restrict to analytical sample (for imputation)
#------------------------------------------------------------------------------

merged_df <- full_join(preschool_indicatormissing, cog_raw, by = "case_id")

# Restrict to eligible sample (present for cognition measures in NLSY): 
elig_df = merged_df %>% drop_na(c(cog_year))

# Restrict to eligible US-born sample (born in the US): 
elig_us_df <- subset(elig_df, usborn==1)

# Restrict to non-missing on exposure: 
df = elig_us_df %>% drop_na(c(presch_headstart))


#### Create sample flow diagram:
n <- dim(merged_df)
n_elig <- dim(elig_df)
n_elig_us <-dim(elig_us_df)
n_final <- dim(df)
n_excluded_exp <- n_elig_us - n_final

flow_diag <- grViz(
  glue("digraph flowchart {{ 
      graph[splines = ortho]
      node [fontname = Helvetica, shape = box, width = 4, height = 1]
      
        node1[label = <Participants in NLSY79 Wave 1<br/>n={n}>]
        
        node2[label = <Present for cognitive measures<br/>n={n_elig}>]
        node1 -> node2;
        
        node3[label = <Born in US<br/>n={n_elig_us}>]
        node2 -> node3;
                
        blank1[label = '', width = 0.01, height = 0.01]
        excluded1[label = <Missing exposure<br/>n={n_excluded_exp}>]
        
        node3 -> blank1[dir = none];
        blank1 -> excluded1[minlen = 2];
        {{ rank = same; blank1 excluded1 }}
        
        node4[label = <Analytical sample<br/>n={n_final}>]
        blank1 -> node4;
     }}")
)
flow_diag

# export graph
export_svg(flow_diag) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG(here("Outputs", "flow diagram_allyears.png"))


# Recode southern born to exclude the "outside US" category since no observations after exclusion:
df$southernborn <-factor(df$southernborn, levels = c("Non-south", "South"))
df <- subset(df, select = -c(usborn))
saveRDS(df, file=here("Data", "df_indicatormissing_allyears.rds"))




#------------------------------------------------------------------------------
# 1c. Restrict to analytical sample (for imputation, for sensitivity analysis excluding those born before 1960)
#------------------------------------------------------------------------------

# Restrict to eligible sample (present for cognition measures in NLSY): 
# Already performed above (elig_df) 

# Restrict to eligible US-born sample (born in the US): 
# Already performed above (elig_us_df) 

# Excluding those born before 1960
elig_us_df_1960 <- subset(elig_us_df, year_birth_c1957>=3)

# Restrict to non-missing on exposure: 
df_excl1960 = elig_us_df_1960 %>% drop_na(c(presch_headstart))


#### Create sample flow diagram:
n <- dim(merged_df)
n_elig <- dim(elig_df)
n_elig_us <-dim(elig_us_df)
n_elig_us_1960 <- dim(elig_us_df_1960) 
n_final <- dim(df_excl1960)
n_excluded_exp <- n_elig_us_1960 - n_final 

flow_diag <- grViz(
  glue("digraph flowchart {{ 
      graph[splines = ortho]
      node [fontname = Helvetica, shape = box, width = 4, height = 1]
      
        node1[label = <Participants in NLSY79 Wave 1<br/>n={n}>]
        
        node2[label = <Present for cognitive measures<br/>n={n_elig}>]
        node1 -> node2;
        
        node3[label = <Born in US<br/>n={n_elig_us}>]
        node2 -> node3;
        
        node4[label = <Born in 1960 or later<br/>n={n_elig_us_1960}>]
        node3 -> node4;
                
        blank1[label = '', width = 0.01, height = 0.01]
        excluded1[label = <Missing exposure<br/>n={n_excluded_exp}>]
        
        node4 -> blank1[dir = none];
        blank1 -> excluded1[minlen = 2];
        {{ rank = same; blank1 excluded1 }}
        
        node5[label = <Analytical sample<br/>n={n_final}>]
        blank1 -> node5;
     }}")
)
flow_diag

# export graph
export_svg(flow_diag) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG(here("Outputs", "flow diagram.png"))


# Recode southern born to exclude the "outside US" category since no observations after exclusion:
df_excl1960$southernborn <-factor(df_excl1960$southernborn, levels = c("Non-south", "South"))
df_excl1960 <- subset(df_excl1960, select = -c(usborn))
saveRDS(df_excl1960, file=here("Data", "df_indicatormissing_exclpre1960.rds"))


#------------------------------------------------------------------------------
# 2. Set up function for imputation
#------------------------------------------------------------------------------
imputation_function <- function(df) {
  
  #Factor variables that are not pmm 
  factor_vars <- c("southernborn", "rural_bin", "poverty", 
                   "mom_notpresent", "dad_notpresent", "neverknew_mom", "neverknew_dad", 
                   "bc86_adj", "bc20_adj") 
  df[factor_vars] <- lapply(df[factor_vars], factor)
  # Note: haven't specified the method because everything used the correct 
  # method when R chose (after specifying the factor variables)
  
  #Create predictor matrix (override collinearity)
  mice_predictor_matrix <- mice(df, maxit = 0, seed = 123)
  predictor_matrix <- mice_predictor_matrix$predictorMatrix
  
  #Set which variables will not be used to predict other variables
  #Cog year not used for analysis, so don't need to include:
  predictor_matrix["cog_year", ] <- 0
  predictor_matrix[, "cog_year"] <- 0
  
  #Dad and mom skilled are partially derived from dad and mom pay, so excluding 
  predictor_matrix["dad_pay", "dad_skilled"] <- 0
  predictor_matrix["mom_pay", "mom_skilled"] <- 0
  predictor_matrix["dad_skilled", "dad_pay"] <- 0
  predictor_matrix["mom_skilled", "mom_pay"] <- 0
  
  #Removing these as everyone who is 'never knew' would also be 'not present'
  predictor_matrix["neverknew_mom", "mom_notpresent"] <- 0
  predictor_matrix["mom_notpresent", "neverknew_mom"] <- 0
  predictor_matrix["neverknew_dad", "dad_notpresent"] <- 0
  predictor_matrix["dad_notpresent", "neverknew_dad"] <- 0
  
  # Note: Also received warnings for the following, but parent presence 
  # indicator variables are likely truly strongly correlated and should be used 
  # to inform the imputation
  # 1) parent_usborn & never_knew_parent
  # 2) mom_pay/mom_skilled & mom_notpresent
  # 3) dad_ed & neverknew_dad
  
  imps <- mice(df, maxit = 15, m=200, 
               predictorMatrix = predictor_matrix, seed = 123) #Actual imputation - takes 1.5hr on my computer
  imps$loggedEvents
  imps$method #Check the method is correct for every variable
  ##pmm = predictive mean matching (continuous variable)
  ##logreg = log regression (binary variable)
  ##polyreg = polytomous regression (unordered categorical variable)
  ##polr = polytomous regression (ordered categorical variable)
  
  #Save completed imputations (imputed datasets plus original missing dataset)
  df_imp <- complete(imps, action = 'long', include = T) #Include original missing (.imp = 0)
  
  return(df_imp)
}



#------------------------------------------------------------------------------
# 3a. Create dataset for main analysis (with imputation) (eFig 1)
#------------------------------------------------------------------------------
# Examine test imputation first to check imputation and predictor matrix, 
# logged events, and imputed variable distributions:
# 
# # Read in the file:
# df <- readRDS(here("Data", "df_indicatormissing_allyears.rds"))
# 
# #Factor variables that are not pmm 
# factor_vars <- c("southernborn", "rural_bin", "poverty", 
#                  "mom_notpresent", "dad_notpresent", "neverknew_mom", "neverknew_dad", 
#                  "bc86_adj", "bc20_adj") 
# df[factor_vars] <- lapply(df[factor_vars], factor)
# 
# #Create predictor matrix (override collinearity)
# mice_predictor_matrix <- mice(df, maxit = 0, seed = 123)
# predictor_matrix <- mice_predictor_matrix$predictorMatrix
# 
# test_imps <- mice(df, maxit = 1, m=1,
#                   predictorMatrix = predictor_matrix, seed = 123) #Test imputation
# test_imps$method
# test_imps$loggedEvents
# 
# densityplot(test_imps, ~ dad_pay)
# densityplot(test_imps, ~ dad_skilled)
# df_test_imp <- complete(test_imps, action = 'long', include = T)
# table(df_test_imp %>% group_by(.imp) %>% dplyr::select(dad_pay)) #(0 = original distribution)
# table(df_test_imp %>% group_by(.imp) %>% dplyr::select(dad_skilled)) #(0 = original distribution)




# Run the imputation function and save a version of the imputed dataset with post-imputation cleaning not yet performed:
# Imputation function takes ~1.5hr on my computer

# Read in the file:
df <- readRDS(here("Data", "df_indicatormissing_allyears.rds"))

df_imp <- imputation_function(df) 
saveRDS(df_imp, file=here("Data", "df_imp_allyears.rds")) 

#Inspect variable distributions after running the imputation - specify variables:
# table(df_imp %>% group_by(.imp) %>% dplyr::select(mom_ed)) #(0 original distribution) - for categorical variables
# imps <- as.mids(df_imp)
# densityplot(imps, ~ dad_pay)
# densityplot(imps, ~ mom_ed | .imp, subset = .imp <= 5) # Examine 5 fits





#------------------------------------------------------------------------------
# 3b. Create dataset for sensitivity analysis excluding people born before 1960
#------------------------------------------------------------------------------
# Read in the file for sensitivity analysis:
df_exclpre1960 <- readRDS(here("Data", "df_indicatormissing_exclpre1960.rds"))

# Run the imputation function and save a version of the imputed dataset with post-imputation cleaning not yet performed:
# Imputation function takes ~1.5hr on my computer
df_imp_exclpre1960 <- imputation_function(df_exclpre1960)
saveRDS(df_imp_exclpre1960, file=here("Data", "df_imp_exclpre1960.rds"))
