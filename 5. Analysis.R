#------------------------------------------------------------------------------
# Project title: Is early childhood education associated with better midlife 
# cognition, especially for children facing socioeconomic marginalization?

# Script 5 of 5

# Code for all analysis for manuscript
# Code author: Whitney Wells
# Code reviewer: Jilly Hebert
# Last updated: Feb 26, 2026

# Sections:
# 1a. Declare covariate sets
# 1b. Set up helper function for forest plots

# 2. Generate Table 1

# 3. Run pooled models for models 1-3
# Outputs: Fig 1, eTable2 (detailed results), eFig 3 (memory & attention subdomains), eFig 6 panel A
#   3a. Helper functions
#   3b. Run models & output (Fig 1, eTable 2, eFig 3)
#   3b2. Run models & output - For sensitivity analysis excluding those born before 1960 (eFig 6 panel A)

# 4. Interaction: Output table of interaction terms
# Outputs: eTable 3 & 4
#   4a. Helper functions
#   4b. Run models & output (eTable 5 & 6)
#   4b2. Run models & output - For sensitivity analysis excluding those born before 1960 to support eFig 6 panel B&C

# 5. Interaction: Output graphs stratified (via rotating ref groups)
# Outputs: Fig, Fig 3, eFig 4 (by parent ed), eTable 5 & 6
#   5a. Helper functions
#   5b. Run models & output (Fig 2, Fig 3, eFig 4, eTable 5 & 6)
#   5b2. Run models & output - For sensitivity analysis excluding those born before 1960 (eFig 6 panel B & C)

# 6. eTable 1 (Showing missingness for each variable)

# 7. Supplement: Complete case analysis
# Outputs: eFig 5
# 7a. Complete case analysis - Declare covariate sets
# 7b. Complete case analysis - Pooled analysis 
#   7ba. Helper functions
#   7bb. Run models & output (eFig 5 Panel A)
# 7c. Complete case analysis - Interaction examination
#   7ca. Helper functions
#   7cb. Run models & output
# 7d. Complete case analysis - Stratified analysis
#   7da. Helper functions
#   7db. Run models & output (eFig 5 Panel B & C)

# 8. Examination of complete case sample vs those excluded from complete case sample


#------------------------------------------------------------------------------

library(here)
library(tidyverse)
library(glue)
library(gtsummary)
library(flextable)
library(ggplot2)
library(miceadds)
library(patchwork)
library(sandwich)
library(lmtest)
library(miceadds)
library(tibble)
library(officer)
source("Functions_rmph.R")
library(forcats)
library(Hmisc)
library(labelled)


#------------------------------------------------------------------------------
# 1a. Declare covariate sets
#------------------------------------------------------------------------------

covars1 <- c("year_birth_c1957", "race", "sex", "southernborn", "mom_usborn", "dad_usborn")
mod1_cov <- glue("{paste0(covars1, collapse='+')}")

covars2 <- c("year_birth_c1957", "race", "sex", "southernborn", "mom_usborn", "dad_usborn",
             "mom_ed", "dad_ed")
mod2_cov <- glue("{paste0(covars2, collapse='+')}")

covars3 <- c("year_birth_c1957", "race", "sex", "southernborn", "mom_usborn", "dad_usborn",
             "mom_ed", "dad_ed", "rural_bin", "poverty", "dad_skilled", "mom_skilled",
             "dad_absent", "mom_absent")
mod3_cov <- glue("{paste0(covars3, collapse='+')}")

#Covars for interaction model for effect modification by race/ethnicity & sex:
covarsracesex <- c("year_birth_c1957", "southernborn", "mom_usborn", "dad_usborn")
modracesex_cov <- glue("{paste0(covarsracesex, collapse='+')}")



#------------------------------------------------------------------------------
# 1b. Set up helper function for forest plots
#------------------------------------------------------------------------------

## Function for outputting forest plot:
forestplot <- function(results, level_var, ylab_str){
  results$level <- factor(results$level, 
                            levels = c("Neither", "Head Start", "Preschool"))
  colors <- c("#00B6EB", "#A58AFF", "#FB61D7")
  level_name <- "Preschool/Head Start"
  ggplot(results, aes(x = model, y = coef, color = level)) +
    geom_point(size = 4, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.1, linewidth = 0.8, 
                  position = position_dodge(width = 0.3)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = NULL, y = ylab_str) +
    scale_color_manual(values = colors, name = level_name) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 14), 
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),  
      axis.text.y = element_text(size = 12),
      legend.position = "bottom",  # Position legend at the bottom
      legend.title = element_text(size = 12),  
      legend.text = element_text(size = 10),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),   
      panel.border = element_rect(fill = NA, size = 0.5)
    ) 
}


#------------------------------------------------------------------------------
# 2. Generate Table 1
#------------------------------------------------------------------------------

# Table 1 showing summarized data after imputation

# Use this function from github to create pooled summary statistics:
# Reference: https://bookdown.org/rwnahhas/RMPH/mi-descriptives.html


df_rds = readRDS(file=here("Data", "df_imp_final.rds"))
df <-as.mids(df_rds)
mi.n.p.by(df, "cSEScount_tri",   BY = "presch_headstart")

table(df_rds$presch_headstart, df_rds$.imp, useNA="always")

mi.n.p(df, "presch_headstart")

#Store pooled estimates: 
#cont_data takes ~10min and #cat_data takes ~20min on my computer
cont_data <- rbind(mi.mean.se.sd.by(df, "year_birth", BY = "presch_headstart")[, c("mean.1", "sd.1", "mean.2", "sd.2", "mean.3", "sd.3")],
                   mi.mean.se.sd.by(df, "z_global", BY = "presch_headstart")[, c("mean.1", "sd.1", "mean.2", "sd.2", "mean.3", "sd.3")],
                   mi.mean.se.sd.by(df, "z_memory", BY = "presch_headstart")[, c("mean.1", "sd.1", "mean.2", "sd.2", "mean.3", "sd.3")],
                   mi.mean.se.sd.by(df, "z_attention", BY = "presch_headstart")[, c("mean.1", "sd.1", "mean.2", "sd.2", "mean.3", "sd.3")])

cat_data <- rbind(mi.n.p.by(df, "sex",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "race",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "southernborn",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "dad_usborn",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "mom_usborn",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "poverty",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "rural_bin",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "dad_ed_cat",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "mom_ed_cat",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "dad_skilled",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "mom_skilled",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "dad_absent",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "mom_absent",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")],
                  mi.n.p.by(df, "cSEScount_tri",   BY = "presch_headstart")[, c("n.1", "p.1", "n.2", "p.2", "n.3", "p.3")]
)

# Filter out dichotomous levels that aren't needed:
cat_data <- subset(cat_data, !(rownames(cat_data) %in% c("sex: Male", "southernborn: Non-south", "dad_usborn: Other country",
                                                         "mom_usborn: Other country", "poverty: Not in poverty", "rural_bin: In town or city",
                                                         "dad_absent: Dad present", "mom_absent: Mom present")))
cat_data


# Change format of numbers:
continuous_data_combined <- cont_data %>%
  mutate_at(vars(starts_with("mean")), function(x) {
    round(x, digits = 3)
  }) %>%
  mutate_at(vars(starts_with("sd")), function(x) {
    round(x, digits = 2)
  })
cont_frmt <- continuous_data_combined %>%
  mutate("Neither" = paste0(mean.1, " ± ", sd.1),
         "Head Start" = paste0(mean.2, " ± ", sd.2),
         "Preschool" = paste0(mean.3, " ± ", sd.3)) %>%
  select("Neither", "Head Start", "Preschool")

cat_frmt <- cat_data %>%
  mutate("Neither" = paste0(round(n.1), " (", round(p.1 * 100), "%)"),
         "Head Start" = paste0(round(n.2), " (", round(p.2 * 100), "%)"),
         "Preschool" = paste0(round(n.3), " (", round(p.3 * 100), "%)")) %>%
  select("Neither", "Head Start", "Preschool")

#Save:
tbl1 <- rbind(cont_frmt, cat_frmt)
ft <- flextable(tbl1 %>% rownames_to_column("Variable"))
ft
ft <- set_table_properties(ft, width = 0.8, layout = "autofit")  # Overall table width
ft <- width(ft, j = 1, width = 2)  # Set width for 'Variable' column
ft <- width(ft, j = 2, width = 1)  # Set width for 'Coef' column
save_as_docx(
  ft,
  path = here("Outputs", "Table1.docx"))


#------------------------------------------------------------------------------
# 3. Run pooled models for models 1-3
# Outputs: Fig 1, eTable2 (detailed results), eFig 3 (memory & attention subdomains), eFig 6 panel A
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 3a. Helper functions
#------------------------------------------------------------------------------

# Set-up helper function to extract coefficients into a df for pooled models, robust SEs:
extract_coefs <- function(df_list, outcome, outcome_name, exposure, ref_text, covars) {
  lm_model <- with(df_list, lm(as.formula(paste0(outcome, " ~ ", exposure, " + ", covars))))
  betas <- lapply(lm_model, coef) #For robust SE
  vars <- lapply(lm_model, FUN = function(x){vcovHC(x)}) #For robust SE
  pool_summary <- summary(pool_mi(betas,vars)) #For robust SE
  pool_summary
  
  # Only keep the results for the exposure
  rows <- grep(paste0("^", exposure), rownames(pool_summary))
  results <- pool_summary[rows, ]
  
  # Create a column called "level" that lists the exposure level
  results$level <- rownames(results)
  results <- results %>%
    mutate(level = sub(exposure, "", level))
  results <- results %>%
    select(level, everything())
  
  # Add a new column "model" 
  results$model <- outcome_name
  
  # Drop columns "se", "t", and "missInfo", rename columns
  results <- results[, !names(results) %in% c("se", "t", "missInfo", "p")]
  colnames(results)[colnames(results) == "(lower"] <- "lci"
  colnames(results)[colnames(results) == "upper)"] <- "uci"
  colnames(results)[colnames(results) == "results"] <- "coef"
  
  #Add row for ref level:
  new_row = data.frame(level = ref_text, coef = 0, lci = 0, uci = 0, model = outcome_name)
  results <- rbind(new_row, results)
  return(results)
}


#------------------------------------------------------------------------------
# 3b. Run models & output (Fig 1, eTable 2, eFig 3)
#------------------------------------------------------------------------------
df_rds = readRDS(file=here("Data", "df_imp_final.rds"))
df <-as.mids(df_rds)
df_list <- miceadds::mids2datlist(df)

# Run pooled models for models 1-3, for main outcome and secondary outcomes for supplement:
mod1_cog <- extract_coefs(df_list, "z_global", "Global cognition score", "presch_headstart", "Neither", mod1_cov)
mod2_cog <- extract_coefs(df_list, "z_global", "Global cognition score", "presch_headstart", "Neither", mod2_cov)
mod3_cog <- extract_coefs(df_list, "z_global", "Global cognition score", "presch_headstart", "Neither", mod3_cov)

mod1_mem <- extract_coefs(df_list, "z_memory", "Memory subdomain score", "presch_headstart", "Neither", mod1_cov)
mod2_mem <- extract_coefs(df_list, "z_memory", "Memory subdomain score", "presch_headstart", "Neither", mod2_cov)
mod3_mem <- extract_coefs(df_list, "z_memory", "Memory subdomain score", "presch_headstart", "Neither", mod3_cov)

mod1_att <- extract_coefs(df_list, "z_attention", "Attention subdomain score", "presch_headstart", "Neither", mod1_cov)
mod2_att <- extract_coefs(df_list, "z_attention", "Attention subdomain score", "presch_headstart", "Neither", mod2_cov)
mod3_att <- extract_coefs(df_list, "z_attention", "Attention subdomain score", "presch_headstart", "Neither", mod3_cov)


# Store models 1-3 in a dataframe for each outcome:
mod123_cog <-rbind(mod1_cog, mod2_cog, mod3_cog)
mod123_cog$model <- rep(c("Baseline Model", "Partially SES-Adjusted Model", "Fully SES-Adjusted Model"), 
                                      times = c(nrow(mod1_cog), nrow(mod2_cog), nrow(mod3_cog)))
mod123_mem <-rbind(mod1_mem, mod2_mem, mod3_mem)
mod123_mem$model <- rep(c("Baseline Model", "Partially SES-Adjusted Model", "Fully SES-Adjusted Model"), 
                          times = c(nrow(mod1_mem), nrow(mod2_mem), nrow(mod3_mem)))
mod123_att<-rbind(mod1_att, mod2_att, mod3_att)
mod123_att$model <- rep(c("Baseline Model", "Partially SES-Adjusted Model", "Fully SES-Adjusted Model"), 
                          times = c(nrow(mod1_att), nrow(mod2_att), nrow(mod3_att)))


# Output forest plot (Fig 1):
fp_mod123_cog <- forestplot(mod123_cog, "presch_headstart", "Global cognition score")
fp_mod123_cog2 <- fp_mod123_cog + scale_y_continuous(limits = c(-0.75, 0.55), breaks = seq(-0.75, 0.55, by = 0.25))
fp_mod123_cog2
ggsave(here("Outputs", "Figure 1.png"), plot = fp_mod123_cog2, width = 8, height = 6, dpi = 300)


## Memory and attention forest plots for supplement (eFig 3):
fp_mod123_mem <- forestplot(mod123_mem, "presch_headstart", "Memory subdomain score")
fp_mod123_mem
ggsave(here("Outputs", "eFig3_mem.png"), plot = fp_mod123_mem, width = 8, height = 6, dpi = 300)

fp_mod123_att <- forestplot(mod123_att, "presch_headstart", "Attention subdomain score")
fp_mod123_att
ggsave(here("Outputs", "eFig3_att.png"), plot = fp_mod123_att, width = 8, height = 6, dpi = 300)



# Output global cognition to word doc (eTable 2):
#Round:
mod123_cog[c("coef", "lci", "uci")] <- round(mod123_cog[c("coef", "lci", "uci")], 2)

#Combine estimate & CI into a single cell:
mod123_cog$result <- paste(mod123_cog$coef, " (", mod123_cog$lci, ", ", mod123_cog$uci, ")", sep = "")
mod123_cog <- subset(mod123_cog, select = -c(lci, uci))

#Output:
ft <- flextable(mod123_cog)
ft <- set_table_properties(ft, width = 0.8, layout = "autofit")  # Overall table width
ft <- width(ft, j = 1, width = 2)  # Set width for 'Variable' column
ft <- width(ft, j = 2, width = 1)  # Set width for 'Coef' column
doc <- read_docx()
doc <- doc %>%
  body_add_flextable(ft)
print(doc, target = here("Outputs", "Fig1_detailed.docx")) # Save the document



# Output memory and attention to word doc for including results in manuscript:
#Round:
mod123_mem[c("coef", "lci", "uci")] <- round(mod123_mem[c("coef", "lci", "uci")], 2)
mod123_att[c("coef", "lci", "uci")] <- round(mod123_att[c("coef", "lci", "uci")], 2)

#Combine estimate & CI into a single cell:
mod123_mem$result <- paste(mod123_mem$coef, " (", mod123_mem$lci, ", ", mod123_mem$uci, ")", sep = "")
mod123_mem <- subset(mod123_mem, select = -c(lci, uci))

mod123_att$result <- paste(mod123_att$coef, " (", mod123_att$lci, ", ", mod123_att$uci, ")", sep = "")
mod123_att <- subset(mod123_att, select = -c(lci, uci))

#Output mem:
ft <- flextable(mod123_mem)
ft <- set_table_properties(ft, width = 0.8, layout = "autofit")  # Overall table width
ft <- width(ft, j = 1, width = 2)  # Set width for 'Variable' column
ft <- width(ft, j = 2, width = 1)  # Set width for 'Coef' column
doc <- read_docx()
doc <- doc %>%
  body_add_flextable(ft)
print(doc, target = here("Outputs", "Fig1_detailed_mem.docx"))

#Output att:
ft <- flextable(mod123_att)
ft <- set_table_properties(ft, width = 0.8, layout = "autofit")  # Overall table width
ft <- width(ft, j = 1, width = 2)  # Set width for 'Variable' column
ft <- width(ft, j = 2, width = 1)  # Set width for 'Coef' column
doc <- read_docx()
doc <- doc %>%
  body_add_flextable(ft)
print(doc, target = here("Outputs", "Fig1_detailed_att.docx"))




#------------------------------------------------------------------------------
# 3b2. Run models & output - For sensitivity analysis excluding those born before 1960 (eFig 6 panel A)
#------------------------------------------------------------------------------
df_rds = readRDS(file=here("Data", "df_imp_final_exclpre1960.rds")) 
table(df_rds$.imp, useNA="always")
df <-as.mids(df_rds)
df_list <- miceadds::mids2datlist(df)


# Run pooled models for models 1-3:
mod1_cog <- extract_coefs(df_list, "z_global", "Global cognition score", "presch_headstart", "Neither", mod1_cov)
mod2_cog <- extract_coefs(df_list, "z_global", "Global cognition score", "presch_headstart", "Neither", mod2_cov)
mod3_cog <- extract_coefs(df_list, "z_global", "Global cognition score", "presch_headstart", "Neither", mod3_cov)

# Store models 1-3 in a dataframe for each outcome:
mod123_cog <-rbind(mod1_cog, mod2_cog, mod3_cog)
mod123_cog$model <- rep(c("Baseline Model", "Partially SES-Adjusted Model", "Fully SES-Adjusted Model"),
                         times = c(nrow(mod1_cog), nrow(mod2_cog), nrow(mod3_cog)))

# Output forest plots:
fp_mod123_cog <- forestplot(mod123_cog, "presch_headstart", "Global cognition score")
fp_mod123_cog
ggsave(here("Outputs", "eFig6panelA_1960.png"), plot = fp_mod123_cog, width = 8, height = 6, dpi = 300)


#------------------------------------------------------------------------------
# 4. Interaction: Output table of interaction terms
# Outputs: eTable 5 & 6
#------------------------------------------------------------------------------
## Note: Using rotating reference groups, because causes issues when trying to stratify on a
## variable that is imputed, because each set of .imp then has a different # of observations

#------------------------------------------------------------------------------
# 4a. Helper functions
#------------------------------------------------------------------------------

# Helper function for extracting interaction terms:
extract_interactions <- function(df_list, outcome, exposure, thirdvar, covars) {
  lm_model <- with(df_list, lm(as.formula(paste0(outcome, " ~ ", exposure, "*", thirdvar, " + ", covars))))
  betas <- lapply(lm_model, coef) #For robust SE
  vars <- lapply(lm_model, FUN = function(x){vcovHC(x)}) #For robust SE
  pool_summary <- summary(pool_mi(betas,vars)) #For robust SE
  return(pool_summary)
}

# Helper function to process interaction results and output to word table
process_interaction_mod <- function(pool_summary, filename, exposure, thirdvar) {
  pool_summary <- as.data.frame(pool_summary)
  pool_summary <- rownames_to_column(pool_summary, var = "Variable")

  # Drop unnecessary columns and only keep rows for exposure, EM, interaction term
  pool_summary <- subset(pool_summary, select = -c(se, t, missInfo))
  
  # First filter for rows starting with exposure or thirdvar without a colon
  no_colon_rows <- subset(pool_summary, grepl(paste0("^", exposure, "[^:]*$|^", thirdvar, "[^:]*$"), Variable))
  # Second filter for rows that match exposure:thirdvar
  interaction_rows <- subset(pool_summary, grepl(paste0("^", exposure, ".*", thirdvar), Variable))
  # Combine the two filters
  pool_summary <- rbind(no_colon_rows, interaction_rows)

  # Round columns to 2 decimal places
  pool_summary[c("results", "p", "(lower", "upper)")] <- round(pool_summary[c("results", "p", "(lower", "upper)")], 2)

  # Concatenate CI
  pool_summary$`95% CI` <- paste("(", pool_summary$`(lower`, ", ", pool_summary$`upper)`, ")", sep = "")
  pool_summary <- subset(pool_summary, select = -c(`(lower`, `upper)`))

  # Reorder columns & rename
  pool_summary <- pool_summary[, c("Variable", "results", "95% CI", "p")]
  pool_summary <- pool_summary %>% rename(Coef = results)

  # Remove variable name from the "Variable" column, keeping only the label
  pool_summary$Variable <- gsub(paste0("^", exposure), "", pool_summary$Variable)
  pool_summary$Variable <- gsub(paste0("^", thirdvar), "", pool_summary$Variable)

  # Replace ":" with "*" in interaction terms
  pool_summary$Variable <- gsub(paste0(":", thirdvar), " * ", pool_summary$Variable)

  #Output to word:
  ft <- flextable(pool_summary)
  ft <- set_table_properties(ft, width = 0.8, layout = "autofit")  # Overall table width
  ft <- width(ft, j = 1, width = 2)  # Set width for 'Variable' column
  ft <- width(ft, j = 2, width = 1)  # Set width for 'Coef' column
  doc <- read_docx()
  doc <- doc %>%
    body_add_flextable(ft)
  print(doc, target = here("Outputs", filename)) # Save the document
}

#Helper function to run the anova comparison for nested models with and without interaction term:
examine_EM <- function(df_list, outcome, exposure, thirdvar, covars) {
  lm_mod1 <- with(df_list, lm(as.formula(paste0(outcome, " ~ ", exposure, "*", thirdvar, " + ", covars))))
  print(summary(pool(lm_mod1)))
  lm_mod0 <- with(df_list, lm(as.formula(paste0(outcome, " ~ ", exposure, "+", thirdvar, " + ", covars))))
  print(summary(pool(lm_mod0)))
  #anova(lm_mod1, lm_mod0, test = "LRT") #Cannot use Anova for models with multiple imputation, using pool.compare instead
  #pool.compare(lm_mod1, lm_mod0) #Deprecated, directs to use D1 or D3 (which rely on testModels from mitml)
  D3(lm_mod1, lm_mod0) #Using D3 to independently conduct in each imputation, then pool
  #Note: this doesn't explicitly include robust SEs, but does it independently in each imputation & then pools, which should be sufficient
  #(And the individual interaction values with CIs include robust SEs explicitly)
}


#------------------------------------------------------------------------------
# 4b. Run models & output (eTable 5 & 6)
#------------------------------------------------------------------------------
df_rds = readRDS(file=here("Data", "df_imp_final.rds"))

df_rds$racesex <- relevel(df_rds$racesex, ref = "Female White")

df <-as.mids(df_rds)
df_list <- miceadds::mids2datlist(df)

# Run models
racesex_int_mod_cog <- extract_interactions(df_list, "z_global", "presch_headstart", "racesex", modracesex_cov)
racesex_int_mod_cog
ses_int_mod_cog <- extract_interactions(df_list, "z_global", "presch_headstart", "cSEScount_tri", mod1_cov)
ses_int_mod_cog

daded_cat_int_mod_cog <- extract_interactions(df_list, "z_global", "presch_headstart", "dad_ed_cat", mod1_cov)
daded_cat_int_mod_cog
momed_cat_int_mod_cog <- extract_interactions(df_list, "z_global", "presch_headstart", "mom_ed_cat", mod1_cov)
momed_cat_int_mod_cog

# Output word docs (eTable 5 & 6)
process_interaction_mod(racesex_int_mod_cog, "eTable6.docx", "presch_headstart", "racesex")
process_interaction_mod(ses_int_mod_cog, "eTable5.docx", "presch_headstart", "cSEScount_tri")
# Output word docs for sensitivity analysis supporting eFig 4:
process_interaction_mod(daded_cat_int_mod_cog, "daded_int.docx", "presch_headstart", "dad_ed_cat")
process_interaction_mod(momed_cat_int_mod_cog, "momed_int.docx", "presch_headstart", "mom_ed_cat")

# Get p-values for overall interaction terms:
examine_EM(df_list, "z_global", "presch_headstart", "racesex", modracesex_cov) # statistic=2.51, p=0.005
examine_EM(df_list, "z_global", "presch_headstart", "cSEScount_tri", mod1_cov) # statistic=1.15, p=0.33
# Get p-values for overall interaction terms:
examine_EM(df_list, "z_global", "presch_headstart", "dad_ed_cat", mod1_cov) # statistic=0.50, p=0.81
examine_EM(df_list, "z_global", "presch_headstart", "mom_ed_cat", mod1_cov) # statistic=1.62, p=0.14


#------------------------------------------------------------------------------
# 4b2. Run models & output - For sensitivity analysis excluding those born before 1960 to support eFig 6 panel B&C
#------------------------------------------------------------------------------
df_rds = readRDS(file=here("Data", "df_imp_final_exclpre1960.rds"))
table(df_rds$.imp, useNA="always")

df_rds$racesex <- relevel(df_rds$racesex, ref = "Female White")

df <-as.mids(df_rds)
df_list <- miceadds::mids2datlist(df)

# Run models 
racesex_int_mod_cog <- extract_interactions(df_list, "z_global", "presch_headstart", "racesex", modracesex_cov)
racesex_int_mod_cog
ses_int_mod_cog <- extract_interactions(df_list, "z_global", "presch_headstart", "cSEScount_tri", mod1_cov)
ses_int_mod_cog

# Output word docs for sensitivity analysis supporting eFig 6 panel B&C:
process_interaction_mod(racesex_int_mod_cog, "racesex_int_1960.docx", "presch_headstart", "racesex")
process_interaction_mod(ses_int_mod_cog, "ses_int_1960.docx", "presch_headstart", "cSEScount_tri")


#------------------------------------------------------------------------------
# 5. Interaction: Output graphs stratified (via rotating ref groups)
# Outputs: Fig 3, eFig 4 (by parent ed), eTable 5 & 6
#------------------------------------------------------------------------------
## Note: Using rotating reference groups, because causes issues when trying to stratify on a
## variable that is imputed, because each set of .imp then has a different # of observations

#------------------------------------------------------------------------------
# 5a. Helper functions
#------------------------------------------------------------------------------

# Set-up helper function to extract coefficients into a df (with rotating ref groups for subgroups), robust SEs:
extract_subgroup_coefs <- function(df_rds, outcome, outcome_name, covars, subgroupvar, ref_text) {
  List <- as.character(na.omit(levels(df_rds[[subgroupvar]])))
  mod_out <- data.frame(outcome = character(), level = character(), 
                        coef = numeric(), lci = numeric(), uci = numeric(), model = character(),
                        stringsAsFactors = FALSE)
  
  for (i in 1:length(List)){
    df_rds[[subgroupvar]] <- relevel(df_rds[[subgroupvar]], ref = List[i])
    df <-as.mids(df_rds)
    df_list <- miceadds::mids2datlist(df)
    
    lm_model <- with(df_list, lm(as.formula(paste0(outcome, " ~ presch_headstart*",subgroupvar, " + ", covars))))
    print(lm_model)
    betas <- lapply(lm_model, coef) #For robust SE
    vars <- lapply(lm_model, FUN = function(x){vcovHC(x)}) #For robust SE
    pool_summary <- summary(pool_mi(betas,vars)) #For robust SE
    
    coef <- pool_summary["presch_headstartHead Start","results"]
    lci <- pool_summary["presch_headstartHead Start","(lower"]
    uci <- pool_summary["presch_headstartHead Start","upper)"]
    temp_mod_out_HS <- data.frame(outcome = outcome_name, level = "Head Start", 
                                  coef, lci, uci, model = List[i], stringsAsFactors = FALSE)
    temp_mod_out_HS <- temp_mod_out_HS[, c("outcome", "level", "coef", "lci", "uci", "model")]
    
    coef <- pool_summary["presch_headstartPreschool","results"]
    lci <- pool_summary["presch_headstartPreschool","(lower"]
    uci <- pool_summary["presch_headstartPreschool","upper)"]
    temp_mod_out_PS <- data.frame(outcome = outcome_name, level = "Preschool", 
                                  coef, lci, uci, model = List[i], stringsAsFactors = FALSE)
    temp_mod_out_PS <- temp_mod_out_PS[, c("outcome", "level", "coef", "lci", "uci", "model")]
    
    mod_out <- rbind(mod_out, temp_mod_out_HS)
    mod_out <- rbind(mod_out, temp_mod_out_PS)
    
    #Add row for ref level:
    ref_row = data.frame(outcome = outcome_name, level = ref_text, coef = 0, lci = 0, uci = 0, 
                         model = List[i])
    mod_out <- rbind(mod_out, ref_row)
  }
  return(mod_out)
}


#------------------------------------------------------------------------------
# 5b. Run models & output (Fig 2, Fig 3, eFig 4, eTable 3 & 4)
#------------------------------------------------------------------------------

df_rds = readRDS(file=here("Data", "df_imp_final.rds"))

df <-as.mids(df_rds)
mi.n.p(df, "racesex")
mi.n.p(df, "cSEScount_tri")
mi.n.p(df, "dad_ed_cat")
mi.n.p(df, "mom_ed_cat")


#Change labels & Add sample sizes:
df_rds$racesex <- dplyr::recode(df_rds$racesex,
                                "Male White" = "White men (n=1,824)",
                                "Male Black" = "Black men (n=1,128)",
                                "Male Hispanic" = "Hispanic men (n=539)",
                                "Female White" = "White women (n=1,890)",
                                "Female Black" = "Black women (n=1,179)",
                                "Female Hispanic" = "Hispanic women (n=569)")

df_rds$cSEScount_tri <- dplyr::recode(df_rds$cSEScount_tri,
                                "Lower disadvantage" = "Lower marginalization (n=3,889)",
                                "Medium disadvantage" = "Medium marginalization (n=2,185)",
                                "Higher disadvantage" = "Higher marginalization (n=1,054)")

df_rds$dad_ed_cat <- dplyr::recode(df_rds$dad_ed_cat,
                                "Dad < 8th grade" = "Dad < 8th grade (n=1,192)",
                                "Dad 8th to < HS" = "Dad 8th to < HS (n=1,972)",
                                "Dad Highschool" = "Dad Highschool (n=2,398)",
                                "Dad > Highschool" = "Dad > Highschool (n=1,556)")

df_rds$mom_ed_cat <- dplyr::recode(df_rds$mom_ed_cat,
                                   "Mom < 8th grade" = "Mom < 8th grade (n=805)",
                                   "Mom 8th to < HS" = "Mom 8th to < HS (n=2,243)",
                                   "Mom Highschool" = "Mom Highschool (n=2,878)",
                                   "Mom > Highschool" = "Mom > Highschool (n=1,203)")

## By Race/ethnicity and sex:
#Model:
racesex_int_cog <- extract_subgroup_coefs(df_rds, "z_global", "Cognition score", modracesex_cov, "racesex", "Neither")
racesex_int_cog

# Output forest plots (Fig 3):
racesex_int_cog$model <- factor(racesex_int_cog$model, levels = c("Black women (n=1,179)", "Hispanic women (n=569)", "White women (n=1,890)", 
                                                                  "Black men (n=1,128)", "Hispanic men (n=539)", "White men (n=1,824)"))

fp_racesex_int_cog <- forestplot(racesex_int_cog, "presch_headstart", "Global cognition score")
fp_racesex_int_cog <- fp_racesex_int_cog + 
  scale_y_continuous(limits = c(-0.75, 0.45), breaks = seq(-0.75, 0.45, by = 0.2))  # Set limits and tick marks
fp_racesex_int_cog

#Update dataframe, manually adding significance stars based on interaction table:
#Only including stars for any results where interaction p-value<=0.1
racesex_int_cog$significance <- NA
racesex_int_cog$significance[racesex_int_cog$level == "Head Start" & racesex_int_cog$model == "Black men (n=1,128)"] <- "**" #Sig < 0.05
racesex_int_cog$significance[racesex_int_cog$level == "Head Start" & racesex_int_cog$model == "White men (n=1,824)"] <- "*" #Sig < 0.1
racesex_int_cog$significance[racesex_int_cog$level == "Head Start" & racesex_int_cog$model == "Hispanic men (n=539)"] <- "*" #Sig < 0.1
racesex_int_cog

# Generate the plot and add asterisks at the top of the confidence interval:
fp_racesex_int_cog_star <- fp_racesex_int_cog +
  geom_text(
    data = subset(racesex_int_cog, !is.na(significance)),   # Only add where significance is not NA
    aes(x = model, y = uci, label = significance),      # Position at the upper confidence interval
    color = "#A58AFF",                                  
    size = 6,                                           
    vjust = -0.5                                        
  )
fp_racesex_int_cog_star2 <- fp_racesex_int_cog_star + scale_y_continuous(limits = c(-0.75, 0.55), breaks = seq(-0.75, 0.55, by = 0.25))
fp_racesex_int_cog_star2
ggsave(here("Outputs", "Figure 3.png"), plot = fp_racesex_int_cog_star2, width = 14, height = 6, dpi = 300)




## By family SES:
ses_int_cog <- extract_subgroup_coefs(df_rds, "z_global", "Cognition score", mod1_cov, "cSEScount_tri", "Neither")
ses_int_cog

# Output forest plots (Fig 2):
ses_int_cog$model <- factor(ses_int_cog$model, levels = c("Higher marginalization (n=1,054)", "Medium marginalization (n=2,185)", "Lower marginalization (n=3,889)")) 
fp_ses_int_cog <- forestplot(ses_int_cog, "presch_headstart", "Global cognition score")
fp_ses_int_cog

#Update dataframe, manually adding significance stars based on interaction table:
# Only including stars for any results where interaction p-value<=0.1
ses_int_cog$significance <- NA
ses_int_cog$significance[ses_int_cog$level == "Head Start" & ses_int_cog$model == "Higher marginalization (n=1,054)"] <- "*" #Sig < 0.1

# Generate the plot and add asterisks at the top of the confidence interval:
fp_ses_int_cog_star <- fp_ses_int_cog +
  geom_text(
    data = subset(ses_int_cog, !is.na(significance)),   # Only add where significance is not NA
    aes(x = model, y = uci, label = significance),      # Position at the upper confidence interval
    color = "#A58AFF",                                  
    size = 6,                                           
    vjust = -0.5                                        
  )
fp_ses_int_cog_star2 <- fp_ses_int_cog_star + scale_y_continuous(limits = c(-0.75, 0.55), breaks = seq(-0.75, 0.55, by = 0.25))
fp_ses_int_cog_star2
ggsave(here("Outputs", "Figure 2.png"), plot = fp_ses_int_cog_star2, width = 10, height = 6, dpi = 300)




# Output racesex and SES detailed tables to word docs (eTable 3 & 4):
## By Race/ethnicity and sex:
#Round:
racesex_int_cog[c("coef", "lci", "uci")] <- round(racesex_int_cog[c("coef", "lci", "uci")], 2)

#Combine estimate & CI into a single cell:
racesex_int_cog$result <- paste(racesex_int_cog$coef, " (", racesex_int_cog$lci, ", ", racesex_int_cog$uci, ")", sep = "")
racesex_int_cog <- subset(racesex_int_cog, select = -c(lci, uci))

ft <- flextable(racesex_int_cog)

ft <- set_table_properties(ft, width = 0.8, layout = "autofit")  # Overall table width
ft <- width(ft, j = 1, width = 2)  # Set width for 'Variable' column
ft <- width(ft, j = 2, width = 1)  # Set width for 'Coef' column

doc <- read_docx()
doc <- doc %>%
  body_add_flextable(ft)

# Save the document
print(doc, target = here("Outputs", "Fig3_detailed.docx"))


## By SES:
#Round:
ses_int_cog[c("coef", "lci", "uci")] <- round(ses_int_cog[c("coef", "lci", "uci")], 2)

#Combine estimate & CI into a single cell:
ses_int_cog$result <- paste(ses_int_cog$coef, " (", ses_int_cog$lci, ", ", ses_int_cog$uci, ")", sep = "")
ses_int_cog <- subset(ses_int_cog, select = -c(lci, uci))

ft <- flextable(ses_int_cog)

ft <- set_table_properties(ft, width = 0.8, layout = "autofit")  # Overall table width
ft <- width(ft, j = 1, width = 2)  # Set width for 'Variable' column
ft <- width(ft, j = 2, width = 1)  # Set width for 'Coef' column

doc <- read_docx()
doc <- doc %>%
  body_add_flextable(ft)

# Save the document
print(doc, target = here("Outputs", "Fig2_detailed.docx"))




#Parent education:
# For supplement
#------------------------------------------------------------------------------
daded_int_cog_mod1 <- extract_subgroup_coefs(df_rds, "z_global", "Cognition score", mod1_cov, "dad_ed_cat", "Neither")
momed_int_cog_mod1 <- extract_subgroup_coefs(df_rds, "z_global", "Cognition score", mod1_cov, "mom_ed_cat", "Neither")

# Output forest plots:
daded_int_cog_mod1$model <- factor(daded_int_cog_mod1$model, levels = c("Dad < 8th grade (n=1,192)", "Dad 8th to < HS (n=1,972)", "Dad Highschool (n=2,398)", "Dad > Highschool (n=1,556)")) 
fp_daded_int_cog_mod1 <- forestplot(daded_int_cog_mod1, "presch_headstart", "Global cognition score")
fp_daded_int_cog_mod1
ggsave(here("Outputs", "eFig4_dad.png"), plot = fp_daded_int_cog_mod1, width = 10, height = 6, dpi = 300)

momed_int_cog_mod1$model <- factor(momed_int_cog_mod1$model, levels = c("Mom < 8th grade (n=805)", "Mom 8th to < HS (n=2,243)", "Mom Highschool (n=2,878)", "Mom > Highschool (n=1,203)")) 
fp_momed_int_cog_mod1 <- forestplot(momed_int_cog_mod1, "presch_headstart", "Global cognition")
fp_momed_int_cog_mod1
ggsave(here("Outputs", "eFig4_mom.png"), plot = fp_momed_int_cog_mod1, width = 10, height = 6, dpi = 300)






#------------------------------------------------------------------------------
# 5b2. Run models & output - For sensitivity analysis excluding those born before 1960 (eFig 6 panel B & C)
#------------------------------------------------------------------------------
df_rds = readRDS(file=here("Data", "df_imp_final_exclpre1960.rds")) 

table(df_rds$.imp, useNA="always")

df <-as.mids(df_rds)
mi.n.p(df, "racesex")
mi.n.p(df, "cSEScount_tri")


#Change labels & Add sample sizes:
df_rds$racesex <- dplyr::recode(df_rds$racesex,
                                "Male White" = "White men (n=1,228)",
                                "Male Black" = "Black men (n=765)",
                                "Male Hispanic" = "Hispanic men (n=374)",
                                "Female White" = "White women (n=1,220)",
                                "Female Black" = "Black women (n=792)",
                                "Female Hispanic" = "Hispanic women (n=392)")

df_rds$cSEScount_tri <- dplyr::recode(df_rds$cSEScount_tri,
                                      "Lower disadvantage" = "Lower marginalization (n=2,586)",
                                      "Medium disadvantage" = "Medium marginalization (n=1,408)",
                                      "Higher disadvantage" = "Higher marginalization (n=777)")

## By Race/ethnicity and sex:
#Model:
racesex_int_cog <- extract_subgroup_coefs(df_rds, "z_global", "Cognition score", modracesex_cov, "racesex", "Neither")
racesex_int_cog

# Output forest plots:
racesex_int_cog$model <- factor(racesex_int_cog$model, levels = c("Black women (n=792)", "Hispanic women (n=392)", "White women (n=1,220)", 
                                                                  "Black men (n=765)", "Hispanic men (n=374)", "White men (n=1,228)"))

fp_racesex_int_cog <- forestplot(racesex_int_cog, "presch_headstart", "Global cognition score")
fp_racesex_int_cog <- fp_racesex_int_cog + 
  scale_y_continuous(limits = c(-0.75, 0.75), breaks = seq(-0.75, 0.75, by = 0.25))  # Set limits and tick marks
fp_racesex_int_cog

#Update dataframe, manually adding significance stars based on interaction table:
# Only including stars for any results where interaction p-value<=0.1
racesex_int_cog$significance <- NA
racesex_int_cog$significance[racesex_int_cog$level == "Head Start" & racesex_int_cog$model == "Black men (n=765)"] <- "**" #Sig < 0.05
racesex_int_cog

# Generate the plot and add asterisks at the top of the confidence interval:
fp_racesex_int_cog_star <- fp_racesex_int_cog +
  geom_text(
    data = subset(racesex_int_cog, !is.na(significance)),   # Only add where significance is not NA
    aes(
      x = model, 
      y = uci, 
      label = significance
    ),
    # Add a horizontal shift only for Preschool and Black men because it's not aligning properly
    position = position_nudge(
      x = ifelse(
        subset(racesex_int_cog, !is.na(significance))$level == "Preschool" & subset(racesex_int_cog, !is.na(significance))$model == "Black men (n=765)", 
        0.1, 0)
    ),
    color = ifelse(
      subset(racesex_int_cog, !is.na(significance))$level == "Head Start", "#A58AFF",
      ifelse(subset(racesex_int_cog, !is.na(significance))$level == "Preschool", "#FB61D7", "#00B6EB")
    ), # Color based on 'level'
    size = 6,             # Adjust size of the asterisk
    vjust = -0.5         # Slightly above the CI line
  )
fp_racesex_int_cog_star
ggsave(here("Outputs", "eFig6PanelC_1960.png"), plot = fp_racesex_int_cog_star, width = 14, height = 6, dpi = 300)



## By family SES:
ses_int_cog <- extract_subgroup_coefs(df_rds, "z_global", "Cognition score", mod1_cov, "cSEScount_tri", "Neither")
ses_int_cog

# Output forest plots:
# Don't need to add significance stars because nothing with interaction p-value <=0.1
ses_int_cog$model <- factor(ses_int_cog$model, levels = c("Higher marginalization (n=777)", "Medium marginalization (n=1,408)", "Lower marginalization (n=2,586)")) 
fp_ses_int_cog <- forestplot(ses_int_cog, "presch_headstart", "Global cognition score")
fp_ses_int_cog
ggsave(here("Outputs", "eFig6PanelB_1960.png"), plot = fp_ses_int_cog, width = 10, height = 6, dpi = 300)




#------------------------------------------------------------------------------
# 6. eTable 1 (Showing missingness for each variable)
#------------------------------------------------------------------------------
df_rds = readRDS(file=here("Data", "df_imp_final.rds"))
df_orig = subset(df_rds, .imp==0)

df_orig <- df_orig %>% 
  mutate(southernborn = factor(southernborn) %>% forcats::fct_explicit_na(na_level = "Missing"),
         dad_usborn = factor(dad_usborn) %>% forcats::fct_explicit_na(na_level = "Missing"),
         mom_usborn = factor(mom_usborn) %>% forcats::fct_explicit_na(na_level = "Missing"),
         poverty = factor(poverty) %>% forcats::fct_explicit_na(na_level = "Missing"),
         rural_bin = factor(rural_bin) %>% forcats::fct_explicit_na(na_level = "Missing"),
         dad_ed_cat = factor(dad_ed_cat) %>% forcats::fct_explicit_na(na_level = "Missing"),
         mom_ed_cat = factor(mom_ed_cat) %>% forcats::fct_explicit_na(na_level = "Missing"),
         dad_skilled = factor(dad_skilled) %>% forcats::fct_explicit_na(na_level = "Missing"),
         mom_skilled = factor(mom_skilled) %>% forcats::fct_explicit_na(na_level = "Missing"),
         dad_absent = factor(dad_absent) %>% forcats::fct_explicit_na(na_level = "Missing"),
         mom_absent = factor(mom_absent) %>% forcats::fct_explicit_na(na_level = "Missing"))

label(df_orig$z_global) <- "Global cognition score"
label(df_orig$z_memory) <- "Memory subdomain score"
label(df_orig$z_attention) <- "Attention subdomain score"

list("style_number-arg:big.mark" = "") %>%
  set_gtsummary_theme()

tbl1 <- tbl_summary(
  df_orig, 
  by = presch_headstart, 
  include=c(year_birth, sex, race, southernborn, dad_usborn, mom_usborn, 
            poverty, rural_bin, dad_ed_cat, mom_ed_cat, dad_skilled, mom_skilled,
            dad_absent, mom_absent, z_global, z_memory, z_attention),
  statistic = list(
    c(z_global, z_memory, z_attention) ~ c("{mean} ± {sd}", "{N_miss} ({p_miss}%)"),
    c(year_birth) ~ c("{mean} ± {sd}"),
    c(sex, race) ~ c("{p}%"),
    c(southernborn, dad_usborn, mom_usborn, 
      poverty, rural_bin, dad_ed_cat, mom_ed_cat, dad_skilled, mom_skilled,
      dad_absent, mom_absent) ~ c("{p}%")
  ),
  type = c(year_birth, z_global, z_memory, z_attention) ~ "continuous2",
  digits = list(
    year_birth ~ c(0, 1)
  ),
  missing = "no"
) %>%  
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "N missing (% missing)",
                   "Missing N (%)",
                   label)
  ) %>%
  as_flex_table()

# Set background color to white
tbl1 <- bg(tbl1, bg = "white", part = "all")
tbl1

#Save
save_as_image(tbl1, path = here("Outputs", "Appendix_Table1.png"))
save_as_docx(
  tbl1,
  path = here("Outputs", "Appendix_Table1.docx"))


#------------------------------------------------------------------------------
# 7. Supplement: Complete case analysis
# Outputs: eFig 5
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 7a. Complete case analysis - Declare covariate sets
#------------------------------------------------------------------------------

# Set up vectors for covariates - slightly different sets of covariates for the complete case analysis vs imputation:
# (For mod3 covars, parent present/absent not included as already included as categories in other variables)

covars1_cc <- c("year_birth_c1957", "race", "sex", "southernborn", "mom_usborn_cc", "dad_usborn_cc")
mod1_cov_cc <- glue("{paste0(covars1_cc, collapse='+')}")

covars2_cc <- c("year_birth_c1957", "race", "sex", "southernborn", "mom_usborn_cc", "dad_usborn_cc",
                "mom_ed_cat_cc", "dad_ed_cat_cc")
mod2_cov_cc <- glue("{paste0(covars2_cc, collapse='+')}")

covars3_cc <- c("year_birth_c1957", "race", "sex", "southernborn", "mom_usborn_cc", "dad_usborn_cc",
                "mom_ed_cat_cc", "dad_ed_cat_cc", "rural_bin", "poverty", "dad_skilled_cc", "mom_skilled_cc")
mod3_cov_cc <- glue("{paste0(covars3_cc, collapse='+')}")

#Covars for interaction analysis with racesex:
covarsracesex_cc <- c("year_birth_c1957", "southernborn", "mom_usborn_cc", "dad_usborn_cc")
modracesex_cov_cc <- glue("{paste0(covarsracesex_cc, collapse='+')}")


#------------------------------------------------------------------------------
# 7b. Complete case analysis - Pooled analysis
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 7ba. Helper functions
#------------------------------------------------------------------------------
# Function to extract coefficients and adjust model names
extract_coefs_completecase <- function(df, outcome, outcome_name, exposure, ref_text, covars) {
  lm_model <- lm(as.formula(paste0(outcome, " ~ ", exposure, " + ", covars)), data = df)
  summary(lm_model)
  summary <- coeftest(lm_model, vcov = vcovHC(lm_model)) # Robust SEs using vcovHC
  print(summary)
  
  # Only keep the results for the exposure
  rows <- grep(paste0("^", exposure), rownames(summary))
  results <- summary[rows, ]
  
  # Create a column called "level" that lists the exposure level
  results <- as.data.frame(results)
  results$level <- rownames(results)
  results <- results %>%
    mutate(level = sub(exposure, "", level))
  results <- results %>%
    select(level, everything())
  
  # Add a new column "model" 
  results$model <- outcome_name
  
  # Add cols for lci and uci 
  results$lci <- results$Estimate - 1.96 * results$`Std. Error`
  results$uci <- results$Estimate + 1.96 * results$`Std. Error`
  
  # Drop columns "se" and "t", and "p" and rename column
  results <- results[, !names(results) %in% c("Std. Error", "t value", "missInfo", "Pr(>|t|)")]
  colnames(results)[colnames(results) == "Estimate"] <- "coef"
  
  print(results)
  
  #Add row for ref level:
  new_row = data.frame(level = ref_text, coef = 0, lci = 0, uci = 0, model = outcome_name)
  results <- rbind(new_row, results)
  return(results)
}

#------------------------------------------------------------------------------
# 7bb. Run models & output (eFig 5 Panel A)
#------------------------------------------------------------------------------

df_cc = readRDS(file=here("Data", "df_completecase.rds"))

#Note for mod3, one observation causing challenges for robust estimation, so excluding:
dim(df_cc)
df_cc_excludedObs <- df_cc[-c(3507), ]
dim(df_cc_excludedObs)

#Run models:
mod1_cog_cc <- extract_coefs_completecase(df_cc_excludedObs, "z_global", "Global cognition score", "presch_headstart", "Neither", mod1_cov_cc)
mod2_cog_cc <- extract_coefs_completecase(df_cc_excludedObs, "z_global", "Global cognition score", "presch_headstart", "Neither", mod2_cov_cc)
mod3_cog_cc <- extract_coefs_completecase(df_cc_excludedObs, "z_global", "Global cognition score", "presch_headstart", "Neither", mod3_cov_cc)

# Store models 1-3 in a dataframe for each outcome:
mod123_cog_cc <-rbind(mod1_cog_cc, mod2_cog_cc, mod3_cog_cc)
mod123_cog_cc$model <- rep(c("Baseline Model", "Partially SES-Adjusted Model", "Fully SES-Adjusted Model"),
                         times = c(nrow(mod1_cog_cc), nrow(mod2_cog_cc), nrow(mod3_cog_cc)))

# Output forest plots:
fp_mod123_cog_cc <- forestplot(mod123_cog_cc, "presch_headstart", "Global cognition score")
fp_mod123_cog_cc
ggsave(here("Outputs", "eFig5panelA_cc.png"), plot = fp_mod123_cog_cc, width = 8, height = 6, dpi = 300)

#------------------------------------------------------------------------------
# 7c. Complete case analysis - Interaction examination
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 7ca. Helper functions
#------------------------------------------------------------------------------

# Define function for extracting interaction terms:
extract_interactions_cc <- function(df, outcome, exposure, thirdvar, covars) {
  lm_model <- lm(as.formula(paste0(outcome, " ~ ", exposure, "*", thirdvar, " + ", covars)), data=df)
  summary <- coeftest(lm_model, vcov = vcovHC(lm_model)) # Robust SEs using vcovHC
  summary <- as.data.frame(summary[,])
  print(summary)
  summary$lci <- summary$Estimate - 1.96 * summary$`Std. Error`
  summary$uci <- summary$Estimate + 1.96 * summary$`Std. Error`
  summary <- summary[, !names(summary) %in% c("Std. Error", "t value")]
  return(summary)
}


# Define the function to process interaction results and output to word table
process_interaction_mod_cc <- function(pool_summary, filename, exposure, thirdvar) {
  pool_summary <- as.data.frame(pool_summary)
  pool_summary <- rownames_to_column(pool_summary, var = "Variable")
  
  # First filter for rows starting with exposure or thirdvar without a colon
  no_colon_rows <- subset(pool_summary, grepl(paste0("^", exposure, "[^:]*$|^", thirdvar, "[^:]*$"), Variable))
  # Second filter for rows that match exposure:thirdvar
  interaction_rows <- subset(pool_summary, grepl(paste0("^", exposure, ".*", thirdvar), Variable))
  # Combine the two filters
  pool_summary <- rbind(no_colon_rows, interaction_rows)
  
  print(pool_summary)
  
  # Round columns to 2 decimal places
  pool_summary[c("Estimate", "Pr(>|t|)", "lci", "uci")] <- round(pool_summary[c("Estimate", "Pr(>|t|)", "lci", "uci")], 2)
  
  # Concatenate CI
  pool_summary$`95% CI` <- paste("(", pool_summary$lci, ", ", pool_summary$uci, ")", sep = "")
  pool_summary <- subset(pool_summary, select = -c(lci, uci))
  
  # Reorder columns & rename
  pool_summary <- pool_summary[, c("Variable", "Estimate", "95% CI", "Pr(>|t|)")]
  
  # Remove variable name from the "Variable" column, keeping only the label
  pool_summary$Variable <- gsub(paste0("^", exposure), "", pool_summary$Variable)
  pool_summary$Variable <- gsub(paste0("^", thirdvar), "", pool_summary$Variable)
  
  # Replace ":" with "*" in interaction terms
  pool_summary$Variable <- gsub(paste0(":", thirdvar), " * ", pool_summary$Variable)
  
  #Output to word:
  ft <- flextable(pool_summary)
  
  ft <- set_table_properties(ft, width = 0.8, layout = "autofit")  # Overall table width
  ft <- width(ft, j = 1, width = 2)  # Set width for 'Variable' column
  ft <- width(ft, j = 2, width = 1)  # Set width for 'Coef' column
  
  doc <- read_docx()
  doc <- doc %>%
    body_add_flextable(ft)
  
  # Save the document
  print(doc, target = here("Outputs", filename))
}


#------------------------------------------------------------------------------
# 7cb. Run models & output
#------------------------------------------------------------------------------
df_cc = readRDS(file=here("Data", "df_completecase.rds"))

df_cc$racesex <- relevel(df_cc$racesex, ref = "Female White")

#Run models & output to word
racesex_int_mod_cog_cc <- extract_interactions_cc(df_cc, "z_global", "presch_headstart", "racesex", modracesex_cov_cc)
racesex_int_mod_cog_cc
process_interaction_mod_cc(racesex_int_mod_cog_cc, "racesex_int_cc.docx", "presch_headstart", "racesex")

ses_int_mod1_cog_cc <- extract_interactions_cc(df_cc, "z_global", "presch_headstart", "cSEScount_tri_completecase", mod1_cov_cc)
ses_int_mod1_cog_cc
process_interaction_mod_cc(ses_int_mod1_cog_cc, "ses_int_cc.docx", "presch_headstart", "cSEScount_tri_completecase")



#------------------------------------------------------------------------------
# 7d. Complete case analysis - Stratified analysis
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 7da. Helper functions
#------------------------------------------------------------------------------
#Function for extracting subgroup coefs with rotating ref groups, fully interacted
extract_subgroup_coefs_cc <- function(df, outcome, outcome_name, covars, subgroupvar, ref_text) {
  #Prep for cycling through levels of thirdvar:
  List <- as.character(na.omit(levels(df[[subgroupvar]])))
  mod_out <- data.frame(outcome = character(), level = character(), 
                        coef = numeric(), lci = numeric(), uci = numeric(), model = character(),
                        stringsAsFactors = FALSE)
  
  for (i in 1:length(List)){
    df[[subgroupvar]] <- relevel(df[[subgroupvar]], ref = List[i])
    lm_model <- lm(as.formula(paste0(outcome, " ~ presch_headstart*",subgroupvar, " + ", covars)), data=df)
    summary <- coeftest(lm_model, vcov = vcovHC(lm_model)) # Robust SEs using vcovHC
    summary <- as.data.frame(summary[,])
    
    summary$lci <- summary$Estimate - 1.96 * summary$`Std. Error`
    summary$uci <- summary$Estimate + 1.96 * summary$`Std. Error`
    
    print(summary)
    
    coef <- summary["presch_headstartHead Start","Estimate"]
    lci <- summary["presch_headstartHead Start","lci"]
    uci <- summary["presch_headstartHead Start","uci"]
    
    temp_mod_out_HS <- data.frame(outcome = outcome_name, level = "Head Start", 
                                  coef, lci, uci, model = List[i], stringsAsFactors = FALSE)
    temp_mod_out_HS <- temp_mod_out_HS[, c("outcome", "level", "coef", "lci", "uci", "model")]
    
    coef <- summary["presch_headstartPreschool","Estimate"]
    lci <- summary["presch_headstartPreschool","lci"]
    uci <- summary["presch_headstartPreschool","uci"]
    temp_mod_out_PS <- data.frame(outcome = outcome_name, level = "Preschool", 
                                  coef, lci, uci, model = List[i], stringsAsFactors = FALSE)
    temp_mod_out_PS <- temp_mod_out_PS[, c("outcome", "level", "coef", "lci", "uci", "model")]
    
    mod_out <- rbind(mod_out, temp_mod_out_HS)
    mod_out <- rbind(mod_out, temp_mod_out_PS)
    
    #Add row for ref level:
    ref_row = data.frame(outcome = outcome_name, level = ref_text, coef = 0, lci = 0, uci = 0, 
                         model = List[i])
    mod_out <- rbind(mod_out, ref_row)
  }
  return(mod_out)
}


#------------------------------------------------------------------------------
# 7db. Run models & output (eFig 5 Panel B & C)
#------------------------------------------------------------------------------
df_cc = readRDS(file=here("Data", "df_completecase.rds"))

table(df_cc$racesex)
table(df_cc$cSEScount_tri_completecase)

# Change labels:
df_cc$racesex <- dplyr::recode(df_cc$racesex,
                                "Male White" = "White men (n=1,291)",
                                "Male Black" = "Black men (n=611)",
                                "Male Hispanic" = "Hispanic men (n=305)",
                                "Female White" = "White women (n=1,259)",
                                "Female Black" = "Black women (n=650)",
                                "Female Hispanic" = "Hispanic women (n=341)")

df_cc$cSEScount_tri_completecase <- dplyr::recode(df_cc$cSEScount_tri_completecase,
                               "Lower disadvantage" = "Lower marginalization (n=2,591)",
                               "Medium disadvantage" = "Medium marginalization (n=1,603)",
                               "Higher disadvantage" = "Higher marginalization (n=263)")


racesex_int_cog_cc <- extract_subgroup_coefs_cc(df_cc, "z_global", "Cognition score", modracesex_cov_cc, "racesex", "Neither")
racesex_int_cog_cc

racesex_int_cog_cc$model <- factor(racesex_int_cog_cc$model, levels = c("Black women (n=650)", "Hispanic women (n=341)", "White women (n=1,259)", 
                                                                  "Black men (n=611)", "Hispanic men (n=305)", "White men (n=1,291)"))

fp_racesex_int_cog_cc <- forestplot(racesex_int_cog_cc, "presch_headstart", "Global cognition score")
fp_racesex_int_cog_cc

#Update dataframe, manually adding significance stars based on interaction table:
# Only including stars for any results where interaction p-value<=0.1
racesex_int_cog_cc$significance <- NA
racesex_int_cog_cc$significance[racesex_int_cog_cc$level == "Head Start" & racesex_int_cog_cc$model == "White men (n=1,291)"] <- "**" #Sig < 0.05
racesex_int_cog_cc$significance[racesex_int_cog_cc$level == "Head Start" & racesex_int_cog_cc$model == "Hispanic men (n=305)"] <- "*" #Sig < 0.1
racesex_int_cog_cc

# Generate the plot and add asterisks at the top of the confidence interval:
fp_racesex_int_cog_cc_star <- fp_racesex_int_cog_cc +
  geom_text(
    data = subset(racesex_int_cog_cc, !is.na(significance)),   # Only add where significance is not NA
    aes(x = model, y = uci, label = significance),      # Position at the upper confidence interval
    color = "#A58AFF",                                  
    size = 6,                                           
    vjust = -0.5                                        
  )
fp_racesex_int_cog_cc_star

fp_racesex_int_cog_cc_star <- fp_racesex_int_cog_cc_star +
  scale_y_continuous(limits = c(-0.8, 1.0), breaks = seq(-0.8, 1.0, by = 0.2))  # Set limits and tick marks
fp_racesex_int_cog_cc_star

ggsave(here("Outputs", "eFig5PanelC_cc.png"), plot = fp_racesex_int_cog_cc_star, width = 14, height = 6, dpi = 300)


ses_int_cog_mod1_cc <- extract_subgroup_coefs_cc(df_cc, "z_global", "Cognition score", mod1_cov_cc, "cSEScount_tri_completecase", "Neither")
ses_int_cog_mod1_cc$model <- factor(ses_int_cog_mod1_cc$model, levels = c("Higher marginalization (n=263)", "Medium marginalization (n=1,603)", "Lower marginalization (n=2,591)"))

fp_ses_int_cog_mod1_cc <-forestplot(ses_int_cog_mod1_cc, "presch_headstart", "Global cognition score")
fp_ses_int_cog_mod1_cc
ggsave(here("Outputs", "eFig5PanelB_cc.png"), plot = fp_ses_int_cog_mod1_cc, width = 10, height = 6, dpi = 300)






#------------------------------------------------------------------------------
# 8. Examination of complete case sample vs those excluded from complete case sample
#------------------------------------------------------------------------------

#Only included for context regarding complete case analysis for supplement.

df_cc = readRDS(file=here("Data", "df_completecase.rds"))

df_rds = readRDS(file=here("Data", "df_imp_final.rds"))
df_noimps = subset(df_rds, .imp==0)

df_not_in_cc <- anti_join(df_noimps, df_cc, by = "case_id")

table(df_cc$presch_headstart)
table(df_noimps$presch_headstart)
table(df_not_in_cc$presch_headstart)

var_label(df_cc$racesex) <- "Sex, race and ethnicity"
var_label(df_noimps$racesex) <- "Sex, race and ethnicity"
var_label(df_not_in_cc$racesex) <- "Sex, race and ethnicity"

tbl_cc <- tbl_summary(
  df_cc,
  include = c(year_birth, racesex, southernborn, dad_usborn, mom_usborn, 
              poverty, rural_bin, dad_ed_cat, mom_ed_cat, dad_skilled,
              mom_skilled, dad_absent, mom_absent, cSEScount_tri),
  type = list(year_birth ~ "continuous", 
              c(racesex, southernborn, dad_usborn, mom_usborn, 
                poverty, rural_bin, dad_ed_cat, mom_ed_cat, dad_skilled,
                mom_skilled, dad_absent, mom_absent, cSEScount_tri) ~ "categorical"),
  statistic = list(all_continuous() ~ "{mean} ({sd})", 
                   all_categorical() ~ "{n} ({p}%)")
) %>%
  bold_labels() %>%
  as_flex_table()

save_as_docx(
  tbl_cc,
  path = here("Outputs", "Complete case distribution.docx"))



tbl_excl_cc <- tbl_summary(
  df_not_in_cc,
  include = c(year_birth, racesex, southernborn, dad_usborn, mom_usborn, 
              poverty, rural_bin, dad_ed_cat, mom_ed_cat, dad_skilled,
              mom_skilled, dad_absent, mom_absent, cSEScount_tri),
  type = list(year_birth ~ "continuous", 
              c(racesex, southernborn, dad_usborn, mom_usborn, 
                poverty, rural_bin, dad_ed_cat, mom_ed_cat, dad_skilled,
                mom_skilled, dad_absent, mom_absent, cSEScount_tri) ~ "categorical"),
  statistic = list(all_continuous() ~ "{mean} ({sd})", 
                   all_categorical() ~ "{n} ({p}%)")
) %>%
  bold_labels()%>%
  as_flex_table()

save_as_docx(
  tbl_excl_cc,
  path = here("Outputs", "Excluded complete case distribution.docx"))



tbl_imp <- tbl_summary(
  df_noimps,
  include = c(year_birth, racesex, southernborn, dad_usborn, mom_usborn, 
              poverty, rural_bin, dad_ed_cat, mom_ed_cat, dad_skilled,
              mom_skilled, dad_absent, mom_absent, cSEScount_tri),
  type = list(year_birth ~ "continuous", 
              c(racesex, southernborn, dad_usborn, mom_usborn, 
                poverty, rural_bin, dad_ed_cat, mom_ed_cat, dad_skilled,
                mom_skilled, dad_absent, mom_absent, cSEScount_tri) ~ "categorical"),
  statistic = list(all_continuous() ~ "{mean} ({sd})", 
                   all_categorical() ~ "{n} ({p}%)")
) %>%
  bold_labels()%>%
  as_flex_table()


save_as_docx(
  tbl_imp,
  path = here("Outputs", "Imputation distribution.docx"))


