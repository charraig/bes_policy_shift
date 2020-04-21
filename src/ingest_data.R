# library import ----------------------------------------------------------
library(haven)
library(readr)
library(dplyr)
library(labelled)
library(stringr)
library(tidyr)

# Housing: profile_house_tenure, then housing
# Obj Hardship Job: workingStatus, then profile_work_stat
# Social Class: profile_socialgrade_cie, then profile_work_type

output_file = 'data/survey_data.rds'
if (file.exists(output_file)) {
  relevant_df <- readRDS(output_file)
} else {
  # ingest data -------------------------------------------------------------
  raw_df <- haven::read_dta(
    # "data/BES2017_W14_Panel_v0.3.dta", 
    "data/BES2019_W16_Panel_v0.3.dta"
  )
  
  # Slim down to variables of interest
  selected_df <- raw_df %>% 
    select(
      id,
      # Metadata
      matches("^wave\\d+$"),
      matches("^country$"),
      # Outcome Variables
      matches("lr\\d+"),
      # Predictor Variables
      matches("^ageW", ignore.case = FALSE),  # age
      matches("^education"),  # education
      matches("^gender$"),  # gender
      matches("profile_house_tenure"),  # housing
      matches("housing"),  # housing
      matches("profile_gross_household"),  # income & objective hardship-income
      matches("workingStatus"),  # objective hardship-job
      matches("profile_work_stat"),  # objective hardship-job
      matches("profile_work_type"),  # social class
      matches("profile_socgrade"),  # social class
      matches("riskPoverty"),  # subjective hardship - finsec
      matches("riskUnemployment"),  # subjective hardship - jobsec
    )
  
  # Convert to long form of wave response -----------------------------------
  
  # Convert id & wave indicator data into long form
  # so that we can apply filters to select the right respondents
  # Respondents must:
  # (1) Live in England
  # (2) Have responded to all waves
  relevant_df <- 
    selected_df %>% 
    labelled::remove_val_labels() %>% 
    dplyr::filter(country == 1) %>%
    tidyr::drop_na(matches("lr\\d+"))
  
  # Save for later use.
  saveRDS(relevant_df, file = output_file)
}
