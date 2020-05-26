# ----------------------------------------------------------
# Library import 
# ----------------------------------------------------------
library(haven)
library(readr)
library(dplyr)
library(labelled)
library(stringr)
library(tidyr)

# ----------------------------------------------------------
# Ingest data from Stata file (.dta)
# ----------------------------------------------------------
# Ingest from raw data only if the specified cache file (.rds) is not present
# Otherwise, ingest from the cache file
# If ingesting raw, down-select the necessary columns, do basic filtering,
#   then save result to the cache.

output_file = 'data/survey_data.rds'
if (file.exists(output_file)) {
  relevant_df <- readRDS(output_file)
} else {
  # ingest data
  raw_df <- haven::read_dta(
    "data/BES2019_W16_Panel_v0.3.dta"
  )
  
  # Down-select to variables of interest
  selected_df <- raw_df %>% 
    select(
      # Metadata
      id,
      matches("^country$"),
      matches("^wave\\d+$"),
      # Outcome Variables
      matches("lr\\d+"),
      # Predictor Variables
      matches("^ageW", ignore.case = FALSE),  # age
      matches("^education"),  # education
      matches("^gender$"),  # gender
      matches("profile_house_tenure"),  # housing
      matches("housing"),  # housing
      matches("profile_gross_household"),  # income & objhard_income
      matches("workingStatus"),  # objhard_job
      matches("profile_work_stat"),  # objhard_job
      matches("profile_work_type"),  # social_class
      matches("profile_socgrade"),  # social_class
      matches("econPersonalRetro"),  # subjhard_income
      matches("riskUnemployment"),  # subjhard_job
    )
  
  # Convert id & wave indicator data into long form
  # so that we can apply filters to select the right respondents
  # Respondents must:
  # (1) Live in England
  # (2) Have responded to all waves
  # Also replace `9999` values with NA
  relevant_df <- 
    selected_df %>% 
    labelled::remove_val_labels() %>% 
    dplyr::filter(country == 1) %>%
    dplyr::mutate_at(vars(-id), ~ na_if(., 9999)) %>% 
    tidyr::drop_na(matches("lr\\d+")) %>% 
    select(-country)
  
  # Save for later use.
  saveRDS(relevant_df, file = output_file)
}
