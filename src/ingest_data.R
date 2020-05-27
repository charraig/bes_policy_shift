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
# Call dependencies
# ----------------------------------------------------------
print('Reading in Config...')
source('src/utilities.R')
source('src/config/config.R')

# ----------------------------------------------------------
# Ingest data from Stata file (.dta)
# ----------------------------------------------------------
# Ingest from raw data only if the specified cache file (.rds) is not present
# Otherwise, ingest from the cache file
# If ingesting raw, down-select the necessary columns, do basic filtering,
#   then save result to the cache.

output_file = 'data/survey_data.rds'
print('Ingesting data...')

if (file.exists(output_file)) {
  
  print('Data found in cache. Reading in...')
  relevant_df <- readRDS(output_file)
  
} else {
  
  print('Data not found in cache. Reading in raw data...')
  
  # ingest data
  raw_df <- haven::read_dta(
    "data/BES2019_W16_Panel_v0.3.dta"
  )
  
  # Down-select to variables of interest
  selected_df <- raw_df %>% 
    select(
      # Metadata
      id,
      wt_new_W1_W16,
      matches("^country$"),
      matches(select_regex, ignore.case = FALSE)
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
    # Pre-filter
    dplyr::filter(country == 1) %>%
    tidyr::drop_na(matches("lr\\d+")) %>%
    select(-country) %>% 
    # Basic operations
    dplyr::mutate_at(vars(-id), ~ na_if(., 9999))
  
  # Save for later use.
  print('Saving data to cache...')
  saveRDS(relevant_df, file = output_file)
}
