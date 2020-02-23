# library import ----------------------------------------------------------
library(dplyr)
library(haven)
library(labelled)
library(stringr)
library(tidyr)

# ingest data -------------------------------------------------------------
raw_df <- haven::read_dta(
  "data/BES2017_W14_Panel_v0.3.dta", 
  col_select = c(
    # Metadata
    matches("wave\\d+"),
    matches("country"),
    # Outcome Variables
    matches("lr\\d+"),
    contains("leftRight"),
    # Predictor Variables
    matches("^age"),  # age
    matches("^gender$"),  # gender
    matches("selfOccStatusW"),  # social status
    matches("profile_work_type"),  # social status
    matches("profile_socgrade"),  # social status
    matches("profile_house_tenure"),  # housing
    matches("^education"),  # education
    matches("profile_gross_household"),  # objective hardship
    matches("workingStatus"),  # objective hardship
    matches("profile_work_stat"),  # objective hardship
    matches("goodTimePurchase"),  # subjective hardship
    matches("econPersonalProsp"),  # subjective hardship
    matches("econPersonalRetro"),  # subjective hardship
    matches("riskPoverty"),  # subjective hardship
    matches("riskUnemployment"),  # subjective hardship
    matches("changeCostLive"),  # subjective hardship
    matches("subjClass")  # subjective hardship
  )
)

# Convert to long form of wave response -----------------------------------

# Convert id & wave indicator data into long form
# so that we can apply filters to select the right respondents
# Respondents must:
# (1) Live in England
# (2) Have responded to the panel of 
#     left-right questions in the wave groups surrounding the election
relevant_df <- 
  raw_df %>% 
  labelled::remove_val_labels() %>% 
  dplyr::filter(country == 1) %>%
  tidyr::drop_na(
    matches('lr\\dW1W2W3W4W5'),
    matches('lr\\dW6'),
    matches('lr\\dW7W8W9'),
    matches('lr\\dW10W11W12'),
    matches('lr\\dW13'),
    matches('lr\\dW14')
  )
