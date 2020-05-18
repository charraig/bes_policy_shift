# Basic Config
max_wave = 16

# Ingestion Config------------------------------------------------------
# item group name = item regex
item_map <- list(
  # Identification & Accounting columns
  wave = "wave\\d+",  # DO NOT REMOVE!!
  
  # Outcome Variable columns
  lr = "lr\\d+",

  # Predictor Variable columns
  age = "^ageW",  # age
  education = "^education",  # education
  gender = "^gender",  # gender
  profile_house_tenure = "profile_house_tenure",  # housing
  housing = "housing",  # housing
  profile_gross_income = "profile_gross_household", # income & objhard-income
  workingStatus = "workingStatus",  # objhard-job
  profile_work_stat = "profile_work_stat",  # objhard-job
  profile_work_type = "profile_work_type",  # social class
  profile_socgrade = "profile_socgrade",  # social class
  riskPoverty = "riskPoverty",  # subjhard-finsec
  riskUnemployment = "riskUnemployment"  # subjhard-jobsec
)

# Renaming Config------------------------------------------------------
rename_key = c(
  educationW1W2W3W4W5W6 = "educationW1_W6",
  genderW1W2W3W4W5W6W7W8W9W10W11W12W13W14W15W16 = "gender",
  housingW1W2W3W4W5W6W7W8W9W10W11W12W13W14W15W16 = "housing",
  profile_house_tenureW1W2W3W4W5W6W7W8W9 = "profile_house_tenure",
  profile_gross_householdW1W2W3W4W5W6W7W8W9 = "profile_gross_household",
  workingStatusW6W7W8W9W10W11W12 = "workingStatusW6_W14",
  profile_work_statW1W2W3W4W5W6W7W8W9W10 = "profile_work_statW1_W10",
  profile_work_typeW2W3W4 = "profile_work_typeW2_W4"
)

# Recoding Config------------------------------------------------------
education_recoding <- readr::read_csv(
  "src/config/education_source_recoding.csv"
)
housing_recoding <- readr::read_csv(
  "src/config/housing_source_recoding.csv"
)
obj_hard_job_recoding <- readr::read_csv(
  "src/config/objective-hardship-job_source_recoding.csv"
)
social_class_recoding <- readr::read_csv(
  "src/config/social-class_source_recoding.csv"
)
recoding_df <- education_recoding %>% 
  dplyr::union_all(housing_recoding) %>% 
  dplyr::union_all(obj_hard_job_recoding) %>% 
  dplyr::union_all(social_class_recoding) %>% 
  select(-description, -recoding_description)
