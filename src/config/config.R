#------------------------------------------------------
# Basic Config
#------------------------------------------------------
max_wave = 16

#------------------------------------------------------
# Ingestion Config
#------------------------------------------------------
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

#------------------------------------------------------
# Renaming Config
#------------------------------------------------------
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

#------------------------------------------------------
# Recoding Config
#------------------------------------------------------
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

wave_recoding <- read_csv("src/config/wave_recoding.csv",
                          col_types = cols(days_since_epoch = col_integer(), 
                                           end_date = col_date(format = "%Y-%m-%d"), 
                                           epoch = col_date(format = "%Y-%m-%d"), 
                                           mid_date = col_date(format = "%Y-%m-%d"), 
                                           start_date = col_date(format = "%Y-%m-%d"), 
                                           wave = col_integer())) %>% 
  select(wave, days_since_epoch)

#------------------------------------------------------
# Computed Categoricals Config
#------------------------------------------------------
# Work Status Change
# No change (2, 3 grouped into "underemployed", 
#           6, 7, 8 grouped into "retired or other") --> 0
# Employed, Underemployed, Unemployed (1, 2, 3, 4) --> 
#               Student, Retired or Other (5, 6, 7, 8): 1 
# Employed (1) --> Underemployed (2, 3): 2
# Employed, Student, or Retired or Other (1, 5, 6, 7, 8) --> Unemployed (4): 3
# Underemployed (2, 3) --> Employed (1): 4
# Underemployed (2, 3) --> Unemployed (4): 5
# Unemployed (4) --> Underemployed (2, 3): 6
# Unemployed (4) --> Employed (1): 7
# Student (5) --> Employed or Underemployed (1, 2, 3): 8
# Student (5) --> Retired or Other (6, 7, 8): 9
# Retired or Other (6, 7, 8) --> Employed or Underemployed (1, 2, 3): 10
# Retired or Other (6, 7, 8) --> Unemployed (4): 11
# Retired or Other (6, 7, 8) --> Student (5): 0
n_status <- 8
work_status_comb <- tibble::tibble(
  t0 = rep(1:n_status, times=n_status),
  t1 = rep(1:n_status, each=n_status),
) %>% 
  dplyr::mutate(work_status_change = 
                  dplyr::case_when(
                    t0 == t1 ~ 0,
                    t0 %in% c(2, 3) & t1 %in% c(2, 3) ~ 0,
                    t0 %in% c(6, 7, 8) & t1 %in% c(6, 7, 8) ~ 0,
                    t0 %in% c(1, 2, 3, 4) & t1 %in% c(5, 6, 7, 8) ~ 1,
                    t0 == 1 & t1 %in% c(2, 3) ~ 2,
                    t0 %in% c(1, 5, 6, 7, 8) & t1 == 4 ~ 3,
                    t0 %in% c(2, 3) & t1 == 1 ~ 4,
                    t0 %in% c(2, 3) & t1 == 4 ~ 5,
                    t0 == 4 & t1 %in% c(2, 3) ~ 6,
                    t0 == 4 & t1 == 1 ~ 7,
                    t0 == 5 & t1 %in% c(1, 2, 3) ~ 8,
                    t0 == 5 & t1 %in% c(6, 7, 8) ~ 9,
                    t0 %in% c(6, 7, 8) & t1 %in% c(1, 2, 3) ~ 10,
                    t0 %in% c(6, 7, 8) & t1 == 5 ~ 0
                  ))


