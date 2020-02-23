# item group name = item regex
item_map <- list(
  wave = "wave\\d+",  # DO NOT REMOVE!!
  
  
  # Outcome Variables
  lr = "lr\\d+",
  leftRight = "leftRight",
  
  
  # Predictor Variables
  age = "^age",  # age
  
  gender = "^gender$",  # gender
  
  selfOccStatus = "selfOccStatusW",  # social status
  profile_work_type = "profile_work_type",  # social status
  profile_socgrade = "profile_socgrade",  # social status
  
  profile_house_tenure = "profile_house_tenure",  # housing
  
  education = "^education",  # education
  
  profile_gross_household = "profile_gross_household",  # objective hardship
  workingStatus = "workingStatus",  # objective hardship
  profile_work_stat = "profile_work_stat",  # objective hardship
  
  goodTimePurchase = "goodTimePurchase",  # subjective hardship
  econPersonalProsp = "econPersonalProsp",  # subjective hardship
  econPersonalRetro = "econPersonalRetro",  # subjective hardship
  riskPoverty = "riskPoverty",  # subjective hardship
  riskUnemploymnet = "riskUnemployment",  # subjective hardship
  changeCostLive = "changeCostLive",  # subjective hardship
  subjClass = "subjClass"  # subjective hardship
)

# Outcome Variables Config------------------------------------------------------


# Predictor Variables Config ---------------------------------------------------

# age
# no recoding / renaming needed

# gender
gender_rename = c(genderW1W2W3W4W5W6W7W8W9W10W11W12W13W14 = "gender")

# social status
selfOccStatus_rename = c(selfOccStatusW6W7W8W9W10W11W12 = "selfOccStatusW6_W12")
profile_work_type_rename = c(profile_work_typeW2W3W4 = "profile_work_typeW2_W4")
profile_work_type_recode = c(`1` = 2,
                             `2` = 2,
                             `3` = 4,
                             `4` = 1,
                             `5` = 5,
                             `6` = 5,
                             `7` = 6,
                             `8` = 1,
                             `9` = -1)
profile_socgrade_recode = c( `1` = 2,
                             `2` = 2,
                             `3` = 4,
                             `4` = 5,
                             `5` = 6,
                             `6` = 1,
                             `7` = -1,
                             `8` = -1)

# housing
profile_house_tenure_rename = c(profile_house_tenureW1W2W3W4W5W6W7W8W9 = "profile_house_tenure")

# education
education_rename = c(educationW1W2W3W4W5W6 = "educationW1_W6")

# objective hardship
profile_gross_household_rename = c(profile_gross_householdW1W2W3W4W5W6W7W8W9 = "profile_gross_household")
workingStatus_rename = c(workingStatusW6W7W8W9W10W11W12 = "workingStatusW6_W12")
workingStatus_recode = c(`1` = 1,
                         `2` = 2,
                         `3` = 3,
                         `4` = 4,
                         `5` = 5,
                         `6` = 5,
                         `7` = 6,
                         `8` = 7,
                         `9` = 8)
profile_work_state_recode = c(`1` = 1,
                              `2` = 2,
                              `3` = 3,
                              `4` = 5,
                              `5` = 6,
                              `6` = 4,
                              `7` = 7,
                              `8` = 8)


# subjective hardship
subjClass_rename = c(subjClassW2W3W4W5W6W7W8W9 = "subjClassW2_W4W7W9")

