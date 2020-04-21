source('src/utilities.R')
source('src/config.R')

# rename variables specified in config
renamed_df <- relevant_df %>% 
  dplyr::rename(!!rename_key)

# Check to make sure everything has been renamed, if needed
check <- renamed_df %>% 
  select(!matches(".*\\d+$"), matches("W\\d+_W")) %>% 
  colnames()
if (length(check) > 2) {
  print(check[seq(3,length(check))])
  stop("These variables need to be renamed.")
}

# make a list of lists, each top-level entry is the item group colnames
group_items <-
  item_map %>% 
  purrr::map(
    grep,
    x = names(relevant_df),
    value = TRUE
  )

# create a tibble with one row per individual whose columns 
# are tibbles of items by item group
group_data <-
  group_items %>% 
  purrr::map(
    ~ {
      relevant_df %>%
        # just for debugging
        head(10) %>% 
        dplyr::select(id, all_of(.x)) %>% # stop here for "hack" pattern!
        dplyr::group_by(id) %>% 
        tidyr::nest()
    }
  ) %>% 
  purrr::reduce(dplyr::inner_join, by = "id") %>% 
  setNames(c("id", names(group_items)))


# Create a dataframe with the same structure as group_data, but each
# tibble has been cleaned
clean_groups <- group_data %>%
  # Apply renaming and recoding operations
  dplyr::mutate(
    wave = purrr::map(wave, long_waves),
    gender = purrr::map2(gender, 
                         wave, 
                         rename_recode_assign,
                         rename_key = gender_rename),
    selfOccStatus = purrr::map2(selfOccStatus, 
                                wave, 
                                rename_recode_assign,
                               rename_key = selfOccStatus_rename),
    profile_work_type = purrr::map2(profile_work_type, 
                                    wave, 
                                    rename_recode_assign,
                                    rename_key = profile_work_type_rename,
                                    recode_key = profile_work_type_recode
    )
  ) %>%
  dplyr::mutate_at(
    vars(lr, leftRight),
    ~ purrr::map2(., wave, assign_value_to_wave)
  ) %>% 
  # Apply missing value operations

clean_groups %>% 
  select(-wave) %>% 
  tidyr::unnest()

#gather >> separate >> expand >> spread

  dplyr::group_by(id, item_group) %>% 
  tidyr::nest() %>% 
  slice(1) %>% pull(data)

# one pattern: for each group, extract the tibble and do the transformations
data_final <- list()

data_final[["leftRight"]] <-
  group_data[["leftRight"]] %>% 
  # apply whatever function you like

# coerce a list to a tibble
# %>% 
#   tibble::enframe(
#     name = "item_group",
#     value = "data"
#   ) 

# Restructure outcome columns ---------------------------------------------
string_match = c('lr\\d+', 'welfarePreference', 'leftRight', 'redistSelf')
outcome_df <- 
  relevant_df %>% 
  restructure(wave_id_long, string_match) %>%
 dplyr::mutate_at(vars(-id, -wave), funs(replace(., .==9999, NA))) %>% 
  dplyr::mutate_if(is.labelled, as.integer)

# Restructure id info features --------------------------------------------
# First we need to rename some malformed column names
id_info_prefix <- c(
  # age--
  '^age',
  # gender--
  '^gender',
  # social class--
  'selfOccStatus',
  'profile_work_type',
  'profile_socgrade',
  # housing--
  'profile_house_tenure',
  # education--
  '^education',
  # logged income, objective hardship--
  'profile_gross_household',
  # objective hardship--
  'workingStatus',
  'profile_work_stat',
  # subjective hardship--
  'goodTimePurchase',
  'econPersonalProsp',
  'econPersonalRetro',
  'riskPoverty',
  'riskUnemployment',
  'changeCostLive',
  'subjClass'
  )

info_df <- relevant_df %>% 
  dplyr::rename(
    genderW1W2W3W4W5W6W7W8W9W10W11W12W13W14 = gender,
    educationW1W2W3W4W5W6 = educationW1_W6,
    selfOccStatusW6W7W8W9W10W11W12 = selfOccStatusW6_W12,
    profile_work_typeW2W3W4 = profile_work_typeW2_W4,
    workingStatusW6W7W8W9W10W11W12 = workingStatusW6_W12,
    profile_gross_householdW1W2W3W4W5W6W7W8W9 = profile_gross_household,
    profile_house_tenureW1W2W3W4W5W6W7W8W9 = profile_house_tenure,
    subjClassW2W3W4W5W6W7W8W9 = subjClassW2_W4W7W9) %>% 
  restructure(wave_id_long, id_info_prefix)
View(info_df %>% filter(id == 42868))

info_recoded_df <- info_df %>% 
  dplyr::filter(wave <= 12) %>%
  dplyr::arrange(id, wave) %>% 
  dplyr::rename(logged_income = profile_gross_household) %>% 
  dplyr::mutate(pwt = recode(profile_work_type,
                             `1` = 2,
                             `2` = 2,
                             `3` = 4,
                             `4` = 1,
                             `5` = 5,
                             `6` = 5,
                             `7` = 6,
                             `8` = 1,
                             `9` = -1),
                psg = recode(profile_socgrade,
                             `1` = 2,
                             `2` = 2,
                             `3` = 4,
                             `4` = 5,
                             `5` = 6,
                             `6` = 1,
                             `7` = -1,
                             `8` = -1),
                social_class = case_when(
                  selfOccStatus == 0 ~ 3,
                  !is.na(pwt) ~ pwt,
                  !is.na(psg) ~ psg,
                  TRUE ~ NA_real_
                )) %>% 
  dplyr::mutate(pws = recode(profile_work_stat,
                             `1` = 1,
                             `2` = 2,
                             `3` = 3,
                             `4` = 5,
                             `5` = 6,
                             `6` = 4,
                             `7` = 7,
                             `8` = 8),
                ws = recode(workingStatus,
                            `1` = 1,
                            `2` = 2,
                            `3` = 3,
                            `4` = 4,
                            `5` = 5,
                            `6` = 5,
                            `7` = 6,
                            `8` = 7,
                            `9` = 8),
                working_status = case_when(
                  !is.na(ws) ~ ws,
                  !is.na(pws) ~ pws,
                  TRUE ~ NA_real_
                )) %>%
  # tidyr::fill(education, 
  #             gender,
  #             housing,
  #             social_class,
  #             working_status, 
  #             logged_income) %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(working_status_lag = lag(working_status, order_by = wave),
                logged_income_lag = lag(logged_income, order_by = wave),
                logged_income_diff = case_when(
                  logged_income_lag %in% c(17, 9999) ~ NA_real_,
                  logged_income %in% c(17,9999) ~ NA_real_,
                  TRUE ~ logged_income - logged_income_lag
                )) %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(work_status_comb, 
                    by = c("working_status_lag" = "t0",
                           "working_status" = "t1"))

  
View(info_recoded_df %>% filter(id == 17))
View(info_recoded_df %>% filter(id == 8))

predictors_df <- info_recoded_df %>% 
  dplyr::select(-ws, -pws, -pwt, -psg, 
                -profile_work_type, -profile_socgrade, -selfOccStatus
                -workingStatus, -profile_work_stat)
  

# dplyr::group_by(id) %>% 
#   dplyr::arrange(wave) %>% 
#   tidyr::fill(-id, -wave, -age)
View(predictors_df %>% filter(id==1))
