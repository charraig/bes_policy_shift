source('src/utilities.R')
source('src/config/config.R')

# Rename variables------------------------------------------------------
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

# Recode variables------------------------------------------------------
# First, separate the recoding columns from the rest
# Then take the recoding columns df and make it long
# Then conduct left joins and coalesce to recode
recode_colnames <- recoding_df %>% 
  select(item) %>% 
  distinct() %>% 
  pull()

to_recode_pre <- renamed_df %>% 
  select(id, contains(recode_colnames))

no_recode_df <- renamed_df %>% 
  select(-contains(recode_colnames))

to_recode_post <- to_recode_pre %>% 
  tidyr::pivot_longer(-id,
                      names_to = c("item", "wave_group"), 
                      names_pattern = "^(.+?)((?:W\\d+)*)$") %>% 
  dplyr::left_join(recoding_df, 
                   by = c("item", "value")) %>% 
  dplyr::mutate(item_group = stringr::str_c(item, wave_group)) %>% 
  dplyr::select(-value, value = "recoding", -item, -wave_group) %>% 
  tidyr::pivot_wider(id_cols=c("id"), 
                     names_from = item_group, 
                     values_from = value)

recoded_df <- to_recode_post %>% 
  dplyr::full_join(no_recode_df, by="id")

# Check that we didn't gain or lose any rows
if (nrow(recoded_df) != nrow(renamed_df)) {
  stop("Recoding introduced errors.")
}

# Expand wave groups into individual waves------------------------------
# make a list of lists
# each item in the list is a list of columns that belong to an item group
# e.g. [[wave1, wave2, wave3, ...], [lr1W1W2W3W4W5, lr1W6, ...], ...]
group_items <-
  item_map %>% 
  purrr::map(
    grep,
    x = names(recoded_df),
    value = TRUE
  )

# create a tibble with one row per individual whose columns 
# are tibbles of items by item group
# i.e. the group_items list (above) are the columns
group_data <-
  group_items %>% 
  purrr::map(
    ~ {
      recoded_df %>%
        head(10) %>%  # just for debugging
        dplyr::select(id, all_of(.x)) %>%
        dplyr::group_by(id) %>% 
        tidyr::nest() %>% 
        dplyr::ungroup()
    }
  ) %>% 
  purrr::reduce(dplyr::inner_join, by = "id") %>% 
  setNames(c("id", names(group_items)))

# Assign responses to specific waves, instead of just wave groups
# Fill in NA for missing values
assigned_df <- group_data %>%
  dplyr::mutate(
    wave = purrr::map(wave, long_waves)
    ) %>% 
  dplyr::mutate_at(
    vars(-id, -wave),
    ~ purrr::map2(., wave, assign_value_to_wave)
  )

# Assign missing values policies-----------------------------------
filled_df <- assigned_df %>% 
  # Last value carried over...
  dplyr::mutate_at(
    vars(education, gender),
    ~ purrr::map(., ~ tidyr::fill(.x, value, .direction = "downup"))
  )

# NEXT STEPS
# * Think about recoding the waves to actual timestamps earlier, this could enable filling in missing ages
# * Eventually, you'll want to figure out how to write down all the columns once and have it "just work"
# * Compute lag columns (e.g. income change) and coalesce columns of recoded variables
# * Spot check recodings. make sure 9999's got converted to NAs, etc.



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
  dplyr::mutate(working_status = case_when(
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
