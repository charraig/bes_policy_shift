# Coelsce similar columns-----------------------------------
slimmed_df <- timed_df %>% 
  mutate(
    housing2 = dplyr::case_when(
      !is.na(housing) ~ housing,
      !is.na(profile_house_tenure) ~ profile_house_tenure,
      TRUE ~ NA_integer_
    ),
    working_status = dplyr::case_when(
      !is.na(workingStatus) ~ workingStatus,
      !is.na(profile_work_stat) ~ profile_work_stat,
      TRUE ~ NA_integer_
    ),
    social_class = dplyr::case_when(
      !is.na(profile_socgrade) ~ profile_socgrade,
      !is.na(profile_work_type) ~ profile_work_type,
      TRUE ~ NA_integer_
    )
  )

# Compute additional columns--------------------------------------
expanded_df <- slimmed_df %>% 
  dplyr::arrange(id, wave) %>% 
  dplyr::mutate(profile_gross_household = na_if(profile_gross_household, 17)) %>% 
  dplyr::group_by(id) %>% 
  tidyr::fill(profile_gross_household, .direction="downup") %>% 
  dplyr::mutate(income_lag = lag(profile_gross_household),
                income_lag_diff = profile_gross_household - income_lag)


# Assign missing values policies-----------------------------------
filled_df <- timed_df %>% 
  dplyr::arrange(id, wave) %>% 
  # Last value carried over...
  dplyr::group_by(id) %>% 
  tidyr::fill(education, gender, .direction="downup") %>% 
  dplyr::mutate(age = lin_interp_floor(days_since_epoch, age))




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