# ----------------------------------------------------------
# Call dependencies
# ----------------------------------------------------------
source('src/clean_data.R')

# ----------------------------------------------------------
# Coalesce redundant columns
# ----------------------------------------------------------
# housing: `housing` if available, otherwise `profile_house_tenure`
# working_status: `workingStatus` if available, otherwise `profile_work_stat`
# social_class: profile_socgrade if available, otherwise `profile_work_type`
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
  ) %>% 
  select(-housing, -profile_house_tenure, housing = "housing2",
         -workingStatus, -profile_work_stat, 
         -profile_socgrade, -profile_work_type)

# ----------------------------------------------------------
# Apply missing values policies
# ----------------------------------------------------------
filled_df <- slimmed_df %>%
  dplyr::arrange(id, wave) %>%
  # Stray "prefer not to say" should become NA...
  dplyr::mutate(profile_gross_household = na_if(profile_gross_household, 17)) %>% 
  # Last value carried forward...
  dplyr::group_by(id) %>%
  tidyr::fill(education, gender, 
              profile_gross_household, riskUnemployment, 
              housing, working_status, 
              social_class, .direction="downup") %>%
  # Linear interpolation...
  dplyr::mutate(age = lin_interp_floor(days_since_20140101, age))

# ----------------------------------------------------------
# Add computed columns
# ----------------------------------------------------------
expanded_df <- filled_df %>% 
  dplyr::mutate(
    # `objhard_income` is the number of income buckets (up or down) respondent moved
    income_lag = lag(profile_gross_household),
    objhard_income = profile_gross_household - income_lag,
    # `subjhard_job` is coded by if riskUnemployment values went up or down
    risk_unemp_lag = lag(riskUnemployment),
    risk_unemp_lag_diff = riskUnemployment - risk_unemp_lag,
    subjhard_job = case_when(
      risk_unemp_lag_diff > 0 ~ 1, # got worse
      risk_unemp_lag_diff == 0 ~ 2,  # stayed stame
      risk_unemp_lag_diff < 0 ~ 3,  # got better
      TRUE ~ NA_real_),
    subjhard_job = as.integer(subjhard_job),
    # `objhard_job` is coded using the `work_status_comb` df joining to the
    #    wave and lagged wave value of `working_status` (see join below)
    working_status_lag = lag(working_status)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(work_status_comb, 
                   by = c("working_status" = "t1", 
                          "working_status_lag" = "t0")) %>% 
  # Select final columns, rename as appropriate
  select(id, wave, days_since_20140101, wt_new_W1_W16,
         age, education, gender, housing, 
         income = "profile_gross_household", social_class,
         objhard_income, objhard_job, 
         subjhard_income = "econPersonalRetro", subjhard_job,
         contains("lr"))

# ----------------------------------------------------------
# Save final data to cache
# ----------------------------------------------------------
output_file = 'data/final_df.rds'
saveRDS(expanded_df, file = output_file)
