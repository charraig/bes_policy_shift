relevant_df_cols <- colnames(relevant_df)

outcome_prefix <- c('lr\\d+',
                    'redistSelf',
                    'redistTST1_1',
                    'selfRedist',
                    'leftRight',
                    'welfarePreference')
outcome_cols_regex <- concat_cols(outcome_prefix)
outcome_cols <- relevant_df_cols[stringr::str_detect(colnames(relevant_df),
                                                     outcome_cols_regex)]

var_waves <- outcome_cols %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(idx = regexpr("W", value)) %>% 
  dplyr::rename(variable = value) %>% 
  dplyr::mutate(
    item = substr(variable, 1, idx - 1),
    waves = substr(variable, idx + 1, length(variable)),
    waves_split = strsplit(waves, "W")
  ) %>% 
  tidyr::unnest() %>% 
  dplyr::mutate(wave = as.integer(waves_split)) %>% 
  dplyr::select(variable, item, wave)

right_waves <- 
  long_id %>% 
  dplyr::filter(
    responded == 1,
    id %in% keep_ids
  ) %>% 
  dplyr::right_join(var_waves, by = "wave") %>% 
  dplyr::group_by(variable, id) %>% 
  dplyr::summarize(
    item = first(item, order_by = wave),
    wave = first(wave, order_by = wave)
  )
View(right_waves)
View(right_waves %>% 
       filter(variable == 'lr3W10W11W12') %>% 
       count(wave))


outcome_df <- relevant_df %>% 
  dplyr::select(matches(concat_cols(c("^id", outcome_prefix)))) %>% 
  tidyr::gather('variable', 'value', -id) %>% 
  dplyr::inner_join(right_waves, by=c("id","variable")) %>% 
  dplyr::select(-variable) %>% 
  tidyr::unite("id_wave", id, wave, sep='_') %>% 
  tidyr::spread(item, value) %>% 
  tidyr::separate(id_wave,into=c("id", "wave"), sep="_") %>% 
  dplyr::mutate_at(vars("id", "wave"), as.integer) %>% 
  tidyr::complete(id, wave) %>% 
  dplyr::arrange(wave, id)
View(outcome_df)


# Spot check to see if things are right
# Use id == 24391 for a respondent who joined in wave 2
View(raw_df %>% 
       dplyr::select(matches(id_cols_regex)) %>% 
       dplyr::filter(wave1==0, 
                     wave2==1,
                     wave3==1,
                     wave4==1,
                     wave5==1,
                     wave6==1))
View(long_id %>% filter(id == 1))

relevant_df %>% 
  dplyr::select(matches(concat_cols(c("^id", outcome_prefix)))) %>% 
  filter(id == 1) %>% 
  View()

outcome_df %>% filter(id == 1)

colstarts <- c('profile_work_stat',
               'profile_gross_personal',
               'profile_house_tenure',
               'profile_gross_household',
               'profile_socialgrade',
               'profile_work_type',
               'profile_marital_stat',
               'profile_education')
colregex <- paste0(colstarts, '.*$', collapse = '|')
test <- raw_df %>% select(matches(colregex))
colnames(test)