# ----------------------------------------------------------
# Call dependencies
# ----------------------------------------------------------
source('src/ingest_data.R')

# ----------------------------------------------------------
# Rename variables 
# ----------------------------------------------------------
# `rename_key` from config file, ensures wave presence is clear
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

# ----------------------------------------------------------
# Recode variables
# ----------------------------------------------------------
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

# ----------------------------------------------------------
# Assign responses to specific waves
# ----------------------------------------------------------
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
        # head(10) %>%  # just for debugging
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
  ) %>% 
  select(-wave) %>% 
  tidyr::unnest(-id) %>%  # BUG!!!
  tidyr::pivot_longer(-id, names_to = c("item", "wave"), names_sep = ":") %>% 
  tidyr::pivot_wider(id_cols = c("id", "wave"), 
                     names_from = item,
                     values_from = value) %>% 
  dplyr::mutate_at(vars(-id), as.integer) %>% 
  dplyr::mutate(id = as.factor(id))

# type_fun <- function(df) {df %>% pull(`profile_work_stat:1`) %>% typeof()}
# find_bad <- assigned_df %>% select(id, profile_work_stat) %>% mutate(edtype = purrr::map_chr(profile_work_stat, type_fun))

# Add wave "date" as column----------------------------------------
timed_df <- assigned_df %>% 
  dplyr::left_join(wave_recoding, by="wave") %>% 
  rename(days_since_20140101 = "days_since_epoch")
