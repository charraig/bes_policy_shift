long_waves <- function(df) {
  df %>% 
    tidyr::gather("wave", "responded") %>% 
    dplyr::mutate(wave = as.integer(str_sub(wave, 5))) %>% 
    dplyr::filter(responded == 1) %>% 
    select("wave")
}

concat_cols <- function(x) {
  return (paste0(x, 
                 '(?:W\\d+(?:.?))*$', 
                 collapse = '|'))
}


#' Find columns matching a regex
#' 
#' @param df_cols character vector of columns names
#' @param regex regular expression
#' 
#' @return character vector of column names matching regex
#' @examples
#' \dontrun{
#' find_cols(names(df), "lr\\d")
#' }
find_cols <- function(df_cols, regex) {
  cols_regex <- concat_cols(regex)
  df_cols[stringr::str_detect(df_cols, cols_regex)]
}

spread_varnames <- function(colnames) {
  colnames %>% 
    tibble::enframe() %>% 
    dplyr::mutate(idx = regexpr("W", value)) %>% 
    dplyr::rename(variable = value) %>% 
    dplyr::mutate(
      item = substr(variable, 1, idx - 1),  # get the variable name only
      waves = substr(variable, idx + 1, nchar(variable)),  # get the wave list
      waves_clean = str_remove_all(waves, "[_]"),
      waves_split = strsplit(waves_clean, "W")
    ) %>%
    tidyr::unnest() %>% 
    dplyr::mutate(wave = as.integer(waves_split)) %>% 
    dplyr::select(variable, item, wave)
}

#' apply_wavenum
#'
#' @param id_waves: Long form of combinations of respondent id and wave.
#' Should have columns: id, wave. 
#' @param var_waves: Long form of variable names associated to waves.
#' Should have columns: variable, item, wave
#'
#' @return
#' @export
#'
#' @examples
apply_wavenum <- function(waves, var_waves) {
  waves %>% 
    dplyr::inner_join(var_waves, by = "wave") %>% 
    dplyr::group_by(variable) %>% 
    dplyr::arrange(wave) %>% 
    dplyr::mutate(rn = row_number()) %>% 
    dplyr::filter(rn == 1) %>% 
    dplyr::select(wave, variable, item)
}

#' assign_value_to_wave
#' 
#' An individual can have at most one response per wave group.
#' This function assigns the response to the appropriate specific wave, 
#' expanding the other waves with NA. Note: the wave number MUST be listed in the
#' incoming variable name, otherwise the created column will be all NA even if
#' an individual is listed as responding in the missing wave number. If this
#' doesn't agree with accepted nomenclature, i.e. if var1W1_W3 should include
#' W2 values, then first rename the variable (see `rename_recode_assign`) 
#' to var1W1W2W3.
#' 
#' @param df tibble whose columns contain a single response for a wave group.
#' @param waves tibble indicating which specific waves the individual was included in.
#'
#' @return tibble with one column per wave
#'
#' @examples
#' df <- tibble(
#'   var1W1W2W3 = 1, var1W4W6 = 10, var1W14 = 4, 
#'   var2W1W3 = 2, var2W4W5W6 = 3
#'   )
#' waves <- data.frame(wave=c(1, 6, 14))
#' df %>% assign_value_to_wave(waves)
assign_value_to_wave <- function(df, waves) {
  cols <- names(df)
  varwaves <- spread_varnames(cols)
  waves <- apply_wavenum(waves, varwaves)
  
  df %>% 
    # dplyr::select(id, matches(concat_cols(regex_vec))) %>% 
    tidyr::gather('variable', "value") %>% 
    dplyr::inner_join(waves, by = "variable") %>% 
    dplyr::select(-variable) %>%
    tidyr::complete(wave=1:14, item) %>%
    dplyr::mutate(wave = stringr::str_pad(wave, 2, "left", "0")) %>% 
    tidyr::unite("var_wave", item, wave, sep = "_") %>%
    dplyr::arrange(var_wave) %>% 
    tidyr::spread(var_wave, value)
}


#' rename_recode_assign
#'
#' @param df tibble whose columns contain a single response for a wave group.
#' @param rename_key vector to map new and old column names. See `dplyr::rename`. 
#' Or FALSE, indicating no renaming should occur.
#' @param recode_key vector to map new and old column values. See `dplyr::recode`.
#' Or FALSE, indicating no recoding should occur.
#' @param waves tibble indicating which specific waves the individual was included in.
#'
#' @return tibble with one column per wave, potentially with recoded values
#'
#' @examples
#' df <- tibble(
#'   var1W1W2W3 = 1, var1W4W6 = 10, var1W14 = 4,
#'   var2W1_W3 = 2, var2W4W5W6 = 3
#'   )
#' waves <- data.frame(wave=c(1, 6, 14))
#' rename_key <- c(var2W1W2W3 = "var2W1_W3")
#' recode_key <- c(`1` = 2)  # recode 1s as 2s
#' df %>% rename_recode_assign(rename_key = rename_key, waves)
#' df %>% rename_recode_assign(rename_key = rename_key, recode_key = recode_key, waves)
rename_recode_assign <- function(df, 
                                 waves, 
                                 rename_key = FALSE, 
                                 recode_key=FALSE) {
  func_list <- list(~ dplyr::rename(.x, !!!rename_key), 
                    ~ dplyr::mutate_all(.x, ~ recode(., !!!recode_key)),
                    ~ assign_value_to_wave(.x, waves))
  final_funcs <- func_list[c(!is.logical(rename_key), !is.logical(recode_key), TRUE)]
  final_func <- purrr::compose(!!!final_funcs, .dir = "forward")
  df %>% final_func()
}