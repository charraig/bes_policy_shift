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


dplyr::mutate(wave = stringr::str_pad(wave, 2, "left", "0")) %>% 
  tidyr::unite("var_wave", item, wave, sep = "_") %>%
  dplyr::arrange(var_wave) %>% 
  tidyr::spread(var_wave, value)