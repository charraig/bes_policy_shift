long_waves <- function(df) {
  df %>% 
    tidyr::pivot_longer(everything()) %>% 
    dplyr::mutate(wave = as.integer(str_sub(name, 5))) %>% 
    dplyr::filter(value == 1) %>% 
    select("wave")
}


#' Expand list of column names for an item group into a mapping dataframe
#' with a row for each wave, mapping back to the original column name
#' 
#' @param colnames list of column names from an item group
#' 
#' @return tibble mapping variable name to wave integers
#' @examples
#' \dontrun{
#' colnames <- list(workingStatusW1W2W3, workingStatusW4W5, workingStatusW6)
#' spread_varnames(colnames)
#' }
spread_varnames <- function(colnames) {
  colnames %>% 
    tibble::enframe() %>% 
    dplyr::mutate(idx = regexpr("W", value)) %>%  # location of first "W"
    dplyr::rename(variable = value) %>%
    dplyr::mutate(
      item = substr(variable, 1, idx - 1),  # get the variable name only
      waves = substr(variable, idx + 1, nchar(variable)),  # get the wave list
      waves_split = strsplit(waves, "W") # col w/ list of wave numbers
    ) %>%
    tidyr::unnest(cols = c(waves_split)) %>% 
    dplyr::mutate(wave = as.integer(waves_split)) %>% 
    dplyr::select(variable, item, wave)
}


#' apply_wavenum
#'
#' @param waves: Column of waves a respondent was present for.
#' Should have column: wave. 
#' @param var_waves: Long form of variable names associated to waves.
#' Should have columns: variable, item, wave
#' Output from spread_varnames
#'
#' @return
#' @export
#'
#' @examples
apply_wavenum <- function(waves, var_waves) {
  waves %>% 
    dplyr::inner_join(var_waves, by = "wave") %>% 
    dplyr::group_by(variable) %>%  # wave groups
    dplyr::arrange(wave) %>% 
    dplyr::mutate(rn = row_number()) %>%  
    dplyr::ungroup() %>% 
    dplyr::filter(rn == 1) %>%  # select first wave in which a person appears
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
    tidyr::pivot_longer(everything(), 
                        names_to = "variable", 
                        values_to = "value") %>% 
    dplyr::inner_join(waves, by = "variable") %>% 
    dplyr::select(-variable) %>%
    tidyr::complete(wave=1:max_wave, item) %>% 
    tidyr::pivot_wider(names_from = c(item, wave), 
                       names_sep = ":", 
                       values_from = value)
}


lin_interp_floor <- function(t, s) {
  l <- lm(s ~ t, na.action = na.omit)
  mb <- l$coefficients
  s2 <- t*mb[2] + mb[1]
  floor(s2)
}
