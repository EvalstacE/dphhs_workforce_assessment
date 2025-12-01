

library(dplyr)
library(tidyr)
library(stringr)

summarize_select_all <- function(df,
                                 group,
                                 prefix = "pref_",
                                 na_tokens = c("na","n/a","none","no","not applicable")) {
  # helper to clean and pivot once
  do_pivot <- function(data) {
    data %>%
      dplyr::select(dplyr::everything()) %>%
      tidyr::pivot_longer(
        cols      = dplyr::starts_with(prefix),
        names_to  = "column",
        values_to = "raw"
      ) %>%
      dplyr::filter(!is.na(raw), stringr::str_trim(raw) != "") %>%
      tidyr::separate_rows(raw, sep = ",") %>%
      dplyr::mutate(
        response = stringr::str_squish(raw),
        key      = stringr::str_to_lower(response)
      ) %>%
      dplyr::filter(
        key != "",
        !key %in% na_tokens
      )
  }
  
  # --- no grouping: keep your original behavior -----------------------------
  if (missing(group)) {
    df_long <- do_pivot(df)
    
    out <- df_long %>%
      dplyr::group_by(column, key) %>%
      dplyr::summarise(
        n        = dplyr::n(),
        response = dplyr::first(response),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(prop = n / nrow(df)) %>%
      dplyr::arrange(column, dplyr::desc(n)) %>%
      dplyr::select(column, response, n, prop)
    
    return(out)
  }
  
  # --- grouped version: proportions within each group -----------------------
  g <- rlang::enquo(group)
  
  # denominator per group = number of rows in df for that group
  group_sizes <- df %>%
    dplyr::count(!!g, name = "group_n")
  
  df_long <- df %>%
    dplyr::select(!!g, dplyr::starts_with(prefix)) %>%
    do_pivot()
  
  out <- df_long %>%
    dplyr::group_by(!!g, column, key) %>%
    dplyr::summarise(
      n        = dplyr::n(),
      response = dplyr::first(response),
      .groups  = "drop"
    ) %>%
    dplyr::left_join(group_sizes, by = rlang::as_name(g)) %>%
    dplyr::mutate(prop = n / group_n) %>%
    dplyr::arrange(!!g, column, dplyr::desc(n)) %>%
    dplyr::select(!!g, column, response, n, prop)
  
  out
}





summarize_select_all_unique <- function(df,
                                        group,
                                        respondent = respondent_id,
                                        prefix = "pref_",
                                        na_tokens = c("na","n/a","none","no","not applicable")) {
  
  r_id <- rlang::enquo(respondent)
  
  do_pivot <- function(data) {
    data %>%
      tidyr::pivot_longer(
        cols      = dplyr::starts_with(prefix),
        names_to  = "column",
        values_to = "raw"
      ) %>%
      dplyr::filter(!is.na(raw), stringr::str_trim(raw) != "") %>%
      tidyr::separate_rows(raw, sep = ",") %>%
      dplyr::mutate(
        response = stringr::str_squish(raw),
        key      = stringr::str_to_lower(response)
      ) %>%
      dplyr::filter(
        key != "",
        !key %in% na_tokens
      ) %>%
      # key step: each respondent contributes only once per option
      dplyr::distinct(!!r_id, column, key, .keep_all = TRUE)
  }
  
  # ----------------------------------------------------------
  # NO GROUPING VERSION
  # ----------------------------------------------------------
  if (missing(group)) {
    df_long <- df %>%
      dplyr::select(!!r_id, dplyr::starts_with(prefix)) %>%
      do_pivot()
    
    out <- df_long %>%
      dplyr::group_by(column, key) %>%
      dplyr::summarise(
        n        = dplyr::n(),
        response = dplyr::first(response),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        prop = n / dplyr::n_distinct(df %>% dplyr::pull(!!r_id))
      ) %>%
      dplyr::arrange(column, dplyr::desc(n)) %>%
      dplyr::select(column, response, n, prop)
    
    return(out)
  }
  
  # ----------------------------------------------------------
  # GROUPED VERSION
  # ----------------------------------------------------------
  g <- rlang::enquo(group)
  
  # denominator per group = unique respondents per group
  group_sizes <- df %>%
    dplyr::distinct(!!g, !!r_id) %>%
    dplyr::count(!!g, name = "group_n")
  
  df_long <- df %>%
    dplyr::select(!!g, !!r_id, dplyr::starts_with(prefix)) %>%
    do_pivot()
  
  out <- df_long %>%
    dplyr::group_by(!!g, column, key) %>%
    dplyr::summarise(
      n        = dplyr::n(),
      response = dplyr::first(response),
      .groups  = "drop"
    ) %>%
    dplyr::left_join(group_sizes, by = rlang::as_name(g)) %>%
    dplyr::mutate(prop = n / group_n) %>%
    dplyr::arrange(!!g, column, dplyr::desc(n)) %>%
    dplyr::select(!!g, column, response, n, prop)
  
  out
}
