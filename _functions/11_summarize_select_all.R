

library(dplyr)
library(tidyr)
library(stringr)

summarize_select_all <- function(df, prefix = "pref_",
                                 na_tokens = c("na","n/a","none","no","not applicable")) {
  
  df %>%

    select(starts_with(prefix)) %>%
    pivot_longer(everything(), names_to = "column", values_to = "raw") %>%
    filter(!is.na(raw), str_trim(raw) != "") %>%

    separate_rows(raw, sep = ",") %>%

    mutate(
      response = str_squish(raw),
      key      = str_to_lower(response)
    ) %>%

    filter(key != "", !key %in% na_tokens) %>%

    group_by(column, key) %>%
    summarise(
      n = n(),
      response = first(response),  # nice display label
      .groups = "drop"
    ) %>%
    group_by(column) %>%
    mutate(prop = n / nrow(df)) %>%
    ungroup() %>%
    arrange(column, desc(n)) %>%
    select(column, response, n, prop)
}



