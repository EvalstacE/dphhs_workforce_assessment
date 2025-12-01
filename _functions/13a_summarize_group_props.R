summarise_group_props <- function(data, group, category, 
                                  rename_cols = FALSE) {
  
  g      <- rlang::enquo(group)
  cat    <- rlang::enquo(category)
  g_name <- rlang::as_name(g) 
  
  out <- data %>%
    filter(!is.na(!!cat)) %>%
    count(!!g, !!cat) %>%
    group_by(!!g) %>%
    mutate(
      prop      = n / sum(n),
      se        = sqrt(prop * (1 - prop) / sum(n)),
      ci_lower  = pmax(prop - 1.96 * se, 0),
      ci_upper  = pmin(prop + 1.96 * se, 1)
    ) %>%
    ungroup() %>%
    mutate(
      across(c(prop, ci_lower, ci_upper), ~ .x * 100)
    )
  
  if (rename_cols) {
    out <- out %>%
      rename(
        group_cat    = !!g,
        category = !!cat
      ) %>%
      mutate(
        group = g_name
      ) %>%
      relocate(group)
  }
  
  out
}


