summarise_group_props <- function(data, group, category) {
  
  g   <- rlang::enquo(group)
  cat <- rlang::enquo(category)
  
  data %>%
    filter(!is.na(!!cat)) %>%
    count(!!g, !!cat) %>%
    group_by(!!g) %>%
    mutate(
      prop      = n / sum(n),
      se        = sqrt(prop * (1 - prop) / sum(n)),
      ci_lower  = prop - 1.96 * se,
      ci_upper  = prop + 1.96 * se,
      ci_lower  = pmax(ci_lower, 0),
      ci_upper  = pmin(ci_upper, 1)
    ) %>%
    ungroup() %>%
    mutate(
      across(
        c(prop, ci_lower, ci_upper),
        ~ .x * 100
      )
    )
}

