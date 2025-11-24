summarise_props <- function(
    data,
    group = NULL,
    cols,
    success_levels = NULL,
    make_binary = NULL
) {
  
  df <- data %>%
    tidyr::pivot_longer(
      cols      = {{ cols }},
      names_to  = "question",
      values_to = "resp"
    ) %>%
    dplyr::filter(!is.na(resp))
  
  # --- create binary success indicator ---
  if (!is.null(success_levels)) {
    df <- df %>% mutate(success = resp %in% success_levels)
  } else if (!is.null(make_binary)) {
    df <- df %>% mutate(success = make_binary(resp))
  } else {
    stop("You must provide either `success_levels` or `make_binary`.")
  }
  
  # --- grouped vs ungrouped ---
  if (is.null(group)) {
    grouped <- df %>% group_by(question)
  } else {
    grouped <- df %>% group_by({{ group }}, question)
  }
  
  grouped %>%
    summarise(
      prop = mean(success),
      n    = n(),
      sd   = stats::sd(as.numeric(success)),
      se   = sd / sqrt(n),
      ci_lower = prop - 1.96 * se,
      ci_upper = prop + 1.96 * se,
      .groups = "drop"
    ) %>%
    mutate(
      across(c(prop, se, ci_lower, ci_upper), ~ .x * 100),
      n_success = round(prop * (n / 100)),
      ci_upper = pmin(ci_upper, 100),
      ci_lower = pmax(ci_lower, 0)
    ) %>%
    relocate(n_success, .after = n)
}
