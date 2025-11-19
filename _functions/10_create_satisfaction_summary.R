#######################################################################
#######################################################################
#######################################################################

summarise_satisfy_props <- function(data, group,
                                    prefix = "s_",
                                    suffix = "_recat",
                                    levels = c("Agree", "Strongly agree")) {
  
  pat <- paste0("^", prefix, ".*", suffix, "$")
  
  df <- data %>%
    tidyr::pivot_longer(
      cols      = dplyr::matches(pat),
      names_to  = "question",
      values_to = "resp"
    ) %>%
    dplyr::filter(!is.na(resp)) %>%
    dplyr::mutate(agree = resp %in% levels)
  
  # --- grouped vs ungrouped ---
  if (missing(group)) {
    grouped <- df %>%
      dplyr::group_by(question)
  } else {
    grouped <- df %>%
      dplyr::group_by({{ group }}, question)
  }
  
  grouped %>%
    dplyr::summarise(
      prop_agree = mean(agree),
      n          = dplyr::n(),
      sd_agree   = stats::sd(as.numeric(agree)),
      se         = sd_agree / sqrt(n),
      ci_lower   = prop_agree - 1.96 * se,
      ci_upper   = prop_agree + 1.96 * se,
      .groups    = "drop"
    ) %>%
    dplyr::mutate(
      dplyr::across(c(prop_agree, se, ci_lower, ci_upper), ~ .x * 100),
      n_agree = round(prop_agree * (n / 100))
    ) %>%
    dplyr::relocate(n_agree, .after = "n") %>%
    ungroup() %>%
    mutate(
      ci_upper = if_else(ci_upper > 100, 100, ci_upper)
    )
}







summ_agree_overall <- function(data, group,
                          prefix = "s_",
                          suffix = "_recat",
                          levels = c("Agree", "Strongly agree")) {
  g <- enquo(group)
  g_name <- quo_name(g)
  pat <- paste0("^", prefix, ".*", suffix, "$")
  
  data %>%
    mutate(id = 1:nrow(data)) %>%
    pivot_longer(
      cols = matches(pat),
      names_to = "question",
      values_to = "resp"
    ) %>%
    filter(!is.na(resp)) %>%
    mutate(agree_binary = resp %in% levels) %>%
    group_by(id, !!g) %>%  # group by person and agency
    summarise(person_agree_rate = mean(agree_binary), .groups = "drop") %>%
    group_by(!!g) %>%
    summarise(
      n_staff = n(),
      prop_agree = mean(person_agree_rate),
      sd_agree = sd(person_agree_rate),
      se = sd_agree / sqrt(n_staff),
      ci_lower = prop_agree - 1.96 * se,
      ci_upper = prop_agree + 1.96 * se,
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(
      ci_upper = if_else(ci_upper > 100, 100, ci_upper)
    ) 
}






#######################################################################
#######################################################################
#######################################################################
#######################################################################

summarise_freq_props <- function(data, group,
                                 prefix = "f_", suffix = "_recat",
                                 lab_rare      = "Rarely/Never",
                                 lab_sometimes = "Sometimes",
                                 lab_usual     = "Usually/Always") {
  
  pat <- paste0("^", prefix, ".*", suffix, "$")
  
  df <- data %>%
    tidyr::pivot_longer(
      cols      = dplyr::matches(pat),
      names_to  = "question",
      values_to = "resp"
    ) %>%
    dplyr::filter(!is.na(resp)) %>%
    dplyr::mutate(
      rare_never   = resp == lab_rare,
      sometimes    = resp == lab_sometimes,
      usually_alwy = resp == lab_usual
    )
  
  if (missing(group)) {
    grouped <- df %>%
      dplyr::group_by(question)
  } else {
    grouped <- df %>%
      dplyr::group_by({{ group }}, question)
  }
  
  grouped %>%
    dplyr::summarise(
      prop_agree = mean(usually_alwy),
      n          = dplyr::n(),
      sd_agree   = stats::sd(as.numeric(usually_alwy)),
      se         = sd_agree / sqrt(n),
      ci_lower   = prop_agree - 1.96 * se,
      ci_upper   = prop_agree + 1.96 * se,
      .groups    = "drop"
    ) %>%
    dplyr::mutate(
      dplyr::across(c(prop_agree, se, ci_lower, ci_upper), ~ .x * 100),
      n_agree = round(prop_agree * (n / 100))
    ) %>%
    dplyr::relocate(n_agree, .after = "n")%>%
    ungroup() %>%
    mutate(
      ci_upper = if_else(ci_upper > 100, 100, ci_upper)
    ) 
}
