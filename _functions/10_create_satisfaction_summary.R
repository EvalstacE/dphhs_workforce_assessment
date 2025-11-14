#######################################################################
#######################################################################
#######################################################################

summarise_satisfy_props <- function(data, group,
                                    prefix = "s_",
                                    suffix = "_recat",
                                    levels = c("Agree", "Strongly agree")) {
  g <- enquo(group)
  pat <- paste0("^", prefix, ".*", suffix, "$")
  
  data %>%
    pivot_longer(
      cols = matches(pat),
      names_to = "question",
      values_to = "resp"
    ) %>%
    filter(!is.na(resp)) %>%
    mutate(agree = resp %in% levels) %>%
    group_by(!!g, question) %>%
    summarise(
      prop_agree = mean(agree),
      n = n(),
      sd_agree = sd(as.numeric(agree)),       # robust SD
      se = sd_agree / sqrt(n),                # robust SE
      ci_lower = prop_agree - 1.96 * se,
      ci_upper = prop_agree + 1.96 * se,
      .groups = "drop"
    ) %>%
    mutate(across(c(prop_agree, se, ci_lower, ci_upper), ~ .x * 100))
           
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
  g   <- enquo(group)
  pat <- paste0("^", prefix, ".*", suffix, "$")
  
data %>%
    pivot_longer(
      cols      = matches(pat),
      names_to  = "question",
      values_to = "resp"
) %>%

filter(!is.na(resp)) %>%
mutate(
      rare_never   = resp == lab_rare,
      sometimes    = resp == lab_sometimes,
      usually_alwy = resp == lab_usual
) %>%
group_by(!!g, question) %>%
  summarise(
      prop_agree = mean(usually_alwy),
      n = n(),
      sd_agree = sd(as.numeric(usually_alwy)),
      # robust SE
      se = sd_agree / sqrt(n),                
      ci_lower = prop_agree - 1.96 * se,
      ci_upper = prop_agree + 1.96 * se,
      .groups = "drop"
) %>%
  mutate(across(c(prop_agree, se, ci_lower, ci_upper), ~ .x * 100))
}



