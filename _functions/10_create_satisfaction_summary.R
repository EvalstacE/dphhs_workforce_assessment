#######################################################################
#######################################################################
#######################################################################

summarise_satisfy_props <- function(data, group,
                                    prefix = "s_",
                                    suffix = "_recat",
                                    level  = "Agree") {
  g <- enquo(group)
  g_name <- quo_name(g)
  pat <- paste0("^", prefix, ".*", suffix, "$")
  
  # safe col name: lowercased, underscores for spaces
  level_clean <- str_replace_all(str_to_lower(level), "\\s+", "_")
  prop_col    <- paste0("prop_", level_clean)
  
  # total per group (denominator)
  group_totals <- data %>%
    group_by(!!g) %>%
    summarise(group_total = n(), .groups = "drop")
  
  data %>%
    pivot_longer(
      cols = matches(pat),
      names_to = "category",
      values_to = "resp"
    ) %>%
    filter(!is.na(resp)) %>%
    group_by(!!g, category) %>%
    summarise(count_level = sum(resp == level), .groups = "drop") %>%
    left_join(group_totals, by = g_name) %>%
    mutate(!!prop_col := 100 * count_level / group_total)
}


#######################################################################
#######################################################################
#######################################################################
#######################################################################

summarise_freq_props <- function(data, group,
                                      prefix = "f_", suffix = "_recat",
                                      lab_rare  = "Rarely/Never",
                                      lab_sometimes = "Sometimes",
                                      lab_usual = "Usually/Always") {
  g <- enquo(group)
  g_name <- quo_name(g)
  pat <- paste0("^", prefix, ".*", suffix, "$")
  
  # fixed denominator per group (includes NAs)
  group_totals <- data %>%
    group_by(!!g) %>%
    summarise(group_total = n(), .groups = "drop")
  
  data %>%
    pivot_longer(
      cols = matches(pat),
      names_to = "category",
      values_to = "resp"
    ) %>%
    filter(!is.na(resp)) %>%                    # only count non-missing, denom stays fixed
    group_by(!!g, category) %>%
    summarise(
      count_rare_never   = sum(resp == lab_rare),
      count_sometimes    = sum(resp == lab_sometimes),
      count_usually_alwy = sum(resp == lab_usual),
      .groups = "drop"
    ) %>%
    left_join(group_totals, by = g_name) %>%
    mutate(
      prop_rare_never      = 100 * count_rare_never   / group_total,
      prop_sometimes       = 100 * count_sometimes    / group_total,
      prop_usually_always  = 100 * count_usually_alwy / group_total
    ) %>%
    select(!!g, category,
           prop_rare_never, prop_sometimes, prop_usually_always)
}


