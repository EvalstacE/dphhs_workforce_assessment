

two_group_count_prop <- function(data, group1, group2,
                                 normalize = TRUE, drop_na = TRUE) {
  g1 <- enquo(group1)
  g2 <- enquo(group2)
  
  g1_name <- quo_name(g1)
  g2_name <- quo_name(g2)
  
  # dynamic output column names
  count_col        <- paste0(g1_name, "_", g2_name, "_count")
  g1_count_col     <- paste0(g1_name, "_count")
  prop_col         <- paste0("prop_", g2_name, "_within_", g1_name)
  
  df <- data
  
  # optional normalization to lower/trim
  if (normalize) {
    df <- df %>%
      mutate(
        !!g1_name := str_squish(str_to_lower(as.character(!!g1))),
        !!g2_name := str_squish(str_to_lower(as.character(!!g2)))
      )
  }
  
  if (drop_na) {
    df <- df %>% filter(!is.na(!!g1), !is.na(!!g2))
  }
  
  df %>%
    count(!!g1, !!g2, name = count_col) %>%
    group_by(!!g1) %>%
    mutate(
      !!g1_count_col := sum(.data[[count_col]]),
      !!prop_col     := 100 * .data[[count_col]] / .data[[g1_count_col]]
    ) %>%
    ungroup()
}
