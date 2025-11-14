df <- read_csv("_data/vs_training_priorities.csv")


# Summarize only columns starting with "dom_"
rank_summary <- df %>%
  summarise(across(starts_with("SS"), list(
    mean_rank = ~mean(.x, na.rm = TRUE),
    median_rank = ~median(.x, na.rm = TRUE),
    n_rank1 = ~sum(.x %in% c(1,2,3), na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("Domain", ".value"),
               names_sep = "_")%>%
  rename(
    "in_top_3" = "n",
    "mean_rank" = "mean", 
    "median_rank" = "median"
  ) %>%
  arrange(mean_rank)

write.csv(rank_summary, "_data/team_rank_summary.csv", row.names = FALSE)

# Sort by mean rank (lower = higher priority)
rank_summary <- rank_summary %>% arrange(mean_rank)