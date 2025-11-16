train_prefs_df <- read.csv(file = here("_data/training_desires.csv"))
train_prefs_sum <- summarize_select_all(train_prefs_df)


format_prefs <- train_prefs_sum %>%
  filter(column == "pref_formats") %>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))

write.csv(format_prefs, "_www/df_exports/format_prefs.csv", row.names = FALSE)
