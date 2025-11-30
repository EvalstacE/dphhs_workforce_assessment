
train_prefs_df <- read.csv(file = here("_data/training_desires.csv"))
train_prefs_sum <- summarize_select_all(train_prefs_df)

other_train <- read.csv(file = here("_data/other_train_attended.csv"))


other_train_sum <- summarize_select_all(df = other_train, prefix = "attend_train_recat")%>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))


train_attended <- train_prefs_sum %>%
  filter(column == "pref_train_recat") %>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))


train_attended_unique <- summarize_select_all_unique(train_prefs_df, prefix = "pref_train_recat")%>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))


other_train_attended <- train_others %>%
  filter(column == "pref_train_other_recat") %>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))



format_prefs <- train_prefs_sum %>%
  filter(column == "pref_formats") %>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))


train_barriers <- train_prefs_sum %>%
  filter(column == "pref_barriers") %>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))


train_locs_all <- summarize_select_all(df = train_prefs_df, group = region, prefix = "pref_location")%>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))



train_locs_rgn <- summarize_select_all(df = train_prefs_df, group = region, prefix = "pref_location") %>%
  group_by(region) %>%
  arrange(region, desc(prop)) %>%
  slice_head(n=1) %>% ungroup() %>%
  rename("pref_lc" = "response") %>%
  select(-column) %>%
  mutate(
    lat = NA_real_, 
    lng = NA_real_
  )


#write.csv(format_prefs, "_data/df_exports/training_prefs/format_prefs.csv", row.names = FALSE)
#write.csv(train_barriers, "_data/df_exports/training_prefs/train_barriers.csv", row.names = FALSE)
#write.csv(train_locs_rgn, "_data/df_exports/training_prefs/train_locs_rgn.csv", row.names = FALSE)
#write.csv(train_locs_all, "_data/df_exports/training_prefs/train_locs_all.csv", row.names = FALSE)
#write.csv(train_attended, "_data/df_exports/training_prefs/train_attended.csv", row.names = FALSE)
#write.csv(other_train_attended, "_data/df_exports/training_prefs/other_train_attended.csv", row.names = FALSE)
#write.csv(train_attended_unique, "_data/df_exports/training_prefs/train_attended_unique.csv", row.names = FALSE)

