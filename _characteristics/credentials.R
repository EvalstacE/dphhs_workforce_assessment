
creds_df <- read.csv(here("_data/credentials_df.csv")) 


degree_sum_sup1 <- two_group_count_prop(creds_df, sup_status, degree_recat)%>%
  rename(
    "degree_cat_count" = "sup_status_degree_recat_count",
    "prop_degree_recat"   = "prop_degree_recat_within_sup_status"
  )

degree_sum_sup2 <- two_group_count_prop(creds_df, sup_status, highest_degree)


masters_sum <- two_group_count_prop(creds_df, sup_status, highest_degree) %>%
  filter(highest_degree == "masters or higher") %>%
  rename(
    "masters_count" = "sup_status_highest_degree_count",
    "prop_masters_degree" = "prop_highest_degree_within_sup_status"
    ) %>%
  select(-sup_status_count)

degree_sum_final <- degree_sum_sup1 %>% 
  left_join(masters_sum, by = "sup_status", relationship = "many-to-many") %>%
  mutate(
    ms_prop = case_when(
        degree_recat == "bachelors or higher" ~ (prop_masters_degree * prop_masters_degree / 100),
        TRUE ~ NA_real_
      )
    )

degree_sum_all <- two_group_count_prop(creds_df, degree_recat, highest_degree)


creds_df_sum <- summarize_select_all(df = creds_df, prefix = "degrees")%>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))