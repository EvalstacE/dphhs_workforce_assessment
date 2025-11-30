###########################################
###########################################
###                                     ###
###        Current Credentials          ###
###                                     ###
###########################################
###########################################

cred_all <- read.csv(file = here("_data/credentials_df.csv"))%>%
  enforce_cat_factors(sup_col = sup_status, size_col = size)

##-- pre-processed and exported:
#cred_all <- data_cleaned %>%
  #select(region, size, sup_status, contains(c("credentials", "degrees", "degree"))) %>%
  #mutate(cred_recat = if_else(current_credentials == "Not formally certified", "Not formally certified", "certified")) %>%
  #relocate(cred_recat, .after = current_credentials)

#write.csv(cred_all, here("_data/credentials_df.csv"), row.names = FALSE)


cred_sum <- cred_all %>%
  group_by(cred_recat) %>%
  summarise(
    n_cnt = n(), 
    int_prop = 100*n_cnt/448,
    .groups = "drop"
  )


cred_list <- cred_all %>%
  select(-current_credentials_list) %>%
  filter(current_credentials != "Not formally certified")

cred_list_sum <- summarize_select_all(df = cred_list, prefix = "current_credentials")%>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))









cred_sup <- two_group_count_prop(cred_all, sup_status, cred_recat)
###-- no sig. differences
cred_sup_stats <- summarise_group_props(cred_all, sup_status, cred_recat)%>%
  enforce_cat_factors(sup_col = sup_status)

cred_size_stats <- summarise_group_props(cred_all, size, cred_recat)%>%
  enforce_cat_factors(size_col = size)


make_prop_plot(
  cred_size_stats,
  filter_col   = cred_recat,
  filter_value = "certified",
  x_var        = size
)


make_prop_plot(
  cred_sup_stats,
  filter_col   = cred_recat,
  filter_value = "certified",
  x_var        = sup_status
)




###########################################
###########################################
###                                     ###
###     Interst in Credentials          ###
###                                     ###
###########################################
###########################################

cred_int_sum <- cred_all %>%
  group_by(credentials_interest) %>%
  summarise(
    n_cnt = n(), 
    int_prop = 100*n_cnt/448,
    .groups = "drop"
  )


###-- no sig. differences
#cred_int_cert <- two_group_count_prop(cred_all, cred_recat, credentials_interest)

##--close to sig: executives and frontier less interested on average
cred_int_sup_stats <- summarise_group_props(cred_all, sup_status, credentials_interest)
cred_int_size <- summarise_group_props(cred_all, size, credentials_interest)


make_prop_plot(
  cred_int_size,
  filter_col   = credentials_interest,
  filter_value = "Yes",
  x_var        = size
)




cred_int_list <- cred_all %>%
  select(credentials_interest_list) %>%
  filter(!is.na(credentials_interest_list))

cred_int_sum <- summarize_select_all(df = cred_int_list, prefix = "credentials_interest_list")%>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))





###########################################
###########################################
###                                     ###
###        Degrees / JD Size            ###
###                                     ###
###########################################
###########################################


degree_size_stats <- summarise_group_props(data_cleaned, size, degree_recat) %>%
  enforce_cat_factors(size_col =  size)


ggplot(
  data = degree_size_stats %>% filter(degree_recat == "Bachelors or higher"), 
  aes(x = degree_recat, y = prop, color = size)
  ) +
  
  facet_wrap(.~size, nrow = 1) + 
  geom_point() +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.1
  ) +
  scale_y_continuous(
    name   = "Percent",
    labels = scales::percent_format(scale = 1)
  ) +
  ylim(0,100) + 
  theme_classic()+
  theme(
    axis.text.x = element_blank(),
    legend.position = "none"
  )




###########################################
###########################################


pursuing <- data_cleaned %>%
  filter(str_detect(Degrees, "I am currently pursuing a degree"))


degree_sum_sup1 <- two_group_count_prop(creds_df, sup_status, degree_recat)%>%
  rename(
    "degree_cat_count" = "sup_status_degree_recat_count",
    "prop_degree_recat"   = "prop_degree_recat_within_sup_status"
  )

degree_sum_sup2 <- two_group_count_prop(creds_df, sup_status, highest_degree) %>%
  relocate(prop_highest_degree_within_sup_status, .after = highest_degree)


masters_sum <- two_group_count_prop(creds_df, sup_status, highest_degree) %>%
  filter(highest_degree == "masters or higher") %>%
  rename(
    "masters_count" = "sup_status_highest_degree_count",
    "prop_masters_degree" = "prop_highest_degree_within_sup_status"
    ) %>%
  select(-sup_status_count)


degree_sum <- degree_sum_sup1 %>% 
  left_join(masters_sum, by = "sup_status", relationship = "many-to-many") %>%
  mutate(
    ms_prop = case_when(
        degree_recat == "bachelors or higher" ~ (prop_masters_degree * prop_masters_degree / 100),
        TRUE ~ NA_real_
      )
    )



creds_df_sum <- summarize_select_all(df = creds_df, prefix = "degrees")%>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))





###########################################
###########################################
###                                     ###
###  Degrees (higher than highschool)   ###
###                                     ###
###########################################
###########################################


creds_high_df <- creds_df %>%
  filter(degree_recat == "Bachelors or higher") %>%
  mutate(
    degrees = str_remove_all(degrees, "High school or equivalent,")
  )


creds_degree_sum <- summarize_select_all(df = creds_high_df, prefix = "degrees")%>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))







###################
###             ###
###  Exports    ###
###             ###
###################

#write.csv(creds_degree_sum, "_data/df_exports/credentials/creds_degree_sum.csv", row.names = FALSE)
#write.csv(degree_sum, "_data/df_exports/credentials/degree_sum.csv", row.names = FALSE)
#write.csv(degree_size_stats, "_data/df_exports/credentials/degree_size_stats.csv", row.names = FALSE)
#write.csv(cred_list_sum, "_data/df_exports/credentials/cred_list_sum.csv", row.names = FALSE)
#write.csv(cred_int_sum, "_data/df_exports/credentials/cred_int_sum.csv", row.names = FALSE)

