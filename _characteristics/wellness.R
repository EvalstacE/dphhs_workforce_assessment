
mh_df <- data_cleaned %>%
  select(region, size, sup_status, mental_emotional_health, s_comfort_using_PTO_recat, 
         s_agency_mh_policy_support_recat, burnout_recat, starts_with("well_")) %>%
  mutate(
    mental_emotional_health = factor(mental_emotional_health, 
                                     levels = c("Poor", "Fair", "Good", "Very Good", "Excellent")),
    mh_recat = if_else(mental_emotional_health %in% 
                         c("Very Good", "Excellent"), "VG/Excellent", mental_emotional_health),
    mh_recat = factor(mh_recat, 
                      levels = c("Poor", "Fair", "Good", "VG/Excellent")
                      ),
    mh_poor_fair = if_else(mh_recat %in% c("Fair", "Poor"), "fair_poor", "good_better"), 
    mh_poor_fair = factor(mh_poor_fair, levels = c("good_better", "fair_poor"))
  ) %>%
  rename("w_pol_support" = "s_agency_mh_policy_support_recat")


mh_sum <- mh_df %>%
  group_by(mental_emotional_health) %>%
  summarise(
    mh_n = n(), 
    mh_prop = 100*mh_n/448
  )

mh_sum2 <- mh_df %>%
  group_by(mh_poor_fair) %>%
  summarise(
    mh_n = n(), 
    mh_prop = 100*mh_n/448
  )



### -- agree that dept. has supportive mental health policy by mh status

well_sup_all <- mh_df %>%
  group_by(w_pol_support) %>%
  summarise(
    n_cnt = n(),
    prop = 100*n_cnt/448
  )

well_sup_mh <- 
  summarise_group_props(mh_df, mh_poor_fair, w_pol_support, rename_cols = TRUE)%>%
  filter(category == "Agree")


well_sup_pos <-   
  summarise_group_props(mh_df, sup_status, w_pol_support, rename_cols = TRUE)%>%
  filter(category == "Agree")

well_sup_size <-   
  summarise_group_props(mh_df, size, w_pol_support, rename_cols = TRUE)%>%
  filter(category == "Agree")

well_sup_region <-   
  summarise_group_props(mh_df, region, w_pol_support, rename_cols = TRUE)%>%
  filter(category == "Agree")

well_sup_group_stats <- 
  rbind(well_sup_pos, well_sup_size, well_sup_region, well_sup_mh)

summary(well_sup_group_stats$prop)


support_mh_plot <- 
make_prop_plot(
  well_support_mh,
  filter_col   = category,
  filter_value = "Agree",
  x_var        = group_cat
) + 
  ylim(0,100)


ggsave(
  filename = here("_www/plot_exports/support_mh_plot.png"),
  plot = support_mh_plot,
  width = 5,
  height = 10,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)

############













### -- Wellness Programs


##-- interest in programs

well_int_all <- mh_df %>%
  group_by(well_interest) %>%
  summarise(
    n_cnt = n(),
    prop = 100*n_cnt/448
  )



well_int_mh <- 
  summarise_group_props(mh_df, mh_poor_fair, well_interest, rename_cols = TRUE)%>%
  filter(category == "Yes")



int_mh_plot <- 
  make_prop_plot(
    well_int_mh,
    filter_col   = category,
    filter_value = "Yes",
    x_var        = group_cat
  ) + 
  ylim(0,100)



well_int_pos <-   
  summarise_group_props(mh_df, sup_status, well_interest, rename_cols = TRUE)%>%
  filter(category == "Yes")


int_pos_plot <- 
  make_prop_plot(
    well_int_pos,
    filter_col   = category,
    filter_value = "Yes",
    x_var        = group_cat
  ) + 
  ylim(0,100)

well_int_size <-   
  summarise_group_props(mh_df, size, well_interest, rename_cols = TRUE)%>%
  filter(category == "Yes")


int_size_plot <- 
  make_prop_plot(
    well_int_size,
    filter_col   = category,
    filter_value = "Yes",
    x_var        = group_cat
  ) + 
  ylim(0,100)


well_int_region <-   
  summarise_group_props(mh_df, region, well_interest, rename_cols = TRUE)%>%
  filter(category == "Yes")

int_region_plot <- 
  make_prop_plot(
    well_int_region,
    filter_col   = category,
    filter_value = "Yes",
    x_var        = group_cat
  ) + 
  ylim(0,100)



well_int_group_stats <- 
  rbind(well_int_pos, well_int_size, well_int_region, well_int_mh)

summary(well_int_group_stats$prop)



##-- participated in programs

well_part_all <- mh_df %>%
  group_by(well_participate) %>%
  summarise(
    n_cnt = n(),
    prop = 100*n_cnt/448
  )

##--program provider list

w_part_df <- mh_df %>% filter(well_participate == "Yes")

well_org_list <- summarize_select_all(df = w_part_df, prefix = "well_org")%>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))



##--wellness interest program list

well_int_list <- summarize_select_all(df = mh_df, prefix = "well_interest_type")%>%
  mutate(pref_label = paste0(round(prop*100),"%", " ", "(", n, ")"))


