##################################################
### Leaving for reasons other than retirement ####
##################################################

leaving_plans <- data_cleaned %>%
  group_by(considering_leaving, retire_recat) %>%
  summarise(total = n())

### leave_not_retire created in 
#####  clean_raw_data() function
leaving_df <- data_cleaned %>%
  filter(!is.na(leave_not_retire))

not_leaving_df <- data_cleaned %>%
  filter(is.na(leave_not_retire))


leave_age <- leaving_df %>%
  group_by(age_group) %>%
  summarise(n = n(),
            prop = 100*n/74)


burnout_not_leaving <- not_leaving_df %>%
  group_by(burnout) %>%
  summarise(totals = n(),
            prop = 100*totals / 448)


burnout_leaving <- leaving_df %>%
  group_by(burnout) %>%
  summarise(totals = n(),
            prop = 100*totals / length(leaving_df))
