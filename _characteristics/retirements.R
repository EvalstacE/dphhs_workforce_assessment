retire_sum_region <- data_cleaned %>%
  group_by(region, retire_recat) %>%
  summarise(
    total_retire = n()
  ) %>% ungroup() %>%
  group_by(region) %>%
  mutate(total_region = sum(total_retire), 
         retire_prop = 100*total_retire / total_region) %>%
  ungroup() %>%
  filter(retire_recat == "before 2030") %>%
  select(-retire_recat)




retire_sum_all <- data_cleaned %>%
  group_by(leave_retire_recat) %>%
  summarise(
    total_retire = n()
  ) %>% ungroup() 