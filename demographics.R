




# Bring in demogrpahic data
data <- read.csv(file = here("_data/demographics.csv")) 
data_cleaned <- clean_raw_data(data)


retire_sum_region <- data_cleaned %>%
  group_by(region, retire_recat) %>%
    summarise(
    total_retire = n()
    ) %>% ungroup() %>%
  group_by(region) %>%
  mutate(total_region = sum(total_retire), 
         retire_prop = 100*total_retire / total_region) %>%
  ungroup()

retire_sum_all <- data_cleaned %>%
  group_by(retire_recat) %>%
  summarise(
    total_retire = n()
  ) %>% ungroup() 

58/390


sum <- data_cleaned %>%
  group_by(size, supervisor_status) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(size) %>%
    mutate(total = sum(n), 
           prop = 100 * n / total)



53/60


ggplot(data_cleaned) + 
  geom_histogram(aes(x = current_yrs)) + 
  facet_wrap(~supervisor_status)


