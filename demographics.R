

data <- read.csv(file = here("_data/demographics.csv"))

unique(data$supervisor_status)

data_cleaned <- data %>%
  mutate(
    sup_status = case_when(
      supervisor_status = str_starts(supervisor_status, "Supervisor") ~ "Supervisor", 
      supervisor_status = str_starts(supervisor_status, "Non-supervisor") ~ "Non-supervisor", 
      supervisor_status = str_starts(supervisor_status, "Executive") ~ "Executive", 
      TRUE ~ NA_character_
    ),
    
    sup_status_recat = if_else(sup_status == "Non-supervisor", "Non-supervisor", "Supervisor/Executive"),
    retire_recat = if_else(str_starts(Retirement_year, "I am"), "after 2030", "before 2030")
    
  )

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


