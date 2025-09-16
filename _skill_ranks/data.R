df_data <- read.csv(file = here("_data/data.csv")) %>%
  
  create_scores(imp_col = data_imp, skill_col = data_skill) %>%
  
  recat_cols(imp_col = data_imp, skill_col = data_skill) %>%
  mutate(domain = "data")



data_sum <- df_data %>%
  group_by(topic, imp_recat, skill_recat) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(topic, imp_recat) %>%
  mutate(
    n_score = sum(n), 
    skill_prop = 100 * n / n_score
  ) %>%
  ungroup() %>%
  filter(!is.na(imp_recat), !is.na(skill_recat))


data_begin <- data_sum %>% filter(skill_recat == "Beginner")
