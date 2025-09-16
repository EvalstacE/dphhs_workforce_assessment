data <- read.csv(file = here("_data/comm.csv"))


data <- data %>%
  
  create_scores(imp_col = comm_imp, skill_col = comm_skill) %>%
  
  recat_cols(imp_col = comm_imp, skill_col = comm_skill)



comm <- data %>%
  group_by(topic, imp_recat, skill_recat) %>%
    summarise(n = n()) %>% ungroup() %>%
  group_by(topic, imp_recat) %>%
    mutate(
      n_score = sum(n), 
      skill_prop = 100 * n / n_score
    ) %>%
  ungroup() %>%
  filter(!is.na(imp_recat), !is.na(skill_recat))


comm_begin <- comm %>% filter(skill_recat == "Beginner") %>%
  mutate(domain = "comm")
