df_engage <- read.csv(file = here("_data/engage.csv")) %>%
  
  create_scores(imp_col = engage_imp, skill_col = engage_skill) %>%
  
  recat_cols(imp_col = engage_imp, skill_col = engage_skill)



engage_sum <- df_engage %>%
  group_by(topic, imp_recat, skill_recat) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(topic, imp_recat) %>%
  mutate(
    n_score = sum(n), 
    skill_prop = 100 * n / n_score
  ) %>%
  ungroup() %>%
  filter(!is.na(imp_recat), !is.na(skill_recat))


engage_begin <- engage_sum %>% filter(skill_recat == "Beginner")%>%
  mutate(domain = "engage")
