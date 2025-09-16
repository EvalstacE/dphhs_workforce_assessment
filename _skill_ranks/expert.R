df_expert<- read.csv(file = here("_data/expert.csv")) %>%
  
  create_scores(imp_col = expert_imp, skill_col = expert_skill) %>%
  
  recat_cols(imp_col = expert_imp, skill_col = expert_skill)



expert_sum <- df_expert %>%
  group_by(topic, imp_recat, skill_recat) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(topic, imp_recat) %>%
  mutate(
    n_score = sum(n), 
    skill_prop = 100 * n / n_score
  ) %>%
  ungroup() %>%
  filter(!is.na(imp_recat), !is.na(skill_recat))


expert_begin <- expert_sum %>% filter(skill_recat == "Beginner")%>%
  mutate(domain = "expert")
