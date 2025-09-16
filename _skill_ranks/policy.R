df_policy<- read.csv(file = here("_data/policy.csv")) %>%
  
  create_scores(imp_col = policy_imp, skill_col = policy_skill) %>%
  
  recat_cols(imp_col = policy_imp, skill_col = policy_skill) %>%
  
  filter(!is.na(policy_imp)) %>%
  mutate(domain = "policy") %>%
  
  group_by(imp_recat, topic) %>%
  mutate(
    imp_recat_n = n(),
    imp_recat_prop = 100* imp_recat_n / 448
  ) %>%
  
  ungroup() 





policy_sum <- df_policy %>%
  group_by(topic, imp_recat, skill_recat) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(topic, imp_recat) %>%
  mutate(
    n_score = sum(n), 
    skill_prop = 100 * n / n_score
  ) %>%
  ungroup() %>%
  filter(!is.na(imp_recat), !is.na(skill_recat))


policy_begin2 <- policy_sum %>% filter(skill_recat == "Beginner")
