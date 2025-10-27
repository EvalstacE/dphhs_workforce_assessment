
## Bring in skill / training data ---
topic_paths <- c(
  "_data/skill_topics/A_comm.csv",
  "_data/skill_topics/B_data.csv",
  "_data/skill_topics/C_justice.csv",
  "_data/skill_topics/D_budget.csv",
  "_data/skill_topics/E_change.csv",
  "_data/skill_topics/F_systems.csv",
  "_data/skill_topics/G_engage.csv",
  "_data/skill_topics/H_partners.csv",
  "_data/skill_topics/I_policy.csv",
  "_data/skill_topics/J_expert.csv"
)

## full question text by section + question ID ---
question_text_df <- read.csv(file = here("_data/training_question_text.csv"))


## create final master df ---
master_topics <- build_master_topics(topic_paths, reps = 448) %>%
  recat_cols(imp_col = imp_rank, skill_col = skill_rank) %>%
  recat_supervisor_col() %>%
  left_join(question_text_df, by = c("section", "question_id"))


## create separate tables for each position type

non_sup_df <- master_topics%>% filter(supervisor_status == "Non-supervisor") %>%
  filter(!is.na(imp_rank))

sup_df <- master_topics %>% filter(supervisor_status == "Supervisor")%>%
  filter(!is.na(imp_rank))

exec_df <- master_topics %>% filter(supervisor_status == "Executive")%>%
  filter(!is.na(imp_rank))


nonsup_summary <- calc_skill_prop(non_sup_df) %>% arrange(desc(priority_score))
sup_summary <- calc_skill_prop(sup_df) %>% arrange(desc(priority_score))
exec_summary <- calc_skill_prop(exec_df) %>% arrange(desc(priority_score))


all_sum <- rbind(nonsup_summary, sup_summary, exec_summary) %>%
  filter(skill_recat == "Unable to Perform/Beginner") %>%
  mutate(domain = factor(domain, levels = 
                           c("budget", "change",  "policy", "systems", "partners",
                             "justice", "engage", "comm",  "data", "expert")),
         
         domain_cat = case_when(
           domain %in% c("budget", "change",  "policy", "systems") ~ "budget, change, policy, systems",
           domain %in% c("partners",  "engage") ~ "partners, engage",
           domain %in% c("comm",  "data", "expert") ~ "comm, data, expert",
           domain == "justice" ~ "DEI/justice", 
           TRUE ~ NA_character_
         ),
         
         
         
         supervisor_status = factor(supervisor_status, levels = 
                                      c("Non-supervisor", "Supervisor", "Executive"))
         
         
  ) %>%
  
  left_join(question_text_df, by = c("section", "question_id"))


#write.csv(all_sum, "_www/df_exports/all_score_summary.csv", row.names = FALSE)





