
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



master_topics <- build_master_topics(topic_paths, reps = 448) %>%
  recat_cols(imp_col = imp_rank, skill_col = skill_rank) %>%
  recat_supervisor_col()



# Create dataframes for each supervisor status
### filtering out where imp_rank is NA auto-selects for questions asked

non_sup_df <- master_topics%>% filter(supervisor_status == "Non-supervisor") %>%
  filter(!is.na(imp_rank))

sup_df <- master_topics %>% filter(supervisor_status == "Supervisor")%>%
  filter(!is.na(imp_rank))

exec_df <- master_topics %>% filter(supervisor_status == "Executive")%>%
  filter(!is.na(imp_rank))


nonsup_summary <- calc_skill_prop(non_sup_df)
sup_summary <- calc_skill_prop(sup_df)
exec_summary <- calc_skill_prop(exec_df)

