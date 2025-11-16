
#################################################################
#################################################################
###           Satisfaction by Jurisdiction Size               ###
#################################################################
#################################################################
### -- Satisfaction by Jurisdiction Size
s_agree_by_size <- summarise_satisfy_props(data_cleaned, size) 
f_props_by_size <- summarise_freq_props(data_cleaned, size)

satisfaction_size <- rbind(s_agree_by_size, f_props_by_size) %>%
  create_satisfaction_cats() %>%
  arrange(size, desc(prop_agree))%>%
  rename(group_col = "size") %>%
  mutate(grouping = "jd_size") %>%
  relocate(grouping, .before = group_col)



#################################################################
#################################################################
###             Satisfaction by Positions                     ###
#################################################################
#################################################################

s_agree_by_position <- summarise_satisfy_props(data_cleaned, sup_status) 
f_props_by_position <- summarise_freq_props(data_cleaned, sup_status)

satisfaction_position <- rbind(s_agree_by_position, f_props_by_position) %>%
  create_satisfaction_cats() %>%
  arrange(sup_status, desc(prop_agree))%>%
  rename(group_col = "sup_status") %>%
  mutate(grouping = "position_type") %>%
  relocate(grouping, .before = group_col)
  


#--compute diffs by question across position types
s_position_diffs <- satisfaction_position %>%
  select(question, group_col, prop_agree) %>%
  tidyr::pivot_wider(
    id_cols   = question,
    names_from = group_col,
    values_from = prop_agree
  ) %>%
  mutate(
    diff_exec_sup   = abs(Executive - Supervisor),
    diff_sup_nonsup = abs(Supervisor - `Non-supervisor`)
  ) %>%
  tidyr::pivot_longer(
    cols = c(`Non-supervisor`, Supervisor, Executive),
    names_to  = "group_col",
    values_to = "prop_agree"
  ) %>%
  select(question, group_col, diff_exec_sup, diff_sup_nonsup)

#--join diffs back on
satisfaction_position2 <- satisfaction_position %>%
  left_join(s_position_diffs, by = c("question", "group_col")) %>%
  mutate(
    group_col = 
      factor(
        group_col, 
        levels = c("Non-supervisor", "Supervisor", "Executive"), 
        ordered = TRUE
      )
  )



#################################################################
#################################################################
###               Satisfaction by Agency                      ###
#################################################################
#################################################################

s_agree_by_agency <- summarise_satisfy_props(data_cleaned, agency) 
f_props_by_agency <- summarise_freq_props(data_cleaned, agency)
agree_acency_overall <- summ_agree_overall(data_cleaned, agency)

satisfaction_agency <- rbind(s_agree_by_agency, f_props_by_agency) %>%
  create_satisfaction_cats() %>%
  arrange(agency, desc(prop_agree))%>%
  rename(group_col = "agency") %>%
  mutate(grouping = "agency_type") %>%
  relocate(grouping, .before = group_col)

#--compute diffs by question between agency type
s_agency_diffs <- satisfaction_agency %>% 
  tidyr::pivot_wider(
    id_cols = question,
    names_from = group_col,
    values_from = prop_agree
  ) %>%
  mutate(
    diff = abs(`County Health Department` - `Tribal Health Department`)
  ) %>%
  pivot_longer(
    cols = c(`County Health Department`, `Tribal Health Department`),
    names_to = "group_col",
    values_to = "prop_agree"
  ) %>%
  select(group_col, question, diff)

#--join diffs back on
satisfaction_agency2 <- satisfaction_agency %>%
  left_join(s_agency_diffs, by = c("question", "group_col"))




#################################################################
#################################################################
###              Satisfaction by Leaving                      ###
#################################################################
#################################################################

s_agree_by_leave <- summarise_satisfy_props(data_cleaned, leave_or_retire) 
f_props_by_leave <- summarise_freq_props(data_cleaned, leave_or_retire)

satisfaction_leave <- rbind(s_agree_by_leave, f_props_by_leave) %>%
  create_satisfaction_cats() %>%
  arrange(leave_or_retire, desc(prop_agree))%>%
  rename(group_col = "leave_or_retire") %>%
  mutate(grouping = "leave_retire") %>%
  relocate(grouping, .before = group_col)

#--compute diffs by question across position types
s_leave_diffs <- satisfaction_leave %>%
  select(question, group_col, prop_agree) %>%
  tidyr::pivot_wider(
    id_cols   = question,
    names_from = group_col,
    values_from = prop_agree
  ) %>%
  mutate(
    diff_leaving   = abs(`not leaving or retiring` - `leaving not retiring`),
  ) %>%
  tidyr::pivot_longer(
    cols = c(`not leaving or retiring`, `leaving not retiring`),
    names_to  = "group_col",
    values_to = "prop_agree"
  ) %>%
  select(question, group_col, diff_leaving)

#--join diffs back on
satisfaction_leave2 <- satisfaction_leave %>%
  left_join(s_leave_diffs, by = c("question", "group_col")) %>%
  filter(group_col != "retiring")



#################################################################
#################################################################
###           Satisfaction by Job Satisfaction                ###
#################################################################
#################################################################

s_agree_by_satisfy <- summarise_satisfy_props(data_cleaned, satisfied_job) 
f_props_by_satisfy <- summarise_freq_props(data_cleaned, satisfied_job)

satisfaction_satisfy <- rbind(s_agree_by_satisfy, f_props_by_satisfy) %>%
  create_satisfaction_cats() %>%
  arrange(satisfied_job, desc(prop_agree))%>%
  rename(group_col = "satisfied_job") %>%
  mutate(grouping = "job_satisfied") %>%
  relocate(grouping, .before = group_col)

#--compute diffs by question between agency type
s_satisfy_diffs <- satisfaction_satisfy %>% 
  tidyr::pivot_wider(
    id_cols = question,
    names_from = group_col,
    values_from = prop_agree
  ) %>%
  mutate(
    diff = abs(`satisfied with job` - `not satisfied with job`)
  ) %>%
  pivot_longer(
    cols = c(`satisfied with job`, `not satisfied with job`),
    names_to = "group_col",
    values_to = "prop_agree"
  ) %>%
  select(group_col, question, diff)

#--join diffs back on
satisfaction_satisfy2 <- satisfaction_satisfy %>%
  left_join(s_satisfy_diffs, by = c("question", "group_col"))





#################################################################
#################################################################
###               Statewide Satisfaction                      ###
#################################################################
#################################################################


s_props_statewide <- summarise_satisfy_props(data_cleaned)
f_props_statewide <- summarise_freq_props(data_cleaned)

satisfaction_statewide <- rbind(s_props_statewide, f_props_statewide) %>%
  create_satisfaction_cats() %>%
  arrange(desc(prop_agree)) %>%
  group_by(cat_group) %>%
  mutate(
    min_prop = min(prop_agree), 
    max_prop = max(prop_agree), 
    rng_prop = max_prop - min_prop
  ) %>%
  ungroup()




satisfaction_all_grps <- 
  rbind(
    satisfaction_size, 
    satisfaction_position, 
    satisfaction_agency, 
    satisfaction_leave,
    satisfaction_satisfy
  ) %>%
  filter(group_col != "not leaving or retiring")



##--Exported dfs: 
#write.csv(satisfaction_leave2, "_www/df_exports/satisfaction_df_exports/satisfaction_leave.csv", row.names = FALSE)
#write.csv(satisfaction_position2, "_www/df_exports/satisfaction_df_exports/satisfaction_position.csv", row.names = FALSE)
#write.csv(satisfaction_agency2, "_www/df_exports/satisfaction_df_exports/satisfaction_agency.csv", row.names = FALSE)
#write.csv(satisfaction_size, "_www/df_exports/satisfaction_df_exports/satisfaction_size.csv", row.names = FALSE)
#write.csv(satisfaction_statewide, "_www/df_exports/satisfaction_df_exports/satisfaction_statewide.csv", row.names = FALSE)
#write.csv(satisfaction_all_grps, "_www/df_exports/satisfaction_df_exports/satisfaction_all_grps.csv", row.names = FALSE)
#write.csv(satisfaction_satisfy2, "_www/df_exports/satisfaction_df_exports/satisfaction_job_satisfied.csv", row.names = FALSE)
  

