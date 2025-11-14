
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
  arrange(size, desc(prop_agree))


#################################################################
#################################################################
###             Satisfaction by Positions                     ###
#################################################################
#################################################################

s_agree_by_position <- summarise_satisfy_props(data_cleaned, sup_status) 
f_props_by_position <- summarise_freq_props(data_cleaned, sup_status)

satisfaction_position <- rbind(s_agree_by_position, f_props_by_position) %>%
  create_satisfaction_cats() %>%
  arrange(sup_status, desc(prop_agree))


#--compute diffs by question across position types
s_position_diffs <- satisfaction_position %>%
  select(question, sup_status, prop_agree) %>%
  tidyr::pivot_wider(
    id_cols   = question,
    names_from = sup_status,
    values_from = prop_agree
  ) %>%
  mutate(
    diff_exec_sup   = abs(Executive - Supervisor),
    diff_sup_nonsup = abs(Supervisor - `Non-supervisor`)
  ) %>%
  tidyr::pivot_longer(
    cols = c(`Non-supervisor`, Supervisor, Executive),
    names_to  = "sup_status",
    values_to = "prop_agree"
  ) %>%
  select(question, sup_status, diff_exec_sup, diff_sup_nonsup)

#--join diffs back on
satisfaction_position <- satisfaction_position %>%
  left_join(s_position_diffs, by = c("question", "sup_status"))


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
  arrange(agency, desc(prop_agree))

#--compute diffs by question between agency type
s_agency_diffs <- satisfaction_agency %>% 
  tidyr::pivot_wider(
    id_cols = question,
    names_from = agency,
    values_from = prop_agree
  ) %>%
  mutate(
    diff = abs(`County Health Department` - `Tribal Health Department`)
  ) %>%
  pivot_longer(
    cols = c(`County Health Department`, `Tribal Health Department`),
    names_to = "agency",
    values_to = "prop_agree"
  ) %>%
  select(agency, question, diff)

#--join diffs back on
satisfaction_agency <- satisfaction_agency %>%
  left_join(s_agency_diffs, by = c("question", "agency"))




#################################################################
#################################################################
###              Satisfaction by Leaving                      ###
#################################################################
#################################################################

s_agree_by_leave <- summarise_satisfy_props(data_cleaned, leave_or_retire) 
f_props_by_leave <- summarise_freq_props(data_cleaned, leave_or_retire)

satisfaction_leave <- rbind(s_agree_by_leave, f_props_by_leave) %>%
  create_satisfaction_cats() %>%
  arrange(leave_or_retire, desc(prop_agree))

#--compute diffs by question across position types
s_leave_diffs <- satisfaction_leave %>%
  select(question, leave_or_retire, prop_agree) %>%
  tidyr::pivot_wider(
    id_cols   = question,
    names_from = leave_or_retire,
    values_from = prop_agree
  ) %>%
  mutate(
    diff_leaving   = abs(`not leaving or retiring` - `leaving not retiring`),
  ) %>%
  tidyr::pivot_longer(
    cols = c(`not leaving or retiring`, `leaving not retiring`),
    names_to  = "leave_or_retire",
    values_to = "prop_agree"
  ) %>%
  select(question, leave_or_retire, diff_leaving)

#--join diffs back on
satisfaction_leave <- satisfaction_leave %>%
  left_join(s_leave_diffs, by = c("question", "leave_or_retire")) %>%
  filter(leave_or_retire != "retiring")


##--Exported dfs: 
#write.csv(satisfaction_leave, "_www/df_exports/satisfaction_df_exports/satisfaction_leave.csv", row.names = FALSE)
#write.csv(satisfaction_position, "_www/df_exports/satisfaction_df_exports/satisfaction_position.csv", row.names = FALSE)
#write.csv(satisfaction_agency, "_www/df_exports/satisfaction_df_exports/satisfaction_agency.csv", row.names = FALSE)
#write.csv(satisfaction_size, "_www/df_exports/satisfaction_df_exports/satisfaction_size.csv", row.names = FALSE)
