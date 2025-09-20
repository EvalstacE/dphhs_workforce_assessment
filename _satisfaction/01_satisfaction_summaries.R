
s_agree_by_agency <- summarise_satisfy_props(data_cleaned, agency)


f_props_by_sup <- summarise_freq_props(data_cleaned, sup_status)


burnout_position <- 
  two_group_count_prop(data_cleaned, 
                       group1 = sup_status, 
                       group2 = burnout_recat)


s_agree_by_sup <- summarise_satisfy_props(data_cleaned, sup_status) %>%
  mutate(
    cat_group = case_when(
      str_detect(category, "_job")              ~ "job", 
      str_detect(category, "_work_unit")        ~ "team",
      str_detect(category, "_sup")              ~ "supervisor", 
      str_detect(category, "_org")              ~ "org", 
      str_detect(category, "_pay|_benefit")     ~ "pay & benefits", 
      str_starts(category, "s_treated_")        ~ "treated fairly",
      str_starts(category, "s_belonging_")      ~ "belonging",
      TRUE ~ category
    ) %>%
      factor(
        levels = c("job", "team", "supervisor", "org", 
                   "pay & benefits", "treated fairly", "belonging")
      ),
    
    sup_status = factor(sup_status, 
                        levels = c("Non-supervisor", "Supervisor", "Executive"))
  )

