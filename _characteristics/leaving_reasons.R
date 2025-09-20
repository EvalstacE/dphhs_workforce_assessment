##################################################
### Leaving for reasons other than retirement ####
##################################################

leaving_plans <- data_cleaned %>%
  group_by(considering_leaving, retire_recat) %>%
  summarise(total = n())

### leave_or_retire created in 
#####  clean_raw_data() function




retire_or_leave_all <- data_cleaned %>%
  group_by(leave_or_retire) %>%
  summarise(n = n(), 
            prop = 100*n/448)



leave_df <- data_cleaned %>% filter(leave_or_retire == "leaving not retiring")

    leave_age_all <- 
      two_group_count_prop(leave_df, 
                           group1 = leave_or_retire, 
                           group2 = age_group_lg) 
    
    
leaving_satisfaction <- summarise_satisfy_props(
      leave_df,
      leave_or_retire,
      prefix = "s_",
      suffix = "_recat",
      level  = "Agree"
    )

leaving_freq_qs <- summarise_freq_props(leave_df, leave_or_retire)


    
    
leave_burnout <- 
  two_group_count_prop(data_cleaned, 
                       group1 = leave_or_retire, 
                       group2 = burnout)


