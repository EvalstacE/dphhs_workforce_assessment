

retire_region <- 
  two_group_count_prop(data_cleaned, 
                       group1 = region, 
                       group2 = retire_recat)


retire_leave_region <- 
  two_group_count_prop(data_cleaned, 
                       group1 = region, 
                       group2 = leave_retire_recat)



retire_or_leave_position <- 
  two_group_count_prop(data_cleaned, 
                       group1 = sup_status, 
                       group2 = leave_or_retire) 
  


retire_sum_all <- data_cleaned %>%
  group_by(leave_retire_recat) %>%
  summarise(
    total_retire = n()
  ) %>% ungroup() 