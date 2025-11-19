
not_satisfied_df <- data_cleaned %>% filter(satisfied_job == "not satisfied with job")


satisfied_size <- two_group_count_prop(data_cleaned, size, satisfied_job)

satisfied_pos <- two_group_count_prop(data_cleaned, sup_status, satisfied_job)


satisfied_agency <- two_group_count_prop(data_cleaned, agency, satisfied_job)


mh <- data_cleaned %>%
  group_by(mental_emotional_health) %>%
  summarise(
    n = n(),
    pct = 100*n/nrow(data_cleaned)
  )