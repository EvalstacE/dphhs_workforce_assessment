
# Bring in datasets
data_dirs <- c(
  here::here("_www", "df_exports", "satisfaction_df_exports")
)

csv_files <- purrr::map(data_dirs, ~ list.files(
  path = .x,
  pattern = "\\.csv$",
  full.names = TRUE
)) %>% unlist()

purrr::walk(csv_files, function(file_path) {
  obj_name <- tools::file_path_sans_ext(basename(file_path))
  df <- readr::read_csv(file_path, show_col_types = FALSE)
  assign(obj_name, df, envir = knitr::knit_global())
})




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


##################################
##################################
### Staying vs Leaving Reasons ### 
##################################
##################################

staying_reasons <- data_cleaned %>%
  select(-why_staying_other) %>%
  filter(leave_or_retire == "not leaving or retiring") %>%
  summarize_select_all(data_cleaned, prefix = "why_staying")

#write.csv(staying_reasons, "_data/df_exports/leaving_staying_exports/staying_reasons.csv", row.names = FALSE)

leaving_df <- data_cleaned %>%
  filter(leave_or_retire == "leaving not retiring") 

leaving_reasons <- data_cleaned %>%
  filter(leave_or_retire == "leaving not retiring") %>%
  summarize_select_all(data_cleaned, prefix = "leaving_org_")


retire_experience <- two_group_count_prop(data_cleaned, retire_recat, max_yrs_cat)

leave_experience <- two_group_count_prop(leaving_df, leave_or_retire, max_yrs_cat)



experience_sum <- data_cleaned %>%
  group_by(max_yrs_cat) %>%
  summarise(
    n = n(), 
    pct = 100*n/448,
    .groups = "drop"
  )

age_sum <- data_cleaned %>%
  group_by(age_group_lg) %>%
  summarise(
    n = n(), 
    pct = 100*n/448,
    .groups = "drop"
  )



