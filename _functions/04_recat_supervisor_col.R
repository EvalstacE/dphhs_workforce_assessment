
recat_supervisor_col <- function(df) {
  df %>%
    mutate(
      supervisor_status = case_when(
        str_starts(supervisor_status, "Supervisor") ~ "Supervisor", 
        str_starts(supervisor_status, "Non-supervisor") ~ "Non-supervisor", 
        str_starts(supervisor_status, "Executive") ~ "Executive", 
        TRUE ~ NA_character_
      ),
      sup_status_recat = if_else(
        supervisor_status == "Non-supervisor", 
        "Non-supervisor", 
        "Supervisor/Executive"
      )
    )
}