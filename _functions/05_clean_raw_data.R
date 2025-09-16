
clean_raw_data <- function(df) {
  df %>%
    mutate(
      sup_status = case_when(
        str_starts(supervisor_status, "Supervisor") ~ "Supervisor", 
        str_starts(supervisor_status, "Non-supervisor") ~ "Non-supervisor", 
        str_starts(supervisor_status, "Executive") ~ "Executive", 
        TRUE ~ NA_character_
      ),
      sup_status_recat = if_else(
        sup_status == "Non-supervisor", 
        "Non-supervisor", 
        "Supervisor/Executive"
      ),
      retire_recat = if_else(
        str_starts(Retirement_year, "I am"), 
        "after 2030", 
        "before 2030"
      ),
      
      region_color_cat = case_when(
        region %in% c("Region 1", "Region 2") ~ "Region 1 and 2", 
        region %in% c("Region 4", "Region 5") ~ "Region 4 and 5", 
        region== "Region 3" ~ "Region 3", 
        TRUE ~ NA_character_
      )
    )
}





