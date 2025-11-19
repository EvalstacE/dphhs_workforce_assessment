
clean_raw_data <- function(df) {
  df %>%
  recat_degree(Degrees) %>%
    
  mutate(
    
      size = factor(size, levels = c("Frontier", "Small", "Medium", "Large"), ordered = TRUE),
      
      sup_status = case_when(
        str_starts(supervisor_status, "Supervisor") ~ "Supervisor", 
        str_starts(supervisor_status, "Non-supervisor") ~ "Non-supervisor", 
        str_starts(supervisor_status, "Executive") ~ "Executive", 
        TRUE ~ NA_character_
      ),
      
      sup_status = factor(sup_status, levels = c("Non-supervisor", "Supervisor", "Executive"), ordered = TRUE),
  
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
      
      leave_or_retire = case_when(
        retire_recat == "before 2030" | leaving_plans == "Retire" ~ "retiring",
        retire_recat == "after 2030" & considering_leaving == "Yes" & leaving_plans != "Retire" ~ "leaving not retiring",
        TRUE ~ "not leaving or retiring"

      ),
      
      
      burnout_recat = case_when(
        burnout == "I have no symptoms of burnout" ~ "None", 
        burnout == "I have one or more symptoms of burnout that come and go away" ~ "Some", 
        
        burnout %in% c( "I have one or more symptoms of burnout that won't go away",
                        "I am completely burnt out, my symptoms won't go away") ~ "Chronic or Complete",

        TRUE ~ NA_character_
        
      ),
      
      across(
        .cols = starts_with("s_"),
        .fns = recode_agree,
        .names = "{.col}_recat"
      ),
      
      
      across(
        .cols = starts_with("f_"),
        .fns = recode_freq,
        .names = "{.col}_recat"
      ),
      

      region_color_cat = case_when(
        region %in% c("Region 1", "Region 2") ~ "Region 1 and 2", 
        region %in% c("Region 4", "Region 5") ~ "Region 4 and 5", 
        region== "Region 3" ~ "Region 3", 
        TRUE ~ NA_character_
      ),
      
      degree_recat = case_when(
        highest_degree %in% c("Bachelors", "Masters or higher") ~ "Bachelors or higher", 
        TRUE ~ "less than Bachelors"),
      
      age = case_when(
        age == 1968 ~ 57, 
        age < 18 ~ NA_integer_, 
        TRUE ~ age
      ),
      
      age_group = create_age_group(age),
      age_group_lg = create_age_group_lg(age),
      
      max_yrs = calc_max_yrs(current_yrs,
                              agency_yrs,
                              ph_practice_yrs,
                             ph_mangmt_yrs)
      
      
    ) %>%
    
    mutate(
      max_yrs_cat =  create_yrs_cat(max_yrs),
      
      satisfied_job = 
        case_when(
          s_job_recat == "Agree" ~ "satisfied with job", 
          TRUE ~ "not satisfied with job"
        )
    )
  
}





