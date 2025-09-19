
recat_degree <- function(df, col_name) {
  col <- rlang::ensym(col_name)
  
  df %>%
    mutate(
      highest_degree = case_when(
        # Masters or higher (catch first so it's prioritized)
        str_detect(!!col, regex("PhD|ScD|doctorate|MD|DO|PharmD|DNP|JD|DrPH|MBA|MPA|MPH|MSN|MSW|MEd|MHSA|MA/MS|Other master's", ignore_case = TRUE)) ~ "Masters or higher",
        
        # Bachelor's
        str_detect(!!col, regex("BS/BA|BSN|BSPH|BAPH|Other bachelor's", ignore_case = TRUE)) ~ "Bachelors",
        
        # Associate's
        str_detect(!!col, regex("Associate", ignore_case = TRUE)) ~ "Associates",
        
        # High school / equivalent
        str_detect(!!col, regex("High school|GED|equivalent", ignore_case = TRUE)) ~ "High school or equivalent",
        
        # Currently pursuing but no degree listed
        str_detect(!!col, regex("pursuing", ignore_case = TRUE)) ~ NA_character_,
        
        # Missing
        is.na(!!col) ~ NA_character_,
        
        # Default
        TRUE ~ "Other/Uncategorized"
      )
    )
}
