create_satisfaction_cats <- function(df, category_col = question) {
  category_col <- rlang::enquo(category_col)
  
  df %>%
    mutate(
      cat_group = case_when(
        str_detect(!!category_col, "_belonging_")              ~ "belonging",
        str_detect(!!category_col, "_input_|_voice_")          ~ "voice",        
        str_detect(!!category_col, "_job")                     ~ "job",
        str_detect(!!category_col, "_work_unit|_org|_using_|_policy_")          ~ "org & team",
        str_detect(!!category_col, "_sup")                     ~ "supervisor",
        str_detect(!!category_col, "_pay_|_benefits_|_paid_")  ~ "pay & benefits",
        str_detect(!!category_col, "_treated_|_safe_")         ~ "treated fairly",
        str_detect(!!category_col, "_balance_|_demands_|_more_|_complete_")      ~ "balance",
        TRUE ~ as.character(!!category_col)
      ) %>%
        factor(
          levels = c(
            "job", 
            "pay & benefits", 
            "balance",
            "org & team", 
            "voice", 
            "supervisor", 
            "belonging",
            "treated fairly"

          )
        )
    )
}