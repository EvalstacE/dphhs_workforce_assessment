
recat_cols <- function(data, imp_col, skill_col) {
  imp_col   <- ensym(imp_col)   # works with bare or "string"
  skill_col <- ensym(skill_col) # works with bare or "string"
  
  data %>%
    mutate(
      # normalize originals (lowercase, trim) to avoid silent mismatches
      .imp   = str_squish(str_to_lower(as.character(!!imp_col))),
      .skill = str_squish(str_to_lower(as.character(!!skill_col))),
      
      imp_recat = case_when(
        .imp %in% c("not important", "somewhat important") ~ "Not/Somewhat Important",
        .imp %in% c("moderately important", "very important") ~ "Moderately/Very Important",
        TRUE ~ NA_character_
      ),
      skill_recat = case_when(
        .skill %in% c("unable to perform", "beginner") ~ "Unable to Perform/Beginner",
        .skill %in% c("proficient", "expert") ~ "Proficient/Expert",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-.imp, -.skill)
}
