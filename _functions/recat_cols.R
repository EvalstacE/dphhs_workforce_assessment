recat_cols <- function(data, imp_col, skill_col) {
  data %>%
    mutate(
      
      imp_recat = case_when(
        {{ imp_col }} == "Not Important" ~ "Not/Somewhat Important",
        {{ imp_col }} == "Somewhat important" ~ "Not/Somewhat Important",
        {{ imp_col }} == "Moderately important" ~ "Moderately/Very important",
        {{ imp_col }} == "Very important" ~ "Moderately/Very important",
        TRUE ~ NA_character_
      ),
      
      skill_recat = case_when(
        {{ skill_col }} == "Beginner" ~ "Beginner",
        {{ skill_col }} == "Proficient" ~ "Proficient or Expert",
        {{ skill_col }} == "Expert" ~ "Proficient or Expert",
        TRUE ~ NA_character_
      )
    )
}