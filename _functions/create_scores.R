create_scores <- function(data, imp_col, skill_col) {
  data %>%
    mutate(
      imp_score = case_when(
        {{ imp_col }} == "Not Important" ~ 1L,
        {{ imp_col }} == "Somewhat important" ~ 2L,
        {{ imp_col }} == "Moderately important" ~ 3L,
        {{ imp_col }} == "Very important" ~ 4L,
        TRUE ~ NA_integer_
      ),
      skill_score = case_when(
        {{ skill_col }} == "Beginner" ~ 1L,
        {{ skill_col }} == "Proficient" ~ 2L,
        {{ skill_col }} == "Expert" ~ 3L,
        TRUE ~ NA_integer_
      )
    )
}