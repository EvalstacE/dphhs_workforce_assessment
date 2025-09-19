##################################
# === create age group category === #
create_age_group <- function(age_col) {
  case_when(
    between(age_col, 18, 34) ~ "18-34",
    between(age_col, 35, 44) ~ "35-44", 
    between(age_col, 45, 54) ~ "45-54",
    between(age_col, 55, 64) ~ "55-64",
    between(age_col, 65, 74) ~ "65-74",
    between(age_col, 75, 84) ~ "75-84",
    between(age_col, 85, 200) ~ "85+",
    TRUE ~ "Unknown"
  )
}

