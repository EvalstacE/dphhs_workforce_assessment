####################################################################
####      calculates the maximum years across all three     ########
### columns (e.g. current position, at agency, in public health) ###
####################################################################

calc_max_yrs <- function(a, b, c, d) {
  out <- pmax(a, b, c, d, na.rm = TRUE)
  ifelse(is.finite(out), out, NA_real_)
}

#################################################
#####   max yrs in public health category   #####
#################################################
create_yrs_cat <- function(max_yrs_col) {
  case_when(
    between(max_yrs_col, 0, 4)   ~ "0-4",
    between(max_yrs_col, 5, 9)   ~ "5-9", 
    between(max_yrs_col, 10, 200) ~ "10+",
    TRUE                         ~ "Unknown"
  ) %>%
    factor(levels = c("0-4", "5-9", "10+", "Unknown"), ordered = TRUE)
}

