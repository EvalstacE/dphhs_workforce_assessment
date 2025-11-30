##################################################
# === enforce factors across common cat cols === #


enforce_cat_factors <- function(df, sup_col, size_col) {
  sup_quo  <- rlang::enquo(sup_col)
  size_quo <- rlang::enquo(size_col)
  
  out <- df
  
  # Supervisor categories
  sup_lvls <- c("Non-supervisor", "Supervisor", "Executive")
  sup_lvls_lower <- tolower(sup_lvls)
  
  if (!rlang::quo_is_missing(sup_quo)) {
    nm <- rlang::as_name(sup_quo)
    
    out <- out %>%
      mutate(
        !!nm := {
          x <- .data[[nm]]
          # match to canonical cased levels
          idx <- match(tolower(x), sup_lvls_lower)
          factor(sup_lvls[idx], levels = sup_lvls)
        }
      )
  }
  
  # Size categories
  size_lvls <- c("Frontier", "Small", "Medium", "Large")
  size_lvls_lower <- tolower(size_lvls)
  
  if (!rlang::quo_is_missing(size_quo)) {
    nm <- rlang::as_name(size_quo)
    
    out <- out %>%
      mutate(
        !!nm := {
          x <- .data[[nm]]
          idx <- match(tolower(x), size_lvls_lower)
          factor(size_lvls[idx], levels = size_lvls)
        }
      )
  }
  
  out
}
