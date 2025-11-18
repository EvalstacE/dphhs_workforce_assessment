
order_questions <- function(df, 
                            group_col = cat_group, 
                            question_col = question) {
  
  group_col    <- rlang::enquo(group_col)
  question_col <- rlang::enquo(question_col)
  
  df %>%
    arrange(!!group_col, !!question_col) %>%
    mutate(
      cat_rank = as.numeric(!!group_col),
      !!question_col := forcats::fct_reorder(!!question_col, cat_rank)
    ) %>%
    select(-cat_rank)
}