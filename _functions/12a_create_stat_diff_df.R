get_topic_diff <- function(topic_question,
                           all_df          = satisfaction_all_grps,
                           state_df        = satisfaction_statewide,
                           buffer          = 2,
                           exclude_groups  = c(
                             "not leaving or retiring", "leaving not retiring", 
                             "satisfied with job", "not satisfied with job", 
                             "statewide_results"
                           ),
                           grouping_levels = c("agency_type", "position_type", "jd_size"),
                           group_levels    = c(
                             "County Health Department", "Tribal Health Department", 
                             "Non-supervisor", "Supervisor", "Executive", 
                             "Frontier", "Small", "Medium", "Large"
                           )) {
  
  # statewide CI for this question
  state_row <- state_df %>%
    dplyr::filter(question == topic_question)
  
  state_lwr <- state_row$ci_lower
  state_upr <- state_row$ci_upper
  
  # full subset for this topic
  df_subset <- diff_vs_state(
    topic_question = topic_question,
    all_df         = all_df,
    state_df       = state_df,
    buffer         = buffer,
    exclude_groups = exclude_groups
  ) %>%
    dplyr::mutate(
      grouping  = factor(grouping,  levels = grouping_levels),
      group_col = factor(group_col, levels = group_levels)
    )
  
  # only diff / near diff rows
  df_diff <- df_subset %>%
    dplyr::filter(is_diff %in% c("diff", "near diff"))
  
  # return everything as a list
  list(
    topic_question = topic_question,
    df_subset      = df_subset,
    df_diff        = df_diff,
    state_lwr      = state_lwr,
    state_upr      = state_upr
  )
}

