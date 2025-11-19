

diff_vs_state <- function(topic_question,
                          all_df   = satisfaction_all_grps,
                          state_df = satisfaction_statewide,
                          buffer   = 2,
                          exclude_groups = NULL) {
  

all_topic <- all_df %>%
    filter(
      question == topic_question
    )
  
  state_topic <- state_df %>%
    filter(question == topic_question)
  

  state_lwr <- state_topic %>% pull(ci_lower)
  state_upr <- state_topic %>% pull(ci_upper)
  

  common_cols <- intersect(colnames(all_topic), colnames(state_topic))
  
  topic_df <- bind_rows(
    select(all_topic, all_of(common_cols)),
    select(state_topic, all_of(common_cols))
  )
  

topic_df %>%
    mutate(
      # Normal logic
      ceiling_effect = if_else(state_lwr >= 93 & abs(state_upr - state_lwr) <= 7, "yes", "no"),
      is_diff_raw = case_when(
        ci_lower > state_upr              ~ "diff",
        ci_upper < state_lwr              ~ "diff",
        ci_lower > state_upr - buffer     ~ "near diff",
        ci_upper < state_lwr + buffer     ~ "near diff",
        TRUE                              ~ "not diff"
      ),
      
      # filter out ceiling effect
      is_diff = case_when(
        ceiling_effect == "yes" & is_diff_raw != "diff" ~ "not diff",
        ci_lower > 99 ~ "not diff",
        TRUE                                    ~ is_diff_raw
      )
    ) %>%
    filter(!group_col %in% exclude_groups)

}