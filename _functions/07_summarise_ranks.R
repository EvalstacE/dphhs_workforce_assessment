# counts & percents by importance ------------------------------------------------
calc_imp_prop <- function(df) {
  n_resp <- dplyr::n_distinct(df$respondent_id)
  
  df %>%
    dplyr::count(
      domain, supervisor_status, section, question_id, topic, imp_recat,
      name = "imp_n"
    ) %>%
    dplyr::mutate(
      imp_prop = 100 * imp_n / n_resp
    ) %>%
    dplyr::arrange(section, question_id)
}

# skill counts & percents among "Moderately/Very Important" ----------------------
calc_skill_prop <- function(df) {
  
  # denominators for skill %: total "Moderately/Very Important" per item
  imp_denoms <- calc_imp_prop(df) %>%
    dplyr::filter(imp_recat == "Moderately/Very Important") %>%
    dplyr::select(
      domain, supervisor_status, section, question_id, topic,
      imp_n, imp_prop
    )
  
  # numerators: skill counts within the "Moderately/Very Important" subset
  skill_counts <- df %>%
    dplyr::filter(imp_recat == "Moderately/Very Important") %>%
    dplyr::count(
      domain, supervisor_status, section, question_id, topic, skill_recat,
      name = "skill_n"
    )
  
  skill_counts %>%
    dplyr::left_join(
      imp_denoms,
      by = c("domain","supervisor_status","section","question_id","topic")
    ) %>%
    dplyr::mutate(
      imp_recat  = "Moderately/Very Important",
      skill_prop = 100 * skill_n / imp_n
    ) %>%
    dplyr::relocate(imp_recat, .after = topic) %>%
    dplyr::arrange(section, question_id) %>%
    
    mutate(priority_score = imp_prop * skill_prop) 
}


  

