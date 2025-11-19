#########################################
#########################################
#########################################
#########################################


psych_list <- get_topic_diff("f_psych_safe_recat")


df_subset  <- psych_list$df_subset
df_diff    <- psych_list$df_diff
state_lwr  <- psych_list$state_lwr
state_upr  <- psych_list$state_upr


psych_plot <- create_stat_diff_plot(df_subset, df_diff, state_lwr, state_upr)
psych_plot

####################
####################
####################
ggsave(
  filename = here("_www/plot_exports/safe_satisfaction.png"),
  plot = safe_satisfaction,
  width = 6,
  height = 7,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)
