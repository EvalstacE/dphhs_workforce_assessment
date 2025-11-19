#########################################
#########################################
#########################################
#########################################


manage_list <- get_topic_diff("f_manage_demands_recat")


df_subset  <- manage_list$df_subset
df_diff    <- manage_list$df_diff
state_lwr  <- manage_list$state_lwr
state_upr  <- manage_list$state_upr


manage_plot <- create_stat_diff_plot(df_subset, df_diff, state_lwr, state_upr)
manage_plot


####################
####################
####################
ggsave(
  filename = here("_www/plot_exports/balance_satisfaction.png"),
  plot = balance_satisfaction,
  width = 6,
  height = 7,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)
