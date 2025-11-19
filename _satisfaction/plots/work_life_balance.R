#########################################
#########################################
#########################################
#########################################


balance_list <- get_topic_diff("f_work_life_balance_recat")


df_subset  <- balance_list$df_subset
df_diff    <- balance_list$df_diff
state_lwr  <- balance_list$state_lwr
state_upr  <- balance_list$state_upr


balance_plot <- create_stat_diff_plot(df_subset, df_diff, state_lwr, state_upr)
balance_plot


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
