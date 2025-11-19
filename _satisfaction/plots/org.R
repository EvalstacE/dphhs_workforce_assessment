#########################################
#########################################
#########################################
#########################################


org_list <- get_topic_diff("s_org_recat")


df_subset  <- org_list$df_subset
df_diff    <- org_list$df_diff
state_lwr  <- org_list$state_lwr
state_upr  <- org_list$state_upr


org_plot <- create_stat_diff_plot(df_subset, df_diff, state_lwr, state_upr)
org_plot


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
