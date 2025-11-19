#########################################
#########################################
#########################################
#########################################


job_list <- get_topic_diff("s_job_recat")


df_subset  <- job_list$df_subset
df_diff    <- job_list$df_diff
state_lwr  <- job_list$state_lwr
state_upr  <- job_list$state_upr


job_plot <- create_stat_diff_plot(df_subset, df_diff, state_lwr, state_upr)
job_plot


####################
####################
####################
ggsave(
  filename = here("_www/plot_exports/job_satisfaction.png"),
  plot = job_satisfaction,
  width = 6,
  height = 7,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)
