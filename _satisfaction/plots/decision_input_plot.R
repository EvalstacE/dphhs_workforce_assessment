#########################################
#########################################
#########################################
#########################################


input_list <- get_topic_diff("f_happy_input_recat")


df_subset  <- input_list$df_subset
df_diff    <- input_list$df_diff
state_lwr  <- input_list$state_lwr
state_upr  <- input_list$state_upr


input_plot <- create_stat_diff_plot(df_subset, df_diff, state_lwr, state_upr)
input_plot


####################
####################
####################
ggsave(
  filename = here("_www/plot_exports/input_satisfaction.png"),
  plot = input_satisfaction,
  width = 6,
  height = 7,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)
