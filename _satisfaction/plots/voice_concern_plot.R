#########################################
#########################################
#########################################
#########################################


voice_list <- get_topic_diff("f_voice_concerns_recat")

df_subset  <- voice_list$df_subset
df_diff    <- voice_list$df_diff
state_lwr  <- voice_list$state_lwr
state_upr  <- voice_list$state_upr


voice_plot <- create_stat_diff_plot(df_subset, df_diff, state_lwr, state_upr)
voice_plot


####################
####################
####################
ggsave(
  filename = here("_www/plot_exports/voice_satisfaction.png"),
  plot = voice_satisfaction,
  width = 6,
  height = 7,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)
