



all_sum <- read.csv(file = here("_www/df_exports/all_score_summary.csv"))

#nonsup_plot_df <- all_sum %>% filter(supervisor_status == "Non-supervisor")
#sup_plot_df <- all_sum %>% filter(supervisor_status == "Supervisor")
#exec_plot_df <- all_sum %>% filter(supervisor_status == "Executive")


#write.csv(nonsup_plot_df, "_www/df_exports/nonsup_summary.csv", row.names = FALSE)
#write.csv(sup_plot_df, "_www/df_exports/sup_summary.csv", row.names = FALSE)
#write.csv(exec_plot_df, "_www/df_exports/exec_summary.csv", row.names = FALSE)


nonsup_plot_df <- read.csv(file = here("_www/df_exports/nonsup_summary.csv"))
sup_plot_df <- read.csv(file = here("_www/df_exports/sup_summary.csv"))
exec_plot_df <- read.csv(file = here("_www/df_exports/exec_summary.csv"))



###########################
### plots ###
###########################


nonsup_plot_void <- create_priority_plot_void(nonsup_plot_df)
sup_plot_void <- create_priority_plot_void(sup_plot_df)
exec_plot_void <- create_priority_plot_void(exec_plot_df)


nonsup_plot <- create_priority_plot(nonsup_plot_df)
sup_plot <- create_priority_plot(sup_plot_df)
exec_plot <- create_priority_plot(exec_plot_df)


nonsup_plot
sup_plot
exec_plot



###########################
###########################


ggsave(
  filename = here("_www/plot_exports/exec_plot.png"),
  plot = exec_plot,
  width = 4,
  height = 8,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)

ggsave(
  filename = here("_www/plot_exports/nonsup_plot.png"),
  plot = nonsup_plot,
  width = 4,
  height = 8,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)

ggsave(
  filename = here("_www/plot_exports/sup_plot.png"),
  plot = sup_plot,
  width = 4,
  height = 8,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)



ggsave(
  filename = here("_www/plot_exports/exec_plot_v.png"),
  plot = exec_plot_void,
  width = 4,
  height = 8,
  units = "in",
  dpi = 900,
  bg = "transparent"

)

ggsave(
  filename = here("_www/plot_exports/nonsup_plot_V.png"),
  plot = nonsup_plot_void,
  width = 4,
  height = 8,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)

ggsave(
  filename = here("_www/plot_exports/sup_plot_v.png"),
  plot = sup_plot_void,
  width = 4,
  height = 8,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)

