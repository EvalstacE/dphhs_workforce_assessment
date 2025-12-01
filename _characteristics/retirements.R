

retire_size_stats <- summarise_group_props(data_cleaned, size, retire_recat)

retire_sup_stats <- summarise_group_props(data_cleaned, sup_status, retire_recat)

retire_region_stats <- summarise_group_props(data_cleaned, region, retire_recat)


make_prop_plot(
  retire_region_stats,
  filter_col   = retire_recat,
  filter_value = "before 2030",
  x_var        = region
)


retire_leave_df <- data_cleaned %>%
  select(region, size, sup_status, retire_recat, leave_or_retire, leave_retire2, max_yrs_cat) 

leave_retire_all <- retire_leave_df %>%
  filter(leave_retire2 == "leaving OR retiring")
  

retire_leave_sup_stats <- 
  summarise_group_props(retire_leave_df, sup_status, leave_retire2,
                        rename_cols = TRUE)


retire_leave_size_stats <- 
  summarise_group_props(retire_leave_df, size, leave_retire2,
                        rename_cols = TRUE)


retire_leave_region_stats <- 
  summarise_group_props(retire_leave_df, region, leave_retire2, 
                        rename_cols = TRUE)


retire_leave_experience <-   
  summarise_group_props(retire_leave_df, max_yrs_cat, leave_retire2, rename_cols = TRUE) %>%
  filter(category == "leaving OR retiring")
  


retire_leave_group_stats <- 
  rbind(retire_leave_region_stats, retire_leave_size_stats, retire_leave_sup_stats) %>%
  filter(category == "leaving OR retiring")


retire_leave_sum <- retire_leave_group_stats %>%
  group_by(group) %>%
  summarise(
    avg_prop = mean(prop), 
    .groups = "drop"
  )







p_region <- 
make_prop_plot(
  retire_leave_region_stats,
  filter_col   = leave_retire2,
  filter_value = "leaving OR retiring",
  x_var        = region
) + 
  ylim(0,75)


p_size <- 
  make_prop_plot(
    retire_leave_size_stats,
    filter_col   = leave_retire2,
    filter_value = "leaving OR retiring",
    x_var        = size
  ) + 
  ylim(0,75)


p_sup <- 
  make_prop_plot(
    retire_leave_sup_stats,
    filter_col   = leave_retire2,
    filter_value = "leaving OR retiring",
    x_var        = sup_status
  ) + 
  ylim(0,75) 





plot_spacer <- function() cowplot::ggdraw()

leave_retire_plot <- plot_grid(
  p_region,
  plot_spacer(),
  p_size,
  plot_spacer(),
  p_sup,
  nrow = 1,
  align = "h",
  axis = "l",
  rel_widths = c(1, 0.15, 1, 0.15, 1)
)

leave_retire_plot

ggsave(
  filename = here("_www/plot_exports/leave_retire_plot.png"),
  plot = leave_retire_plot,
  width = 16,
  height = 5,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)

