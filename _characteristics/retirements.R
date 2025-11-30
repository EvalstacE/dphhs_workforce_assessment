

retire_size_stats <- summarise_group_props(data_cleaned, size, retire_recat)

retire_sup_stats <- summarise_group_props(data_cleaned, sup_status, retire_recat)

retire_region_stats <- summarise_group_props(data_cleaned, region, retire_recat)


make_prop_plot(
  retire_region_stats,
  filter_col   = retire_recat,
  filter_value = "before 2030",
  x_var        = region
)


retire_leave_sup <- data_cleaned %>%
  select(region, size, sup_status, retire_recat, leave_or_retire) %>%
  mutate(
    leave_retire2 = if_else(
      leave_or_retire %in% c("leaving not retiring", "retiring"), 
             "leaving OR retiring", "staying")
         )
  

retire_leave_sup_stats <- 
  summarise_group_props(retire_leave_sup, sup_status, leave_retire2)


retire_leave_size_stats <- 
  summarise_group_props(retire_leave_sup, size, leave_retire2)


retire_leave_region_stats <- 
  summarise_group_props(retire_leave_sup, region, leave_retire2)





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

