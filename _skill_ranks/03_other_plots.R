
high_priority <- all_sum %>% filter(priority_cat == "Highest Priority")

second_priority <- all_sum %>% filter(priority_cat == "Second Priority")



top_priority_plot <- 
ggplot(high_priority, aes(x = imp_prop, y = skill_prop)) +

  
  geom_point(size = 16, 
             color = "black",
             position = position_jitter(width = 0.01, height = 0.01) 
  ) +
  geom_point(size = 15, 
             alpha = 0.9, 
             shape = 21, 
             fill = "#a12729"
  ) +
  
  coord_equal() +
  scale_x_continuous(
    limits = x_lims,
    labels = scales::label_percent(scale = 1)
  ) +
  scale_y_continuous(
    limits = y_lims,
    labels = scales::label_percent(scale = 1)
  ) +
  theme_void()


ggsave(
  filename = here("_www/plot_exports/top_priority_plot.png"),
  plot = top_priority_plot,
  width = 8,
  height = 11,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)



ggplot(second_priority, aes(x = imp_prop, y = skill_prop, fill = supervisor_status)) +
  scale_fill_viridis_d() +
  geom_hline(yintercept = y_thr, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = x_thr, linetype = "dashed", alpha = 0.5) +
  geom_point(size = 4, alpha = 0.8, shape = 21) +
  coord_equal() +
  scale_x_continuous(
    limits = x_lims,
    labels = scales::label_percent(scale = 1)
  ) +
  scale_y_continuous(
    limits = y_lims,
    labels = scales::label_percent(scale = 1)
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "% of staff rating important",
    y = "% of staff beginner or unable"
  ) +
  theme_classic()