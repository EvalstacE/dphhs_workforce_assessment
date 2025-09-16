
create_priority_plot <- function(plot_df) {
  
  y_thr <-  c(20, 40, 60)
  y_lims <- c(0, 65)
  
  x_thr <-  c(70)
  x_lims <- c(50, 100)
  
p <- 
    ggplot(plot_df, aes(x = imp_prop, y = skill_prop, fill = domain)) +
    scale_fill_viridis_d() +
    geom_hline(yintercept = y_thr, linetype = "dashed") +
    geom_vline(xintercept = x_thr, linetype = "dashed") +
    geom_point(size = 4, alpha = 0.7, shape = 21) +
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
  
  return(p)
  
}


