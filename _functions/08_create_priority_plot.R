
create_priority_plot <- function(plot_df) {
  
  y_thr <-  c(20, 40, 60)
  y_lims <- c(0, 65)
  
  x_thr <-  c(70)
  x_lims <- c(50, 100)
  
p <- 
    ggplot(plot_df, aes(x = imp_prop, y = skill_prop, fill = domain_cat)) +
  
    scale_fill_manual(values = c("#fd8b45", "#8c52ff", "#ffd100", "#52b57f")) +
  
  
    geom_hline(yintercept = y_thr, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = x_thr, linetype = "dashed", alpha = 0.5) +
  
  geom_point(size = 16, 
             color = "black"
          ) +
    geom_point(size = 15, 
               alpha = 0.9, 
               shape = 21
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
    labs(
      title = "",
      subtitle = "",
      x = "% of staff rating important",
      y = "% of staff beginner or unable"
    ) +
    theme_classic() + 
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "transparent", colour = NA),  # panel
      plot.background  = element_rect(fill = "transparent", colour = NA)   # around plot
    )

  return(p)
  
}


