make_prop_plot <- function(df, filter_col, filter_value, x_var) {
  
  filter_col <- enquo(filter_col)
  x_var      <- enquo(x_var)
  
  df %>%
    filter( {{ filter_col }} == filter_value ) %>%
    ggplot(aes(x = !!x_var, y = prop)) +
    geom_point(size = 3, color = "#002e6d") +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width  = 0.1,
      color  = "#002e6d"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1)
    ) +
    theme_classic() +
    theme(
      axis.title        = element_blank(),
      axis.text         = element_text(color = "#002e6d"),
      axis.line         = element_line(color = "#002e6d"),
      axis.ticks        = element_line(color = "#002e6d"),
      panel.background  = element_rect(fill = "transparent", color = NA),
      plot.background   = element_rect(fill = "transparent", color = NA)
    )
}