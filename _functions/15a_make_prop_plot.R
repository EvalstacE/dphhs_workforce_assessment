make_prop_plot <- function(df, filter_col, filter_value, x_var) {
  
  filter_col <- enquo(filter_col)
  x_var      <- enquo(x_var)
  
  df %>%
    filter( {{ filter_col }} == filter_value ) %>%
    ggplot(aes(x = !!x_var, y = prop)) +
    geom_point() +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.1
    ) +
    scale_y_continuous(
      name   = "Percent",
      labels = scales::percent_format(scale = 1)
    ) +
    ylim(0, 100) +
    theme_classic()
}