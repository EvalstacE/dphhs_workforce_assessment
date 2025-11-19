

create_stat_diff_plot <- function(df_subset, df_diff, state_lwr, state_upr) {
 
ggplot() + 
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = state_lwr, ymax = state_upr,
    alpha = 0.15, fill = "grey80"
  ) +
  
geom_errorbar(
    data = df_subset, 
    width = 0.5,
    alpha = 0.4,
    color = "#b4b4b4",
    aes(
      x = group_col, 
      y = prop_agree, 
      ymin = ci_lower, 
      ymax = ci_upper
    )
) + 
  
geom_point(
    data = df_subset, 
    alpha = 0.5,
    size = 2,
    shape = 21,
    color = "black",
    aes(
      x = group_col, 
      y = prop_agree, 
      fill = grouping
    )
) + 
  
geom_errorbar(
    data = df_diff, 
    width = 0.5,
    alpha = 0.8,
    linewidth = .7,
    color = "#b4b4b4",
    aes(
      x = group_col, 
      y = prop_agree, 
      ymin = ci_lower, 
      ymax = ci_upper,
    )
) +
  
geom_point(
    data = df_diff, 
    size = 3.5,
    shape = 21,
    color = "black",
    aes(
      x = group_col, 
      y = prop_agree, 
      fill = grouping
    )
) + 
  
facet_wrap(~ grouping, scales = "free_x", nrow = 1)  + 
scale_fill_manual(values = c("#95c6ea", "#1f3465", "#6da587", "#fcd008","#a8b09d")) +
  
ylim(25, 100) + 
  
theme_classic() +
theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.spacing = unit(0, "lines") 
)
  
  
  
  
}

