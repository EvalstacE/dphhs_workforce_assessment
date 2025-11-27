make_hd_map <- function(cat, hd_dorl, lbl_df, mt_sf, pal, lbl_col,
                        alpha_focus = 1, alpha_other = 0.7) {
  

  f_dorl <- hd_dorl %>%
    mutate(
      alpha_val = if_else(hd_cat == cat, alpha_focus, alpha_other)
    )
  

  f_lbl <- lbl_df %>% 
    filter(hd_cat == cat)
  

  ggplot() +
    geom_sf(data = mt_sf, fill = NA, color = "#688ba4") +
    
    geom_sf(
      data = f_dorl,
      aes(fill = hd_cat, alpha = alpha_val)
    ) +
    scale_alpha_identity() +
    scale_fill_manual(values = pal, guide = "none") +
    
    geom_label_repel(
      data = f_lbl,
      aes(x = x, y = y, label = hd_lbl, fill = hd_cat),
      color = lbl_col,
      size = 6,
      box.padding = 0.3,
      point.padding = 0.2,
      min.segment.length = 0,
      seed = 123,
      segment.color = "#002e6d"
    ) +
    
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
}
