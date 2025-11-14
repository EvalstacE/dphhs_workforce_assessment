s_agency_p1 <- satisfaction_agency %>% 
  filter(cat_group %in% c("treated fairly", "belonging"))

s_agency_p1_filter <- satisfaction_agency %>% 
  filter(
    cat_group %in% c("treated fairly", "belonging"),
    diff >= 10
  )


satisfaction_agency_p1 <- 
ggplot() + 
  geom_line(data = s_agency_p1,
            aes(
              x = agency,
              y = prop_agree,
              group = question,
              color = cat_group
            ),
            linewidth = .6, 
            alpha = 0.2,
  ) +
  
  geom_point(data = s_agency_p1,
             size = 6,
             alpha = 0.2,
             aes(
               x = agency,
               y = prop_agree,
               group = question,
               color = cat_group
             )
             
  ) +
  
  geom_line(data = s_agency_p1_filter,
            aes(
              x = agency,
              y = prop_agree,
              group = question,
              color = cat_group
            ),
            linewidth = 1, 
            alpha = 1,
  ) +
  
  geom_point(data = s_agency_p1_filter,
             size = 6,
             alpha = 1,
             aes(
               x = agency,
               y = prop_agree,
               group = question,
               color = cat_group
             )
             
  ) +
  
  scale_y_continuous(limits = c(50, 100)) +
  scale_color_manual(values = c("#95c6ea", "#1f3465"))+ 
  labs(
    x = NULL,
    y = "% agree or strongly agree",
    color = "Domain"
  ) +
  
  theme_classic() + 
  theme(
      legend.position = "none",
      panel.background = element_rect(fill = "transparent", colour = NA),  
      plot.background  = element_rect(fill = "transparent", colour = NA)  
  )




ggsave(
  filename = here("_www/plot_exports/satisfaction_agency_p1.png"),
  plot = satisfaction_agency_p1,
  width = 6,
  height = 6,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)