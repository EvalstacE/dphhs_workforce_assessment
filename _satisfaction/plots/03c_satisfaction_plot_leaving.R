




s_leave_p1 <- satisfaction_leave %>% 
  filter(
    !cat_group %in% c("job", "org"),
    !question %in% c("s_belonging_work_unit_recat", "s_work_unit_recat")
  ) %>%
  group_by(leave_or_retire) %>%
  arrange(desc(diff_leaving)) %>%
  slice_head(n=5)




satisfaction_leave_p1 <- 
  
ggplot() + 
geom_col(
    data = s_leave_p1,
    aes(
      x = question,
      y = prop_agree,
      fill = leave_or_retire
    ),
    position = "dodge"
) + 
  
facet_wrap(~question, nrow = 1) + 
  
theme_classic() +
theme(
    panel.spacing = unit(0, "pt"),   
    strip.background = element_blank(),  
    strip.placement = "inside",
    strip.text = element_blank(),
    axis.text.x = element_blank(), 
    legend.position = "none"
) 
  
  
  
  geom_line(data = s_leave_p1_filter,
            aes(
              x = leave_or_retire,
              y = prop_agree,
              group = question,
              color = cat_group
            ),
            linewidth = 1, 
            alpha = 1,
  ) +
  
  geom_point(data = s_leave_p1_filter,
             size = 6,
             alpha = 1,
             aes(
               x = leave_or_retire,
               y = prop_agree,
               group = question,
               color = cat_group
             )
             
  ) +
  
  scale_y_continuous(limits = c(25, 100)) +

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