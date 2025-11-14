


s_leave_p1 <- satisfaction_leave %>% 
  filter(cat_group %in% c("treated fairly", "belonging", "job", 
                          "pay & benefits", "team", "org")) %>% 
  filter(diff_leaving >= 15) %>%
  mutate(
    leave_or_retire = 
      factor(leave_or_retire, 
             levels = c("not leaving or retiring", "leaving not retiring"), 
             ordered = TRUE
             )
    )






satisfaction_leave_p1 <- 
  
ggplot() + 

  
  
  
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