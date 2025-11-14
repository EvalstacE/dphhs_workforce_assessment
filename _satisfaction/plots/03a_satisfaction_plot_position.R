
s_position_p1 <- satisfaction_position %>% 
  filter(cat_group %in% c("pay & benefits")) %>%
  mutate(
    sup_status = 
      factor(
        sup_status, 
        levels = c("Non-supervisor", "Supervisor", "Executive"),
        ordered = TRUE
        )
  )

#s_position_p1_filter <- satisfaction_agency %>% 
  #filter(
    #cat_group %in% c("pay & benefits"),
    #diff >= 10)


ggplot() + 
  geom_line(data = s_position_p1,
            aes(
              y = question,
              x = prop_agree,
              group = question
            ),
            linewidth = .6, 
            alpha = 0.6,
            color = "grey"
  ) +
  
  geom_point(
    data = s_position_p1,
             size = 6,
             alpha = 1,
             aes(
               y = question,
               x = prop_agree,
               group = question,
               color = sup_status
             ),
    position = position_jitter(width = 1.5, height = 0)
             
  ) +
  
scale_x_continuous(limits = c(55, 100)) +
scale_color_manual(values = c("#95c6ea", "#1f3465", "#fcd008"))+ 
  labs(
    x = NULL,
    y = "% agree or strongly agree",
    color = "Domain"
  ) +
  
  theme_classic() + 
  theme(
    legend.position = "none"
  )
