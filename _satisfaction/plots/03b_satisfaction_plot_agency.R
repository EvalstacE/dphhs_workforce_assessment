

p_filter <- satisfaction_agency2 %>% 
  filter(
    diff >= 10,
    !cat_group %in% c("pay & benefits", "job")
  )

q_keep <- size_filter %>% 
  distinct(question) %>% 
  pull(question)


all_agency <- satisfaction_agency %>%
  filter(!cat_group %in% c("pay & benefits", "job"))

all_state <- satisfaction_statewide %>% 
  filter(!cat_group %in% c("pay & benefits", "job"))

state_filter <- all_state %>%
  filter(question %in% q_keep)

##################  
################## 
##################  
################## 
agency_satisfaction_nopay <- 
  
  ggplot() +
  
  geom_point(
    data = all_state, 
    size = 4,
    alpha =  0,
    aes(
      x = prop_agree, 
      y = forcats::fct_rev(question),
    )
  ) +   
  
  geom_line(
    data = all_agency, 
    color = "#d9d9d9",
    alpha = 0.3,
    linewidth = 1,
    aes(
      x = prop_agree, 
      y = forcats::fct_rev(question),
      group = question
    )
  ) +  
  
  
  geom_line(
    data = p_filter, 
    color = "#d9d9d9",
    alpha = 1,
    linewidth = 1,
    aes(
      x = prop_agree, 
      y = forcats::fct_rev(question),
      group = question
    )
  ) +   
  
  
  geom_point(
    data = all_agency, 
    size = 6,
    alpha = 0.3,
    shape = 21,
    aes(
      x = prop_agree, 
      y = forcats::fct_rev(question),
      fill = group_col
    )
  ) +   
  
  geom_point(
    data = all_state, 
    size = 3,
    alpha = 1,
    shape = 24,
    color = "black",
    fill = "#f1f0ea",
    aes(
      x = prop_agree, 
      y = forcats::fct_rev(question),
    )
  ) + 
  
  
  geom_point(
    data = p_filter, 
    size = 8,
    alpha = 1,
    color = "white",
    aes(
      x = prop_agree, 
      y = forcats::fct_rev(question)
    )
  ) +   
  
  
  geom_point(
    data = p_filter, 
    size = 8,
    alpha = 0.9,
    shape = 21,
    aes(
      x = prop_agree, 
      y = forcats::fct_rev(question),
      fill = group_col
    )
  ) +     
  
  #################  
geom_point(
  data = state_filter, 
  size = 4,
  shape = 24,
  color = "black",
  fill = "#f1f0ea",
  aes(
    x = prop_agree, 
    y = forcats::fct_rev(question),
  )
) +   
  #################   

xlim(25, 100)  + 
  scale_fill_manual(values = c("#95c6ea", "#1f3465", "#6da587", "#fcd008")) +   
  theme_classic() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  )










ggsave(
  filename = here("_www/plot_exports/agency_satisfaction_nopay.png"),
  plot = agency_satisfaction_nopay,
  width = 6,
  height = 12,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)




























#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
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