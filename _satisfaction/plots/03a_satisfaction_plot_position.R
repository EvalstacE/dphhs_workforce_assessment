
###################
###################


pos_filter <- satisfaction_position2 %>% 
  filter(
    (group_col %in% c("Non-supervisor", "Supervisor")   & diff_sup_nonsup  >= 10) |
      (group_col %in% c("Executive",      "Supervisor")   & diff_exec_sup    >= 10) |
      (group_col %in% c("Executive",      "Non-supervisor") & diff_nonsup_exec >= 10)
  )

q_keep <- pos_filter %>% 
  distinct(question) %>% 
  pull(question)


state_filter <- satisfaction_statewide %>%
  filter(question %in% q_keep)


 
##################  
################## 
##################  
################## 
position_satisfaction_p1 <- 

ggplot() +
geom_line(
    data = pos_filter, 
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
    data = pos_filter, 
    size = 8,
    alpha = 1,
    color = "white",
    aes(
      x = prop_agree, 
      y = forcats::fct_rev(question)
    )
) +   
  
  
geom_point(
    data = pos_filter, 
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
scale_fill_manual(values = c("#95c6ea", "#1f3465", "#6da587")) +   
theme_classic() +
theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
)




% culture of quality in phase of 4-6
% of culture of quality 






ggsave(
  filename = here("_www/plot_exports/position_satisfaction_p1.png"),
  plot = position_satisfaction_p1,
  width = 6,
  height = 8,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)

  
