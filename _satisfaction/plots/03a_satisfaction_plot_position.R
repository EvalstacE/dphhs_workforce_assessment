
###################
###################


p_filter <- satisfaction_position2 %>% 
  filter(
      (group_col %in% c("Non-supervisor", "Supervisor")   & diff_sup_nonsup  >= 10) |
      (group_col %in% c("Executive",      "Supervisor")   & diff_exec_sup    >= 10) |
      (group_col %in% c("Executive",      "Non-supervisor") & diff_nonsup_exec >= 10),
    !cat_group %in% c("pay & benefits", "job")
  )

q_keep <- p_filter %>% 
  distinct(question) %>% 
  pull(question)


all_pos <- satisfaction_position %>%
  filter(!cat_group %in% c("pay & benefits", "job"))

all_state <- satisfaction_statewide %>% 
  filter(!cat_group %in% c("pay & benefits", "job"))

state_filter <- all_state %>%
  filter(question %in% q_keep)


 
##################  
################## 
##################  
################## 
position_satisfaction_nopay <- 

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
    data = all_pos, 
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
    data = all_pos, 
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
scale_fill_manual(values = c("#95c6ea", "#1f3465", "#6da587")) +   
theme_classic() +
theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
)










ggsave(
  filename = here("_www/plot_exports/position_satisfaction_nopay.png"),
  plot = position_satisfaction_nopay,
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
s_pos_p1 <- satisfaction_position2 %>% 
  filter(question == "f_happy_input_recat") %>%
  mutate(
    group_col = factor(group_col, levels = c("Executive", "Supervisor", "Non-supervisor"))
  )


position_input_p <- 
ggplot() + 
geom_errorbar(
    data = s_pos_p1,
    width = 0.15,
    alpha = 0.5,
    color = "grey",
    aes(
      x = group_col, 
      y = prop_agree, 
      ymin = ci_lower,
      max = ci_upper
    )
)  +  
geom_point(data = s_pos_p1,
             size = 6, 
             alpha = 1, 
             color = "black",
             aes(
               x = group_col,
               y = prop_agree,
             )
 ) +
    
ylim(40,100) + 
theme_classic() + 
theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),  
    plot.background  = element_rect(fill = "transparent", colour = NA)  
)




ggsave(
  filename = here("_www/plot_exports/position_input_p.png"),
  plot = position_input_p,
  width = 6,
  height = 6,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)

  
