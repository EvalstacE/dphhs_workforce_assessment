
###################
###################


p_filter <- satisfaction_size2 %>% 
  filter(
      (group_col %in% c("Frontier", "Small")  & diff_frontier_small  >= 10) |
      (group_col %in% c("Frontier", "Medium") & diff_frontier_medium >= 10) |
      (group_col %in% c("Frontier", "Large")  & diff_frontier_large  >= 10) |
      (group_col %in% c("Small", "Medium")    & diff_small_medium    >= 10) |
      (group_col %in% c("Small", "Large")     & diff_small_large     >= 10) |
      (group_col %in% c("Medium", "Large")    & diff_medium_large   >= 10),
      !cat_group %in% c("pay & benefits", "job")
  )


q_keep <- size_filter %>% 
  distinct(question) %>% 
  pull(question)


all_size <- satisfaction_size %>%
  filter(!cat_group %in% c("pay & benefits", "job"))

all_state <- satisfaction_statewide %>% 
  filter(!cat_group %in% c("pay & benefits", "job"))

state_filter <- all_state %>%
  filter(question %in% q_keep)

##################  
################## 
##################  
################## 
size_satisfaction_nopay <- 
  
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
    data = all_size, 
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
    data = all_size, 
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
  filename = here("_www/plot_exports/size_satisfaction_nopay.png"),
  plot = size_satisfaction_nopay,
  width = 6,
  height = 12,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)
