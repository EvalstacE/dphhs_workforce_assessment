

all_state <- satisfaction_statewide %>%
  filter(
    !grouping %in%  c("job_satisfied", "leave_retire"),
    !question %in% c("f_love_job_recat", "s_pay_recat")
  )

all_qs <- unique(all_state$question)

all_state_props <- all_state %>%
  rename("state_prop" = "prop_agree") %>%
  select(question, state_prop)


diff_all <- all_qs %>%
  purrr::map(get_topic_diff) %>%
  purrr::map_dfr(~ .x$df_diff %>%
  dplyr::mutate(topic_question = .x$topic_question)) %>%
  filter(question != "s_treated_fair_diability_recat") %>%
  left_join(all_state_props, by = "question") %>%
  filter(prop_agree < state_prop)


##############


p2 <- satisfaction_all_grps %>%
  filter(
    !grouping %in%  c("job_satisfied", "leave_retire"),
    !question %in% c("f_love_job_recat", "s_pay_recat")
    ) %>%
  mutate(
    grouping = 
      factor(
        grouping, 
        levels =
          c( "statewide", "agency_type", "position_type", "jd_size")
      )
  )

p2_min <- p2 %>%
  group_by(question) %>%
  slice_min(order_by = prop_agree, n = 1) %>% 
  mutate(min_prop = min(prop_agree)) %>%
  ungroup()

p2_max <- p2 %>%
  group_by(question) %>%
  slice_max(order_by = prop_agree, n = 1) %>% 
  mutate(min_prop = min(prop_agree)) %>%
  ungroup()



####################
####################
####################
####################

all_diff_plot <- 
  
ggplot() +  
  
geom_point(
    data = p2_min,
    size = 4,
    color = "#3e5c58",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree
    )
)+  
  
  
geom_point(
    data = p2_max,
    size = 4,
    color = "#3e5c58",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree
    )
)+     
  
  
geom_line(
    data = p2,
    linewidth = 4,
    color = "#3e5c58",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree
    )
)+    
  

geom_point(
    data = p2_min,
    size = 2, 
    alpha = 0.5,
    color = "white",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree
    )
) +  
  
  
geom_point(
    data = p2_max,
    size = 2, 
    alpha = 0.5,
    color = "white",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree
    )
) +    
  
geom_point(
    data = diff_all,
    shape = 21, 
    size = 7, 
    alpha = 1, 
    color = "#3e5c58",
    fill = "#3e5c58",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree, 
    )
  ) +   

geom_point(
    data = diff_all,
    shape = 21, 
    size = 6, 
    alpha = 1, 
    color = "#3e5c58",
    fill = "#fcd008",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree, 
    )
) + 
  
geom_point(
    data = all_state,
    alpha = 1,
    shape = 24,
    size = 5,
    color = "#f1f0ea",
    fill = "black",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree
    )
)+    
  
geom_point(
    data = all_state,
    alpha = 1,
    shape = 24,
    size = 3.5,
    color = "black",
    fill = "white",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree
    )
)+  
  
scale_fill_manual(values = c("#f1f0ea", "#fcd008", "#95c6ea")) +
  
xlim(25,100)+
theme_classic() +
theme(
  legend.position = "none",
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background  = element_rect(fill = "transparent", colour = NA),
  axis.text.y = element_blank()
)


####################
####################
####################
ggsave(
  filename = here("_www/plot_exports/all_diff_plot.png"),
  plot = all_diff_plot,
  width = 4,
  height = 9,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)


####################
####################
####################
####################
quantile(satisfaction_statewide$prop_agree)

statewide_boxplot <- 
ggplot() + 
  geom_boxplot(
    data = satisfaction_statewide, 
    aes(
      y = prop_agree
    )
  ) + 
  ylim(25, 100) + 
  theme_classic() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA),
  )
  
######

ggsave(
  filename = here("_www/plot_exports/statewide_boxplot.png"),
  plot = statewide_boxplot,
  width = 2,
  height = 10,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)
