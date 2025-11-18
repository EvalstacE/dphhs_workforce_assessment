

  
s_p1 <- satisfaction_statewide

s_p1_min <- satisfaction_statewide %>%
  group_by(cat_group) %>%
  slice_min(prop_agree, n = 1) %>%
  ungroup() %>%
  arrange(prop_agree) %>%
  slice_head(n=2)

s_p1_max <- satisfaction_statewide %>%
  group_by(cat_group) %>%
  slice_max(prop_agree, n = 1) %>%
  ungroup() %>%
  arrange(desc(prop_agree)) %>%
  slice_head(n=2)


s_p2 <- satisfaction_all_grps %>%
  filter(grouping != "job_satisfied") %>%
  group_by(question) %>%
  slice_min(order_by = prop_agree, n = 2) %>% 
  mutate(min_prop = min(prop_agree)) %>%
  ungroup()


s_p3 <- satisfaction_all_grps %>%
  filter(
    grouping != "job_satisfied",
    cat_group != "job"
  ) %>%
  group_by(cat_group) %>%
  slice_min(order_by = prop_agree, n = 3) %>% 
  mutate(min_prop = min(prop_agree)) %>%
  ungroup()



ggplot() + 
  
  geom_line(
    data = s_p1,
    alpha = 0.5,
    color = "#b4b4b4",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree
    )
  )+  
  
  geom_point(
    data = s_p1,
    size = 6, 
    alpha = 0.5,
    color = "black",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree
    )
  ) + 
  
  geom_point(
    data = s_p1,
    size = 5, 
    alpha = 0.8,
    color = "#f1f0ea",
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree
    )
  ) +  
  
  geom_point(
    data = s_p2,
    shape = 21, 
    size = 4, 
    alpha = 0.9, 
    color = "black",
    position = position_jitter(width = .5, height = 0),
    aes(
      y = forcats::fct_rev(question),
      x = prop_agree, 
      fill = grouping
    )
  ) +  
  
  scale_fill_manual(values = c("#d10a0a", "#fd8b45", "#ffd100",
                               "#52b57f", "#8c52ff", "#19bdd4", "blue")) +
  
xlim(0,100)+
theme_classic() +
theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
)





ggplot() + 
  
  geom_line(
    data = s_p1,
    alpha = 0.5,
    color = "#b4b4b4",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  )+  
  
  geom_point(
    data = s_p1,
    size = 6, 
    alpha = 0.5,
    color = "black",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  ) + 
  
  geom_point(
    data = s_p1,
    size = 5, 
    alpha = 0.8,
    color = "#f1f0ea",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  ) +  
  
  geom_point(
    data = satisfaction_size %>% filter(cat_group != "job"),
    shape = 21, 
    size = 4, 
    alpha = 0.9, 
    color = "black",
    position = position_jitter(width = .5, height = 0),
    aes(
      y = cat_group, 
      x = prop_agree,
      fill = group_col
    )
  ) +  
  
  xlim(25,100)+
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  )






ggplot() + 
  
  geom_line(
    data = s_p1,
    alpha = 0.5,
    color = "#b4b4b4",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  )+  
  
  geom_point(
    data = s_p1,
    size = 6, 
    alpha = 0.5,
    color = "black",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  ) + 
  
  geom_point(
    data = s_p1,
    size = 5, 
    alpha = 0.8,
    color = "#f1f0ea",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  ) +  
  
  geom_point(
    data = satisfaction_position %>% filter(cat_group != "job"),
    shape = 21, 
    size = 4, 
    alpha = 0.9, 
    color = "black",
    position = position_jitter(width = .5, height = 0),
    aes(
      y = cat_group, 
      x = prop_agree,
      fill = group_col
    )
  ) +  
  
  xlim(25,100)+
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  )















ggplot() + 
  
  geom_line(
    data = s_p1,
    alpha = 0.5,
    color = "#b4b4b4",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  )+  
  
  geom_point(
    data = s_p1,
    size = 6, 
    alpha = 0.5,
    color = "black",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  ) + 
  
  geom_point(
    data = s_p1,
    size = 5, 
    alpha = 0.8,
    color = "#f1f0ea",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  ) +  
  
  geom_point(
    data = satisfaction_agency %>% filter(cat_group != "job"),
    shape = 21, 
    size = 4, 
    alpha = 0.9, 
    color = "black",
    fill = "#d10a0a",
    position = position_jitter(width = .5, height = 0),
    aes(
      y = cat_group, 
      x = prop_agree
    )
  ) +  
  
  xlim(25,100)+
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  )














ggplot() + 
  
geom_line(
    data = s_p1,
    alpha = 0.5,
    color = "#b4b4b4",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  )+  
  
  geom_point(
    data = s_p1,
    size = 6, 
    alpha = 1,
    color = "#b4b4b4",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  ) + 
  
  geom_point(
    data = s_p1,
    size = 5, 
    alpha = 0.8,
    color = "#d9d9d9",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  ) +  
  
  geom_point(
    data = satisfaction_position,
    shape = 21, 
    size = 4, 
    alpha = 0.9, 
    color = "black",
    position = position_jitter(width = .5, height = 0),
    aes(
      y = cat_group, 
      x = prop_agree, 
      fill = group_col
    )
  ) +  
  
  scale_fill_manual(values = c("#d10a0a", "#fd8b45", "#ffd100",
                               "#52b57f", "#8c52ff", "#19bdd4", "blue")) + 
  
  xlim(25,100)+
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  )







ggplot(satisfaction_position %>% filter(cat_group != "pay & benefits"), aes(x = fct_rev(cat_group), y = prop_agree, fill = cat_group)) +
  facet_wrap(.~group_col) + 
  
  geom_line(color = "grey")+
  geom_point(size = 5, alpha = 0.9, color = "black") +
  geom_point(shape = 21, size = 4, alpha = 0.9, color = "black") +
  scale_fill_manual(values = c("black", "#d10a0a", "#fd8b45", "#ffd100",
                               "#52b57f", "#8c52ff", "#19bdd4", "blue")) +
  ylim(55,100)+
  theme_classic() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  ) 


#########################################
#########################################
#########################################
#########################################


diff_state_pay <- 
  diff_vs_state(
    topic_question =  "s_pay_recat",
    all_df   = satisfaction_all_grps,
    state_df = satisfaction_statewide,
    buffer = 3,
    exclude_groups = c("not leaving or retiring", "satisfied with job")
  )


diff_input <- 
  diff_vs_state(
    topic_question =  "f_happy_input_recat",
    all_df   = satisfaction_all_grps,
    state_df = satisfaction_statewide,
    buffer = 3
  )

###########################
###########################

state_df <- satisfaction_statewide %>%
  filter(
    question == 
      "s_pay_recat"
  )

state_lwr <- state_df$ci_lower
state_upr <- state_df$ci_upper


df_subset <- 
  diff_vs_state(
    topic_question =  "s_pay_recat",
    all_df   = satisfaction_all_grps,
    state_df = satisfaction_statewide,
    buffer = 3,
    exclude_groups = "statewide_results"
  ) 

df_diff <- df_subset %>%
  filter(
    is_diff %in% c("diff", "near diff")
  )
  

####################
pay_satisfaction_p1 <- 
  
  
  
  
ggplot() + 
annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = state_lwr, ymax = state_upr,
    alpha = 0.15, fill = "grey80"
) +

geom_errorbar(
    data = df_subset, 
    width = 0.5,
    alpha = 0.5,
    color = "#b4b4b4",
    aes(
      x = group_col, 
      y = prop_agree, 
      ymin = ci_lower, 
      ymax = ci_upper
    )
) + 
  
geom_point(
    data = df_subset, 
    alpha = 0.5,
    aes(
      x = group_col, 
      y = prop_agree, 
      color = group_col
    )
) + 
  
geom_errorbar(
    data = df_diff, 
    width = 0.5,
    color = "#b4b4b4",
    aes(
      x = group_col, 
      y = prop_agree, 
      ymin = ci_lower, 
      ymax = ci_upper
    )
  ) +
  
geom_point(
    data = df_diff, 
    aes(
      x = group_col, 
      y = prop_agree, 
      color = group_col
    )
) + 
  
facet_wrap(~ grouping, scales = "free_x", nrow = 1)  + 
  
theme_classic() +
theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.spacing = unit(0, "lines") 
)

  









ggsave(
  filename = here("_www/plot_exports/pay_satisfaction_p1.png"),
  plot = pay_satisfaction_p1,
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)

