

  
s_p1 <- satisfaction_statewide %>%
  arrange(rng_prop) %>%
  mutate(cat_group = fct_reorder(cat_group, min_prop, .desc = TRUE))

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
  group_by(cat_group) %>%
  slice_min(order_by = prop_agree, n = 2) %>% 
  mutate(min_prop = min(prop_agree)) %>%
  ungroup()%>%
  mutate(cat_group = fct_reorder(cat_group, min_prop, .desc = TRUE))


s_p3 <- satisfaction_all_grps %>%
  group_by(cat_group) %>%
  slice_min(order_by = prop_agree, n = 3) %>% 
  mutate(min_prop = min(prop_agree)) %>%
  ungroup()%>%
  mutate(cat_group = fct_reorder(cat_group, min_prop, .desc = TRUE))


ggplot() + 
  
geom_point(
    data = s_p3,
    shape = 21, 
    size = 4, 
    alpha = 0.9, 
    color = "black",
    position = position_jitter(width = .5, height = 0),
    aes(
      y = cat_group, 
      x = prop_agree, 
      fill = grouping
    )
) +  
  
scale_fill_manual(values = c("#d10a0a", "#fd8b45", "#ffd100",
                               "#52b57f", "#8c52ff", "#19bdd4", "blue")) +
  
  
geom_line(
  data = s_p1,
  color = "grey",
  aes(
    y = cat_group, 
    x = prop_agree
  )
)+
  
geom_point(
  data = s_p1,
  size = 4, 
  alpha = 0.4, 
  color = "black",
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
    data = s_p1_min,
    size = 7, 
    color = "#1f3465",
    aes(
      y = cat_group, 
      x = prop_agree, 
    )
  ) +  
  
  geom_point(
    data = s_p1_max,
    size = 8, 
    alpha = 1,
    color = "#1f3465",
    aes(
      y = cat_group, 
      x = prop_agree
    )
  ) + 
  
  geom_point(
    data = s_p1_max,
    size = 7, 
    alpha = 0.8,
    color = "#95c6ea",
    aes(
      y = cat_group, 
      x = prop_agree, 
    )
  ) +  
  
  xlim(50,100)+
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  )







ggplot(satisfaction_agency, aes(x = fct_rev(cat_group), y = prop_agree, fill = cat_group)) +
  facet_wrap(.~agency) + 
  
  geom_line(color = "grey")+
  geom_point(size = 10, alpha = 1, color = "black") +
  geom_point(shape = 21, size = 9, alpha = 0.9, color = "black") +
  scale_fill_manual(values = c("black", "#d10a0a", "#fd8b45", "#ffd100",
                               "#52b57f", "#8c52ff", "#19bdd4", "blue")) +
  ylim(55,100)+
  theme_classic() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  )







s_position_pay <- satisfaction_position %>%
  filter(question == "s_pay_recat")


ggplot() + 
  geom_point(
    data = s_position_pay, 
    aes(
      x = sup_status, 
      y = prop_agree, 
      color = sup_status,
      group = sup_status
    )
  ) + 
  
  geom_errorbar(
    data = s_position_pay,
    width = 0.1,
    aes(
      x = sup_status, 
      y = prop_agree,
      ymin = ci_lower, 
      ymax = ci_upper
      )
    ) + 
  
  ylim(0,100) + 
  
  theme(
    axis.text.x = element_blank(),
    legend.position = "none"
  ) 

