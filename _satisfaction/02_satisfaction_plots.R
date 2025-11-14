
satisfaction_p1 <- 
  
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






