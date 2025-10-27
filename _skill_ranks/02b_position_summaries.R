#nonsup_summary <- calc_skill_prop(non_sup_df) %>% arrange(desc(priority_score))
#sup_summary <- calc_skill_prop(sup_df) %>% arrange(desc(priority_score))
#exec_summary <- calc_skill_prop(exec_df) %>% arrange(desc(priority_score))


complete <- rbind(nonsup_summary, sup_summary, exec_summary) %>%
  
  mutate(
   domain_cat = case_when(
     domain %in% c("budget", "change",  "policy", "systems") ~ "budget, change, policy, systems",
     domain %in% c("partners",  "engage") ~ "partners, engage",
     domain %in% c("comm",  "data", "expert") ~ "comm, data, expert",
     domain == "justice" ~ "DEI/justice", 
     TRUE ~ NA_character_
   ),
   
   domain_cat = 
     factor(
       domain_cat, levels = 
         c(
           "comm, data, expert",
           "DEI/justice",
           "budget, change, policy, systems",
           "partners, engage"
           )
        ),
   supervisor_status = 
     factor(supervisor_status, 
            levels = 
              c("Executive", "Supervisor", "Non-supervisor")
            )
  ) 

comp_subset <- complete %>% select(supervisor_status, imp_prop)

#write.csv(comp_subset, "_www/df_exports/comp_subset.csv")



ggplot(comp_subset) + 
geom_boxplot(
  aes(y = factor(supervisor_status), 
      x = imp_prop,
      fill = supervisor_status
      ),
  alpha = 0.5,
  color = "#1f3465"
  
) + 
scale_fill_manual(values = c("#95c6ea",  "#fcd008", "#1f3465")) +  
xlim(0,100) + 
theme(
  legend.position = "none",
  panel.background = element_rect(fill = "transparent", colour = NA),  
  plot.background  = element_rect(fill = "transparent", colour = NA),
  panel.grid = element_blank()
)



ggplot(all_sum) + 
  geom_boxplot(
    aes(y = factor(supervisor_status), 
        x = priority_score,
        fill = supervisor_status
    ),
    alpha = 0.5,
    color = "#1f3465"
    
  ) + 
  scale_fill_manual(values = c("#95c6ea",  "#fcd008", "#1f3465")) +  
  xlim(0,100) + 
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),  
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank()
  )








summary(nonsup_summary$imp_prop)
summary(sup_summary$imp_prop)
summary(exec_summary$imp_prop)

IQR(nonsup_summary$imp_prop)
IQR(sup_summary$imp_prop)
IQR(exec_summary$imp_prop)

imp_sum <- complete %>%
  group_by(domain_cat, supervisor_status, imp_recat) %>%
  summarise(
    avg_imp = mean(imp_prop),
    min_imp = min(imp_prop),
    max_imp = max(imp_prop),
    med_imp = median(imp_prop),
    .groups = "drop"
  )



imp_plot <- 
  
ggplot()+
facet_wrap(~domain_cat, ncol = 1) + 
  
geom_boxplot(
  data = complete,
  aes(
    x=supervisor_status, 
    y=imp_prop,
    fill = factor(supervisor_status)
  ),
  
  alpha   = 0.7,
  size = 0.5,            
  fatten = 3,
  color = "#1f3465"
) +
  
scale_fill_manual(values = c("#95c6ea", "#a8b09d", "#fcd008")) +  
  
ylim(40,100)+
coord_flip()+
  
theme(
  legend.position = "none",
  panel.background = element_rect(fill = "transparent", colour = NA),  
  plot.background  = element_rect(fill = "transparent", colour = NA),
  panel.grid = element_blank()
)


  



ggsave(
  filename = here("_www/plot_exports/imp_plot.png"),
  plot = imp_plot,
  width = 5,
  height = 8,
  units = "in",
  dpi = 900,
  bg = "transparent"
  
)
