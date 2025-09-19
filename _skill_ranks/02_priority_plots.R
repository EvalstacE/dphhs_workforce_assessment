

all_sum <- rbind(nonsup_summary, sup_summary, exec_summary) %>%
  filter(skill_recat == "Unable to Perform/Beginner") %>%
  mutate(domain = factor(domain, levels = 
                           c("budget", "change",  "policy", "systems", "partners",
                             "justice", "engage", "comm",  "data", "expert")),
         
         domain_cat = case_when(
           domain %in% c("budget", "change",  "policy", "systems") ~ "budget, change, policy, systems",
           domain %in% c("partners",  "engage") ~ "partners, engage",
           domain %in% c("comm",  "data", "expert") ~ "comm, data, expert",
           domain == "justice" ~ "DEI/justice", 
           TRUE ~ NA_character_
         ),
         
         
         
         supervisor_status = factor(supervisor_status, levels = 
                                      c("Non-supervisor", "Supervisor", "Executive"))
         
         
         )


###########################
### plots ###
###########################
nonsup_plot_df <- all_sum %>% filter(supervisor_status == "Non-supervisor")
sup_plot_df <- all_sum %>% filter(supervisor_status == "Supervisor")
exec_plot_df <- all_sum %>% filter(supervisor_status == "Executive")

all_plot <- create_priority_plot(all_sum)

nonsup_plot <- create_priority_plot(nonsup_plot_df)
sup_plot <- create_priority_plot(sup_plot_df)
exec_plot <- create_priority_plot(exec_plot_df)


###########################
###########################

blank_df <- non_sup_df %>% mutate(imp_prop = 0, skill_prop = 0)
blank_plot <- create_priority_plot(blank_df)

ggsave(
  filename = here("_www/plot_exports/all_plot.png"),
  plot = all_plot,
  width = 8,
  height = 11,
  units = "in",
  dpi = 900,
  bg = "transparent"

)

