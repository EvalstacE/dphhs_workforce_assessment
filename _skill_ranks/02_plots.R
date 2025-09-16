
all_sum <- rbind(nonsup_summary, sup_summary, exec_summary) %>%
  filter(skill_recat == "Unable to Perform/Beginner") %>%
  mutate(domain = factor(domain, levels = 
                           c("budget", "change",  "policy", "systems", "partners",
                             "justice", "engage", "comm",  "data", "expert")))


###########################
### non-supervisor plot ###
###########################
nonsup_plot <- all_sum %>% filter(supervisor_status == "Non-supervisor")

create_priority_plot(nonsup_plot)


###########################
###   supervisor plot   ###
###########################
sup_plot <- all_sum %>% filter(supervisor_status == "Supervisor")



###########################
###   executive plot    ###
###########################
exec_plot <- all_sum %>% filter(supervisor_status == "Executive")

