ggplot(all_sum) + 
  
  facet_wrap(~supervisor_status) + 
  
  geom_density(
    aes(x = skill_prop), 
    color = "black", 
    fill = "grey", 
    alpha = 0.5
  ) + 
  
  geom_density(
    aes(x = imp_prop), 
    color = "black", 
    fill = "red", 
    alpha = 0.5
  ) 