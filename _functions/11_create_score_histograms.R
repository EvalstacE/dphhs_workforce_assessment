create_score_histogram <- function(score_df) {
  
  p <- ggplot(score_df) +
    
    geom_density(aes(x = priority_score),
                 fill = "#95c6ea"
                 ) +
    
    scale_fill_manual(values = color_scale ) + 

    theme_classic() + 
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background  = element_rect(fill = "transparent", colour = NA)
    )
  
  return(p)
}

create_score_histogram(sup_summary)
create_score_histogram(nonsup_summary)
create_score_histogram(exec_summary)


ggplot(all_sum_nofilter) +
  
  facet_wrap(~supervisor_status) + 
  
  geom_density(
        aes(
             x = priority_score, 
          fill = supervisor_status, 
          group = supervisor_status
        )
  ) +
  
  scale_fill_manual(values = c("#95c6ea", "#95c6ea","#95c6ea")) + 
  
  theme_classic() + 
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  )



ggplot(all_sum_nofilter) +
  
  facet_wrap(~supervisor_status) + 
  
  geom_histogram(
    aes(x = priority_score, fill = supervisor_status),
    color = "black",
    alpha = 0.7,
    bins = 25) +
  
  
  theme_classic() + 
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA)
  )

