
sex_position_props <- data_cleaned %>%
  filter(gender != "Prefer not to answer")


sex_position_stats <- summarise_group_props(
  data     = data_cleaned %>% filter(!gender %in% c("Prefer not to answer", "In some other way (please specify)")),
  group    = sup_status,
  category = gender
)


size_sum <- data_cleaned %>%
  group_by(size) %>%
  summarise(
    n_size = n(), 
    size_prop = 100*n_size/448,
    .groups = "drop"
  )

str(sex_position_stats)



ggplot(data = sex_position_stats %>% filter(gender == "Man"), aes(x = sup_status, y = prop)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.1
  ) +
  facet_wrap(~ gender) +
  scale_y_continuous(
    name   = "Percent",
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    x = "Gender",
    title = "Gender distribution by position type"
  ) +
  theme_classic()


write.csv(sex_position_stats, "sex_position_stats.csv", row.names = FALSE)
