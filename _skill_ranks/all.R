all <- rbind(
             comm_begin, data_begin, engage_begin, 
             expert_begin, policy_begin
             )

all_sum <- all %>%
  group_by(domain) %>%
  mutate(
    prop_important = 100* n_score / 448
  ) %>% ungroup()


ggplot(all_sum) + 
  geom_point(aes(x=prop_important, y=skill_prop, color = domain)) +
  facet_wrap(~domain)



library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)


all_sum_long <- all_sum %>%
  select(topic, prop_important, skill_prop) %>%
  pivot_longer(cols = c(prop_important, skill_prop),
               names_to = "metric", values_to = "value")



# pick thresholds; median splits work well to start
x_thr <- median(all_sum$prop_important, na.rm = TRUE)
y_thr <- median(all_sum$skill_prop,  na.rm = TRUE)

# an optional composite priority score for sorting or tables
all_sum <- all_sum %>%
  mutate(priority_score = prop_important * skill_prop)

ggplot(all_sum, aes(x = prop_important, y = skill_prop, color = domain)) +
  geom_hline(yintercept = y_thr, linetype = "dashed") +
  geom_vline(xintercept = x_thr, linetype = "dashed") +
  geom_point(size = 3, alpha = 0.9) +
  coord_equal() +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Training Priorities by Topic",
    subtitle = "Upper-right quadrant = high importance & many beginners",
    x = "% of staff rating topic as highly important",
    y = "% of staff self-rating as beginner in this topic"
  ) +
  theme_classic(base_size = 13)



