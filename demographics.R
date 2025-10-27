
##################################
##################################
### Education / Highest Degree ###
##################################


degrees_region <- data_cleaned %>%
  group_by(region, highest_degree) %>%
  summarise(totals = n())
  

degrees_agency <- data_cleaned %>%
  group_by(agency, degree_recat) %>%
  summarise(totals = n())

##################################
##################################
###      Race / Ethnicity      ###
##################################

statewide_race_counts <- data_cleaned %>%
  separate_rows(race, sep = ",") %>%
  mutate(race = trimws(race)) %>%
  filter(!is.na(race) & race != "") %>%
  count(race, sort = TRUE) %>%
  mutate(
    denom = 448,
    prop = n / denom,
    agency = "Statewide"
    
  )

denoms <- data_cleaned %>%
  group_by(agency) %>%
  summarise(denom = n(), .groups = "drop")

# Counts by agency & race from the SPLIT data
race_counts_agency <- data_cleaned %>%
  separate_rows(race, sep = ",") %>%
  mutate(race = trimws(race)) %>%
  filter(!is.na(race) & race != "") %>%
  count(agency, race, name = "n") %>%
  left_join(denoms, by = "agency") %>%
  mutate(prop = n / denom) %>%
  arrange(agency, desc(n)) %>% 
  rbind(statewide_race_counts) %>%
  mutate(prop = 100*prop)


##################################
##################################
####      Position Types      ####
##################################


sup_sum <- data_cleaned %>%
  group_by(supervisor_status) %>%
  summarise(n = n()) %>% ungroup() 


##################################
##################################
####          Age             ####
##################################

age_sum_all <- data_cleaned %>%
  group_by(age_group_lg) %>%
  summarise(n = n(),
            prop = 100*n/448)


age_sum_position <- 
  two_group_count_prop(data_cleaned, 
                        group1 = supervisor_status, 
                        group2 = age_group_lg)

age_sum_stats_position <- data_cleaned %>%
  group_by(supervisor_status) %>%
  summarise(med_age = median(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE), 
            min_age = min(age, na.rm = TRUE),
            IQR_age = IQR(age, na.rm = TRUE))

age_sum_agency <- 
  two_group_count_prop(data_cleaned, 
                       group1 = agency, 
                       group2 = age_group_lg)


