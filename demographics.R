
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
  group_by(age_group) %>%
  summarise(n = n())

age_sum_position <- data_cleaned %>%
  group_by(sup_status_recat, age_group) %>%
    summarise(age_n = n()) %>% ungroup() %>%
  group_by(sup_status_recat) %>%
    mutate(sup_n = sum(age_n)) %>% ungroup()%>%
  group_by(sup_status_recat, sup_n , age_group) %>%
    summarise(prop_age_n = 100*age_n/sup_n) %>% ungroup()



