
##################################
##################################
###   Years in ph workforce    ###
##################################

serv_yrs_all <- data_cleaned %>%
  group_by(max_yrs_cat) %>%
  summarise(n = n(),
            prop = 100*n/448)



serv_yrs_agency <- 
  two_group_count_prop(data_cleaned, 
                       group1 = agency, 
                       group2 = max_yrs_cat)



serv_yrs_position <- 
  two_group_count_prop(data_cleaned, 
                       group1 = sup_status, 
                       group2 = max_yrs_cat)