resp <- read_csv(file = "_data/responses_cnty.csv") %>%
  group_by(hd_cat, agency_type) %>%
  mutate(
    med_size = round(median(total_staff, na.rm = TRUE)),
    min_size = round(min(total_staff, na.rm = TRUE)), 
    max_size = round(max(total_staff, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    total_staff = if_else(total_responses > total_staff, total_responses, total_staff),
    staff_estimate = if_else(is.na(total_staff), med_size, total_staff),
    pct_responded = round(100 * total_responses / staff_estimate),
    hd_cat = factor(hd_cat, levels = c("Frontier", "Small", "Medium", 
                                       "Large"), ordered = TRUE)
  ) %>%
  
  separate(
    col   = location,
    into  = c("lat", "lng"),
    sep   = ",",
    remove = TRUE
  ) %>%
  mutate(
    lat = as.numeric(lat),
    lng = as.numeric(lng)
  )





region_sum <- resp %>%
  group_by(region) %>%
  summarise(
    num_depts = n(),
    num_missing_staff = sum(is.na(total_staff)),
    region_staff = sum(staff_estimate, na.rm = TRUE),
    region_resps = sum(total_responses, na.rm = TRUE),
    pct_respon = 100 * region_resps / region_staff
  )



size_sum <- resp %>%
  group_by(hd_cat) %>%
  summarise(
    size_staff = sum(staff_estimate, na.rm = TRUE),
    size_resps = sum(total_responses, na.rm = TRUE),
    pct_respon = size_resps / size_staff
  )

size_sum_notribal <- resp %>%
  filter(agency_type != "Tribal Health Department") %>%
  group_by(hd_cat) %>%
  summarise(
    size_staff = sum(staff_estimate, na.rm = TRUE),
    size_resps = sum(total_responses, na.rm = TRUE),
    pct_respon = size_resps / size_staff
  )




##--Exports:
#write.csv(resp, "_data/df_exports/responses_by_cnty.csv", row.names = FALSE)
#write.csv(region_sum, "_data/df_exports/responses_by_region.csv", row.names = FALSE)
#write.csv(size_sum, "_data/df_exports/responses_by_size.csv", row.names = FALSE)
#write.csv(size_sum_notribal, "_data/df_exports/responses_by_size_notribal.csv", row.names = FALSE)