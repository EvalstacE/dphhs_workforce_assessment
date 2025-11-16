resp <- read_csv(file = "_data/responses_cnty.csv") %>%
  group_by(hd_cat) %>%
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
                                       "Large", "Tribal"), ordered = TRUE)
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


region_sum_filtered <- resp %>%
  filter(hd != "RiverStone Health") %>%
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

size_sum_filtered <- resp %>%
  filter(hd != "RiverStone Health") %>%
  group_by(hd_cat) %>%
  summarise(
    num_depts = n(),
    num_missing_staff = sum(is.na(total_staff)),
    size_staff = sum(staff_estimate, na.rm = TRUE),
    size_resps = sum(total_responses, na.rm = TRUE),
    pct_respon = size_resps / size_staff
  )


size_cnty_total <- size_sum_filtered %>%
  filter(hd_cat != "Tribal") %>%
  summarise(
    total_staff = sum(size_staff),
    total_responses = sum(size_resps),
    pct_total = total_responses / total_staff
  )

tribal_total <- size_sum_filtered %>%
  filter(hd_cat == "Tribal") %>%
  summarise(
    total_staff = sum(size_staff),
    total_responses = sum(size_resps),
    pct_total = total_responses / total_staff
  )


##--Exports:
#write.csv(size_sum, "_www/df_exports/responses_by_size.csv", row.names = FALSE)
#write.csv(size_sum_filtered, "_www/df_exports/responses_by_size_filtered.csv", row.names = FALSE)
#write.csv(region_sum, "_www/df_exports/responses_by_region.csv", row.names = FALSE)
#write.csv(region_sum_filtered, "_www/df_exports/responses_by_region_filtered.csv", row.names = FALSE)