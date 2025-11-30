
region_responses <- read.csv(file = "_data/df_exports/responses_by_region.csv")

mt <- all_sf$MT_boundary
mt_cntys <- all_sf$MT_counties
tribes  <- all_sf$tribal_nations %>% select(NAME, NAMELSAD)
mt_hd_pnts <- all_sf$mt_hd_pnts
regions <- all_sf$ampho_regions %>% mutate(region = factor(region))



#regions <- all_sf$ampho_regions %>%
  #st_make_valid() %>%
  #left_join(region_responses, by = "region") %>%
  #mutate(region = factor(region))


r1 <- regions %>% filter(region == '1')
r2 <- regions %>% filter(region == '2')
r3 <- regions %>% filter(region == '3')
r4 <- regions %>% filter(region == '4')
r5 <- regions %>% filter(region == '5')

region_points <- st_centroid(regions)


rgn_fill <- "#bfddf2"
rgn_blur <- "#8799ac"
rgn_line <- "#688ba4"
cnty_line <- "#688ba4"
rgn_pnt <-  "#002e6d"

ln_wdth <- 1.5
sgma <- 8




region_map_pnts <- 
ggplot() +  
  geom_sf(data = regions,  color = NA, fill = rgn_fill) + 
  
  as_reference(geom_sf(data = r1, color = rgn_blur, fill = NA,linewidth = ln_wdth), 
    id = "r1") +
  as_reference(geom_sf(data = r2, color = rgn_blur, fill = NA,linewidth = ln_wdth),
    id = "r2") +
  as_reference(geom_sf(data = r3, color = rgn_blur, fill = NA,linewidth = ln_wdth),
    id = "r3") +
  as_reference(geom_sf(data = r4, color = rgn_blur, fill = NA,linewidth = ln_wdth),
    id = "r4") +
  as_reference(geom_sf(data = r5, color = rgn_blur, fill = NA,linewidth = ln_wdth),
    id = "r5") +
  
  with_blur("r1", sigma = sgma) +
  with_blur("r2", sigma = sgma) +
  with_blur("r3", sigma = sgma) +
  with_blur("r4", sigma = sgma) +
  with_blur("r5", sigma = sgma) +
  
  geom_sf(data = mt_cntys, fill = NA, color = alpha(cnty_line, 0.25))+
  geom_sf(data = regions, fill = NA, color = rgn_line, linewidth = 0.8) + 
  geom_sf(data = region_points, aes(size = pct_respon), color = rgn_pnt)+
  geom_sf(data = tribes, fill = alpha("#7cbdeb", 1), color = cnty_line) + 
  scale_size_continuous(range = c(10, 30), trans = "sqrt") +
    
  theme_void() + 
  theme(legend.position = "none")



##################


region_map <- 
  ggplot() +  
  geom_sf(data = regions,  color = NA, fill = rgn_fill) + 
  
  as_reference(geom_sf(data = r1, color = rgn_blur, fill = NA,linewidth = ln_wdth), 
               id = "r1") +
  as_reference(geom_sf(data = r2, color = rgn_blur, fill = NA,linewidth = ln_wdth),
               id = "r2") +
  as_reference(geom_sf(data = r3, color = rgn_blur, fill = NA,linewidth = ln_wdth),
               id = "r3") +
  as_reference(geom_sf(data = r4, color = rgn_blur, fill = NA,linewidth = ln_wdth),
               id = "r4") +
  as_reference(geom_sf(data = r5, color = rgn_blur, fill = NA,linewidth = ln_wdth),
               id = "r5") +
  
  with_blur("r1", sigma = sgma) +
  with_blur("r2", sigma = sgma) +
  with_blur("r3", sigma = sgma) +
  with_blur("r4", sigma = sgma) +
  with_blur("r5", sigma = sgma) +
  
  geom_sf(data = mt_cntys, fill = NA, color = alpha(cnty_line, 0.55))+
  geom_sf(data = regions, fill = NA, color = rgn_line, linewidth = 0.8) + 
  geom_sf(data = tribes, fill = alpha("#7cbdeb", 1), color = cnty_line) + 

  theme_void() + 
  theme(legend.position = "none")



region_map_blank <- 
  ggplot() +  
  geom_sf(data = regions,  color = NA, fill = rgn_fill) + 
  
  as_reference(geom_sf(data = r1, color = rgn_blur, fill = NA,linewidth = ln_wdth), 
               id = "r1") +
  as_reference(geom_sf(data = r2, color = rgn_blur, fill = NA,linewidth = ln_wdth),
               id = "r2") +
  as_reference(geom_sf(data = r3, color = rgn_blur, fill = NA,linewidth = ln_wdth),
               id = "r3") +
  as_reference(geom_sf(data = r4, color = rgn_blur, fill = NA,linewidth = ln_wdth),
               id = "r4") +
  as_reference(geom_sf(data = r5, color = rgn_blur, fill = NA,linewidth = ln_wdth),
               id = "r5") +
  
  with_blur("r1", sigma = sgma) +
  with_blur("r2", sigma = sgma) +
  with_blur("r3", sigma = sgma) +
  with_blur("r4", sigma = sgma) +
  with_blur("r5", sigma = sgma) +
  
  geom_sf(data = regions, fill = NA, color = rgn_line, linewidth = 0.8) + 
  geom_sf(data = tribes, fill = alpha("#7cbdeb", 1), color = cnty_line) + 
  
  theme_void() + 
  theme(legend.position = "none")



######################
######################
######################
######################




col_pal <- c("#fbd113", "#002e6d", "#6da587",  "#7cbdeb")

f_pal <- c("#fbd113", "#f1f0ea", "#f1f0ea",  "#f1f0ea")
s_pal <- c("#f1f0ea", "#002e6d", "#f1f0ea",  "#f1f0ea")
m_pal <- c("#f1f0ea", "#f1f0ea", "#6da587",  "#f1f0ea")
l_pal <- c("#f1f0ea", "#f1f0ea", "#f1f0ea",  "#7cbdeb")


mt_hd_pnts <- mt_hd_pnts %>%
  mutate(w = 0.1)

hd_dorl <- 
  cartogram::cartogram_dorling(
    mt_hd_pnts, 
    k = 0.1,
    weight  = "w",
    itermax = 6) %>%
  mutate(
    hd_cat = factor(hd_cat, levels = c("Frontier", "Small", "Medium", "Large")),
    hd_lbl = str_replace(
      hd_lbl,
      "(THD).*", ""
    ) %>% str_squish(),
    hd_lbl = str_wrap(hd_lbl, width = 15)
  )


hd_dorl_pnts <- st_centroid(hd_dorl)

mt_hd_lbl_pnts <- hd_dorl_pnts %>%
  mutate(coords = st_coordinates(geometry)) %>%
  mutate(
    x = coords[,1],
    y = coords[,2]
  )




map_hd_size_nolbl <- 
  
ggplot() +
  geom_sf(data = mt, fill = NA, color = "#688ba4") +
  geom_sf(data = hd_dorl, aes(fill = hd_cat)) + 
  
  scale_fill_manual(values = col_pal, guide = "none") +
  theme_void()+
  theme(
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )



####################
##                ##
##    Frontier    ##
##                ##
####################

f_dorl <- hd_dorl %>%
  mutate(
    alpha_val = if_else(hd_cat == "Frontier", 1, 0.7)
  )

f_lbl <- mt_hd_lbl_pnts %>% filter(hd_cat == "Frontier")


m_frontier_hds <- make_hd_map(
  cat     = "Frontier",
  hd_dorl = hd_dorl,
  lbl_df  = mt_hd_lbl_pnts,
  mt_sf   = mt,
  pal     = f_pal,
  lbl_col = "#002e6d"
)  



m_small_hds <- make_hd_map(
  cat     = "Small",
  hd_dorl = hd_dorl,
  lbl_df  = mt_hd_lbl_pnts,
  mt_sf   = mt,
  pal     = s_pal,
  lbl_col = "white"
)

m_small_hds

m_med_hds <- make_hd_map(
  cat     = "Medium",
  hd_dorl = hd_dorl,
  lbl_df  = mt_hd_lbl_pnts,
  mt_sf   = mt,
  pal     = m_pal,
  lbl_col = "black"
)

m_med_hds


m_lg_hds <- make_hd_map(
  cat     = "Large",
  hd_dorl = hd_dorl,
  lbl_df  = mt_hd_lbl_pnts,
  mt_sf   = mt,
  pal     = l_pal,
  lbl_col = "#002e6d"
)

m_lg_hds




##################
ggsave(
  filename = here("_www/map_exports/region_map_blank.png"),
  plot = region_map_blank,
  width = 12, 
  height = 8,
  units = "in",
  dpi = 300
)
