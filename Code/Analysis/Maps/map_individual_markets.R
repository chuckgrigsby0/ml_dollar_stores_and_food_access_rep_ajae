# ------------------------- #
# Load packages
# ------------------------- #

pacman::p_load("here", "dplyr", "purrr", "stringr", "tidyr", "sf", 
               "units", "ggplot2", "leaflet", "tmap",  "cols4all", "readr")
options(scipen=999)

# ------------------------- #
# Isocrhones/Driving distances. 
# ------------------------- #

# Urban
load(here::here('Data', 'isochrones_bg_pop_centroids_2010_2M_urban.RData'))
drive_dist_urban <- isochrones_bg_pop_centroids_10_2M_urban
rm(isochrones_bg_pop_centroids_10_2M_urban)

# Rural
drive_dist_rural <- load(here::here('Data', 'isochrones_bg_pop_centroids_2010_10M_rural.RData'))
drive_dist_rural <- isochrones_bg_pop_centroids_10_10M_rural
rm(isochrones_bg_pop_centroids_10_10M_rural)

# ------------------------- #
# BG polygons
# ------------------------- #
load(here::here('Data', 'block_groups_2010_polygons_tigris.RData'))

# ------------------------- #
# BG centroids
# ------------------------- #
load(here::here('Data', 'bg_pop_centroids_2010_projected_w_urban_areas.RData'))

# ------------------------- #
# US States
# ------------------------- #
us_states <- st_read(dsn = here::here('Data', 'us_states_shp', 'us_states.shp'))


# Additional data
# ------------------------- #
# Optimal model results from XGBoost. 
# ------------------------- #

# xgb_mods <- paste0('xgboost_10m_', c("urban_", "rural_"), "low_access_final.rds")
# 
# xgb_urban <- readRDS(here::here('Analysis', 'Model_Training', 'Low_Access', xgb_mods[1]))
# xgb_rural <- readRDS(here::here('Analysis', 'Model_Training', 'Low_Access', xgb_mods[2]))

# ------------------------- #
# BGs with DS policies. 
# ------------------------- #

block_groups_w_ds_policies <- read_csv("Data/block_groups_w_ds_policies.csv")

# ------------------------- #
bg_pop_centroids_10_sfp_geo <- st_transform(bg_pop_centroids_10_sfp_geo, crs = 'EPSG:5070')
bg_polygons_sfp <- st_transform(block_groups_2010_tigris, crs = 'EPSG:5070')
drive_dist_urban <- st_transform(drive_dist_urban, crs = 'EPSG:5070')
drive_dist_rural <- st_transform(drive_dist_rural, crs = 'EPSG:5070')
# ------------------------- #
bg_pop_centroids_10_sfp_geo <- bg_pop_centroids_10_sfp_geo %>%
  
  mutate(Geography = case_when( 
    grepl('Urbanized$|Urban Cluster$', Geography) ~ str_replace_all(Geography, 
                                                                    c('Urbanized' = 'Urban', 
                                                                      'Urban Cluster' = 'Urban')), 
    TRUE ~ Geography
  )
  ) %>%
  
  filter(STATE != 'Puerto Rico') %>%
  
  mutate(Geography = factor(Geography, levels = c('Urban', 'Rural'))) %>%
  
  rename(population = POPULATION)
# ------------------------- #
bg_polygons_sfp <- bg_polygons_sfp %>%
  
  left_join(select(
    st_drop_geometry(bg_pop_centroids_10_sfp_geo), 
    GEOID, GEOID_TR, STATE, COUNTY, market_name, Geography, market_name_full, population
  ), by = 'GEOID') %>%
  
  filter(STATE != 'Puerto Rico') %>%
  
  mutate(Geography = factor(Geography, levels = c('Urban', 'Rural')))
# ------------------------- #
# Create shape of contiguous United States. 
# ------------------------- #
us_states_sfp <- st_transform(us_states, crs = 'EPSG:5070')

contig_us <- st_union(us_states_sfp)
# ------------------------- #
block_groups_w_ds_policies <- block_groups_w_ds_policies %>%
  
  left_join( 
    select(bg_pop_centroids_10_sfp_geo, GEOID, Geography, market_name), 
    by = 'GEOID')

# block_groups_w_ds_policies %>% 
#   
#   filter(Geography == 'Rural' & policy_total > 0) %>% 
#   distinct(City, State, market_name) %>%
#   View()

# block_groups_w_ds_policies %>%
#   
#   filter(str_detect(City, 'Salina') & 
#            State == 'KS' & 
#            Geography == 'Rural &
#            policy_total == 1)

# Salina, KS
# --------------------------- #
bans_city <- block_groups_w_ds_policies %>% 
  
  filter(str_detect(City, 'Birmingham') & 
           State == 'AL' & 
           Geography == 'Urban' & 
           policy_total == 1) %>%
  
  select(-matches("^policy_year_")) %>%
  
  slice(1)

bgs_w_bans <- unique(bans_city$GEOID); bgs_w_bans
# --------------------------- #
bg_poly_city <- bg_polygons_sfp %>%
  
  filter(grepl('Birmingham, AL', market_name))

bg_poly_city <- bg_poly_city %>%
  
  filter(GEOID %in% bgs_w_bans)
# --------------------------- #
bg_cent_city <- bg_pop_centroids_10_sfp_geo %>%
  
  filter(GEOID %in% bgs_w_bans)
# --------------------------- #
drive_dist_city <- drive_dist_urban %>%
  
  filter(GEOID %in% bgs_w_bans)

# --------------------------- #
# Summary statistics on driving distance/BG area comparison. 
# --------------------------- #
paste0('Drive distance area: ', 
       st_area(st_geometry(drive_dist_city)) %>% 
         units::set_units('mi^2') %>%
         round(digits = 2), ' mi^2')
paste0('BG area: ', 
       st_area(st_geometry(bg_poly_city)) %>% 
         units::set_units('mi^2') %>%
         round(digits = 2), ' mi^2')
# ------------------------- #
# Load packages
# c4a_gui()
# colors <- c4a(palette = 'tol.medium', n = 2, type = 'cat')
colors <- c4a(palette = 'tableau.classic_gray5', n = 4, type = 'cat')
# ------------------------- #
# tmap_design_mode() # Toggle on/off to visualize where map components, such as legend are placed. 
# References: 
# https://r-tmap.github.io/tmap/reference/tm_add_legend.html
# https://r-tmap.github.io/tmap/articles/adv_positions

tm = tm_shape(drive_dist_city)+
  
  tm_polygons(fill = colors[1], 
              fill_alpha = 0.6,
              col = colors[1])+
  
  tm_shape(bg_poly_city)+
  
  tm_polygons(fill = colors[2], 
              fill_alpha = 0.6, 
              col = colors[3], 
              col_alpha = 0.5)+

  tm_shape(bg_cent_city)+
  
  tm_symbols(size = 0.8, 
             fill = colors[3], 
             col = colors[3])+
  
  tm_scalebar(position = tm_pos_in(pos.h = 'left', pos.v = 'bottom', align.h = 'left', align.v = 'bottom'), 
              width = 10, 
              text.size = 1)+
  
  tm_basemap(server = 'CartoDB.Positron', 
             alpha = 0.8)+
  
  # tm_credits(text = 'Census block group and centroid in Birmingham, AL with two-mile drive boundary.', 
  #          position = tm_pos_in(pos.h = 'right', pos.v = 'bottom', align.h = 'right', align.v = 'bottom'), 
  #            fontface = 'bold')+ 
  
  tmap_options(# credits.size = 1, 
               unit = 'mi', 
               component.autoscale = FALSE)


tmap_save(tm = tm, 
          filename = here::here('Analysis', 'Figures', 'Low_Access', 'Urban', 'maps', 'map_drive_dist_w_bg_birmingham.png'), 
          width = 2400, 
          height = 1600, 
          dpi = 300,
          outer.margins = c(0, 0, 0, 0))

# ------------------------- #
# Repeat for a rural county with bans. 
# ------------------------- #

bans_county <- block_groups_w_ds_policies %>% 
  
  filter(str_detect(City, 'Salina') & 
           State == 'KS' & 
           Geography == 'Rural' & 
           policy_total == 1) %>%
  
  select(-matches("^policy_year_")) %>%
  
  slice(1)
# --------------------------- #
bgs_w_bans <- unique(bans_county$GEOID); bgs_w_bans
# --------------------------- #
bg_poly_county <- bg_polygons_sfp %>%
  
  filter(grepl('Saline County', market_name))

bg_poly_county <- bg_poly_county %>%
  
  filter(GEOID %in% bgs_w_bans)
# --------------------------- #
bg_cent_county <- bg_pop_centroids_10_sfp_geo %>%
  
  filter(GEOID %in% bgs_w_bans)
# --------------------------- #
drive_dist_county <- drive_dist_rural %>%
  
  filter(GEOID %in% bgs_w_bans)
# --------------------------- #
# Summary statistics on driving distance/BG area comparison. 
# --------------------------- #
paste0('Drive distance area: ', 
       st_area(st_geometry(drive_dist_county)) %>% 
         units::set_units('mi^2') %>%
         round(digits = 2), ' mi^2')

paste0('BG area: ', 
       st_area(st_geometry(bg_poly_county)) %>% 
         units::set_units('mi^2') %>%
         round(digits = 2), ' mi^2')
# --------------------------- #
tm <- tm_shape(drive_dist_county)+
  
  tm_polygons(fill = colors[1], 
              fill_alpha = 0.6,
              col = colors[1])+
  
  tm_shape(bg_poly_county)+
  
  tm_polygons(fill = colors[2], 
              fill_alpha = 0.6, 
              col = colors[3], 
              col_alpha = 0.5)+
  
  tm_shape(bg_cent_county)+
  
  tm_symbols(size = 0.8, 
             fill = colors[3], 
             col = colors[3])+
  
  tm_scalebar(position = tm_pos_in(pos.h = 'left', pos.v = 'bottom', align.h = 'left', align.v = 'bottom'), 
              width = 10, 
              text.size = 1)+
  
  tm_basemap(server = 'CartoDB.Positron', 
             alpha = 0.8)+
  
  # tm_credits(text = 'Census block group and centroid in Salinas County, KS with two-mile drive boundary.', 
  #          position = tm_pos_in(pos.h = 'right', pos.v = 'bottom', align.h = 'right', align.v = 'bottom'), 
  #            fontface = 'bold')+ 
  
  tmap_options(# credits.size = 1, 
               unit = 'mi', 
               component.autoscale = FALSE)

tmap_save(tm = tm, 
          filename = here::here('Analysis', 'Figures', 'Low_Access', 'Rural', 'maps', 'map_drive_dist_w_bg_salina.png'), 
          width = 2400, 
          height = 1600,
          dpi = 300,
          outer.margins = c(0, 0, 0, 0))
