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

# block_groups_w_ds_policies <- read_csv("Data/block_groups_w_ds_policies.csv")

# ------------------------- #

bg_polygons_sfp <- st_transform(block_groups_2010_tigris, crs = 'EPSG:5070')


bg_polygons_sfp <- bg_polygons_sfp %>%
  
  left_join(select(
    st_drop_geometry(bg_pop_centroids_10_sfp_geo), 
    GEOID, GEOID_TR, STATE, COUNTY, market_name, Geography, market_name_full, POPULATION
  ), by = 'GEOID') %>%
  
  rename(population = POPULATION) %>%
  
  mutate(Geography = case_when( 
    grepl('Urbanized$|Urban Cluster$', Geography) ~ str_replace_all(Geography, 
                                                                    c('Urbanized' = 'Urban', 
                                                                      'Urban Cluster' = 'Urban')), 
    TRUE ~ Geography
    )
  ) %>%
  
  filter(STATE != 'Puerto Rico') %>%
  
  mutate(Geography = factor(Geography, levels = c('Urban', 'Rural')))

# Create shape of contiguous United States. 

us_states_sfp <- st_transform(us_states, crs = 'EPSG:5070')

contig_us <- st_union(us_states_sfp)


mkt_polygons_sfp <- bg_polygons_sfp %>%
  
  group_by(
    market_name, Geography
  ) %>%
  
  summarise(population = sum(population, na.rm=TRUE))

# ------------------------- #
# ------------------------- #
# Load packages
c4a_gui()
# colors <- c4a(palette = 'tol.medium', n = 2, type = 'cat')
colors <- c4a(palette = 'tableau.seattle_grays', n = 2, type = 'cat')
# ------------------------- #
# Create separate urban/rural splits. 
# ------------------------- #

urban_mkts <- mkt_polygons_sfp %>%
  
  filter(Geography == 'Urban')


rural_mkts <- mkt_polygons_sfp %>%
  
  filter(Geography == 'Rural')

# Recover missing county for maps. 
sd_bgs <- bg_polygons_sfp %>%
  filter(str_detect('South Dakota', STATE)) 

sd_bgs <- st_union(sd_bgs)

sd <- us_states_sfp %>%
  filter(grepl('South Dakota', NAME)) 

missing_county <- st_difference(sd, sd_bgs)

# Cast to individual polygons to find the largest polygon
# referencing the individual county. 
missing_polygons <- st_cast(missing_county, 'POLYGON')

missing_polygons <- missing_polygons %>%
  
  mutate(areas = st_area(geometry))

# After plotting, we verified that the polygon with max. area is the missing 
# county. 
missing_county <- missing_polygons %>%
  
  slice_max(areas)

# ------------------------- #
# tmap_design_mode() # Toggle on/off to visualize where map components, such as legend are placed. 
# References: 
# https://r-tmap.github.io/tmap/reference/tm_add_legend.html
# https://r-tmap.github.io/tmap/articles/adv_positions

tm_shape(urban_mkts)+
  
  tm_polygons(fill = colors[1], 
              col = NULL)+
  # Additional arguments not used. : 
  #fill.scale = tm_scale_categorical(values = colors[1]), 
  #fill.legend = tm_legend(show = FALSE),
  
  tm_shape(rural_mkts)+

  tm_polygons(fill = colors[2],
              fill_alpha = 0.3,
              col = colors[2],
              lwd = 0.5)+
  # Additional arguments not used. 
  # col.scale = tm_scale_categorical(values = colors[2]),
  # col.legend = tm_legend(show = FALSE),
  # col_alpha = 0.9,
  
  tm_shape(missing_county)+
  
  tm_polygons(fill = colors[2],
              fill_alpha = 0.3,
              col = colors[2],
              lwd = 0.5)+
  
  tm_add_legend(type = 'polygons', # Default is symbols for points. If you are using fill, then use polygons.
                title = 'Geography', 
                title.fontface = 'bold',
                title.align = 'center',
                title.size = 1.2,
                fill = colors, 
                labels = c('Urban', 'Rural'), 
                text.size = 1.1,
                #item.height = 0.9,
                position = c('left', 'bottom'), 
                orientation = 'portrait')

