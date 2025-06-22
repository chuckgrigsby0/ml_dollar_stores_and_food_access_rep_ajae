# ----------------- #
# Load packages
# ----------------- #

pacman::p_load("here", "dplyr", "purrr", "stringr", "tidyr", "sf", "units", "ggplot2", "leaflet", "tmap", "readr")
options(scipen=999)

# ----------------- #
# Isocrhones/Driving distances. 
# ----------------- #
# Urban
load(here::here('Data', 'isochrones_bg_pop_centroids_2010_2M_urban.RData'))
drive_dist_urban <- isochrones_bg_pop_centroids_10_2M_urban
rm(isochrones_bg_pop_centroids_10_2M_urban)

# Rural
drive_dist_rural <- load(here::here('Data', 'isochrones_bg_pop_centroids_2010_10M_rural.RData'))
drive_dist_rural <- isochrones_bg_pop_centroids_10_10M_rural
rm(isochrones_bg_pop_centroids_10_10M_rural)

# ----------------- #
# BG polygons
# ----------------- #
load(here::here('Data', 'block_groups_2010_polygons_tigris.RData'))

# ----------------- #
# BG centroids
# ----------------- #
load(here::here('Data', 'bg_pop_centroids_2010_projected_w_urban_areas.RData'))
# ----------------- #

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
  
  filter(STATE != 'Puerto Rico')

# ----------------- #
# Summary statistics by urban and rural areas for block-group polygons. 
# ----------------- #
bg_polygons_sfp <- bg_polygons_sfp %>% 
  
  mutate(across(.cols = ALAND, 
                .fns = \(x) units::as_units(x, 'm^2') %>%
                  units::set_units('mi^2') %>%
                  as.numeric() 
  )
  ) %>%
  
  mutate(pop_density = population/ALAND)

# ----------------- #
# Compute summary statistics for population density and total population for each geography
# ----------------- #
sum_stats_bg <- bg_polygons_sfp %>%
  
  st_drop_geometry() %>%
  
  group_by(Geography) %>%
  
  summarise(across(.cols = c(population, pop_density, ALAND), 
                   .fns = list('avg' = \(x) mean(x, na.rm = TRUE),
                               'sd' = \(x) sd(x, na.rm = TRUE),
                               'total' = \(x) sum(x, na.rm = TRUE)), 
                   .names = '{.col}_{.fn}')) %>%
  
  select(-c(pop_density_total, ALAND_total))

sum_stats_bg <- sum_stats_bg %>%
  
  ungroup() %>%
  
  mutate(population_us = sum(population_total), 
         population_share = population_total/population_us) %>%
  
  relocate(c(population_us, population_share), .after = population_total)
# ----------------- #
# Calculate number/share of block groups for each geography. 
# ----------------- #
num_bgs <- bg_polygons_sfp %>%
  
  st_drop_geometry() %>%
  
  group_by(Geography) %>%
  
  summarise(block_groups_total = n()) %>%
  
  ungroup() %>%
  
  mutate(block_groups_total_us = sum(block_groups_total), 
         block_groups_share = block_groups_total/block_groups_total_us)


sum_stats_bg <- sum_stats_bg %>%
  
  left_join(num_bgs, by = 'Geography') 

sum_stats_bg$shape_type = 'Block Groups'

# Compute land area of isochrones for comparison with block groups
compute_land_area_isochrones <- function(dta_isoc, geog_str){
  
  sum_stats_isoc <- dta_isoc %>%
    
    st_transform(crs = 'EPSG:5070') %>%
    
    mutate(ALAND = st_area(geometry))
  
  
  sum_stats_isoc <- sum_stats_isoc  %>%
    
    mutate(across(.cols = ALAND, 
                  .fns = \(x) units::set_units(x, 'mi^2') %>%
                    as.numeric() 
    )
    ) %>%
    
    st_drop_geometry() %>%
    
    summarise(across(.cols = ALAND, 
                     .fns = list('avg' = \(x) mean(x, na.rm = TRUE), 
                                 'sd' = \(x) sd(x, na.rm = TRUE) ), 
                     .names = '{.col}_{.fn}'
    ) 
    ) %>%
    
    mutate(Geography = geog_str, 
           shape_type = 'Driving Distance')
  
  return(sum_stats_isoc)
  
}

sum_stats_isoc_urb <- compute_land_area_isochrones(dta_isoc = drive_dist_urban, geog_str = 'Urban')
sum_stats_isoc_rur <- compute_land_area_isochrones(dta_isoc = drive_dist_rural, geog_str = 'Rural')


sum_stats_isoc <- bind_rows(sum_stats_isoc_rur, 
                            sum_stats_isoc_urb)


sum_stats_isoc <- bind_rows(sum_stats_isoc_rur, 
                            sum_stats_isoc_urb)

sum_stats_cb <- bind_rows(sum_stats_bg, sum_stats_isoc) %>%
  relocate(c(shape_type, matches('ALAND')), .after = Geography) 
# ----------------- #