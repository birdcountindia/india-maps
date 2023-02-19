# saves a workspace image called "maps_sf.RData"


library(tidyverse)
library(sf)
library(spdep)


# requires shapefiles and several packages
# provide path to folder and name of file within
country_path <- "data/in_2011"
states_path <- "data/in_states_2019"
dists_path <- "data/in_dists_2019"

country_file <- "India_2011"
states_file <- "in_states_2019"
dists_file <- "in_dist_2019"

# grid sizes can be changed
grid_sizes_km <- c(25, 50, 100, 200)
grid_sizes_deg <- grid_sizes_km*1000/111111



# reading maps ------------------------------------------------------------

india_sf <- st_read(dsn = country_path, layer = country_file) %>% mutate(DISTRICT = NULL) %>% 
  st_set_crs("OGC:CRS84")

states_sf <- st_read(dsn = states_path, layer = states_file) %>% 
  dplyr::select(stname, geometry) %>% 
  magrittr::set_colnames(c("STATE", "geometry")) %>% 
  mutate(STATE = str_to_title(STATE)) %>% 
  # replacing ampersand with "and"
  mutate(STATE = str_replace(STATE, "&", "and")) %>% 
  # corrections for Madhu's SPDF values
  mutate(STATE = case_when(STATE == "Dadra and Nagar Have" ~ "Dadra and Nagar Haveli",
                           STATE == "Andaman and Nicobar" ~ "Andaman and Nicobar Islands",
                           TRUE ~ STATE)) %>% 
  st_set_crs("OGC:CRS84")

dists_sf <- st_read(dsn = dists_path, layer = dists_file) %>% 
  dplyr::select(dtname) %>% 
  rename(DISTRICT.NAME = dtname) %>% 
  # some districts have two different rows (two different polygons) so need to combine
  # them into one polygon
  group_by(DISTRICT.NAME) %>% 
  summarise() %>% # dplyr-sf magic :) 
  st_set_crs("OGC:CRS84")
# https://gis.stackexchange.com/questions/421651/merging-two-multipolygon-shapefiles-and-removing-one-of-overlapping-polygons-usi



# creating grids ----------------------------------------------------------

# g1
res <- 1 
cs <- grid_sizes_deg[res]
n <- (c(diff(st_bbox(india_sf)[c(1, 3)]), diff(st_bbox(india_sf)[c(2, 4)]))/cs) %>% ceiling()

g1_sf <- india_sf %>% 
  st_make_grid(cellsize = cs, n = n) %>% 
  st_as_sf() %>% 
  rename(geometry = x) %>% 
  # cell IDs
  rownames_to_column("CELL.ID")

# neighbours
# refer this issue https://github.com/r-spatial/sf/issues/234
# https://en.wikipedia.org/wiki/DE-9IM
g1_nb_r <- poly2nb(g1_sf, queen = FALSE)
g1_nb_q <- poly2nb(g1_sf)


# g2
res <- 2
cs <- grid_sizes_deg[res]
n <- (c(diff(st_bbox(india_sf)[c(1, 3)]), diff(st_bbox(india_sf)[c(2, 4)]))/cs) %>% ceiling()

g2_sf <- india_sf %>% 
  st_make_grid(cellsize = cs, n = n) %>% 
  st_as_sf() %>% 
  rename(geometry = x) %>% 
  # cell IDs
  rownames_to_column("CELL.ID")

g2_nb_r <- poly2nb(g2_sf, queen = FALSE)
g2_nb_q <- poly2nb(g2_sf)


# g3
res <- 3
cs <- grid_sizes_deg[res]
n <- (c(diff(st_bbox(india_sf)[c(1, 3)]), diff(st_bbox(india_sf)[c(2, 4)]))/cs) %>% ceiling()

g3_sf <- india_sf %>% 
  st_make_grid(cellsize = cs, n = n) %>% 
  st_as_sf() %>% 
  rename(geometry = x) %>% 
  # cell IDs
  rownames_to_column("CELL.ID")

g3_nb_r <- poly2nb(g3_sf, queen = FALSE)
g3_nb_q <- poly2nb(g3_sf)


# g4
res <- 4
cs <- grid_sizes_deg[res]
n <- (c(diff(st_bbox(india_sf)[c(1, 3)]), diff(st_bbox(india_sf)[c(2, 4)]))/cs) %>% ceiling()

g4_sf <- india_sf %>% 
  st_make_grid(cellsize = cs, n = n) %>% 
  st_as_sf() %>% 
  rename(geometry = x) %>% 
  # cell IDs
  rownames_to_column("CELL.ID")

g4_nb_r <- poly2nb(g4_sf, queen = FALSE)
g4_nb_q <- poly2nb(g4_sf)



# intersecting grids with admin boundaries and calculating areas, totcells -----------

sf_use_s2(FALSE)

# already in sq.m because switched off S2 so using planar
india_sf <- india_sf %>% mutate(AREA = units::set_units(round(st_area(geometry)), "km2"))
states_sf <- states_sf %>% mutate(AREA = units::set_units(round(st_area(geometry)), "km2"))
dists_sf <- dists_sf %>% mutate(AREA = units::set_units(round(st_area(geometry)), "km2"))


g1_in_sf <- st_intersection(g1_sf, india_sf) %>% 
  mutate(AREA = units::set_units(round(st_area(geometry)), "km2"),
         TOT.CELLS = n_distinct(CELL.ID))
g2_in_sf <- st_intersection(g2_sf, india_sf) %>% 
  mutate(AREA = units::set_units(round(st_area(geometry)), "km2"),
         TOT.CELLS = n_distinct(CELL.ID))
g3_in_sf <- st_intersection(g3_sf, india_sf) %>% 
  mutate(AREA = units::set_units(round(st_area(geometry)), "km2"),
         TOT.CELLS = n_distinct(CELL.ID))
g4_in_sf <- st_intersection(g4_sf, india_sf) %>% 
  mutate(AREA = units::set_units(round(st_area(geometry)), "km2"),
         TOT.CELLS = n_distinct(CELL.ID))



# writing ---------------------------------------------------------------------------

# all maps regularly used in analyses
save(india_sf, states_sf, dists_sf,
     grid_sizes_deg, grid_sizes_km, g1_in_sf, g2_in_sf, g3_in_sf, g4_in_sf,
     file = "outputs/maps_sf.RData")

# neighbour information for grids
save(grid_sizes_deg, grid_sizes_km, 
     g1_nb_q, g1_nb_r, g2_nb_q, g2_nb_r, g3_nb_q, g3_nb_r, g4_nb_q, g4_nb_r,
     file = "outputs/grids_sf_nb.RData")

# full grids (not intersected with country boundary)
save(grid_sizes_deg, grid_sizes_km, 
     g1_sf, g2_sf, g3_sf, g4_sf,
     file = "outputs/grids_sf_full.RData")
