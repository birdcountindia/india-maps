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
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

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
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

dists_sf <- st_read(dsn = dists_path, layer = dists_file) %>% 
  dplyr::select(dtname) %>% 
  rename(DISTRICT.NAME = dtname) %>% 
  # some districts have two different rows (two different polygons) so need to combine
  # them into one polygon
  group_by(DISTRICT.NAME) %>% 
  summarise() %>% # dplyr-sf magic :) 
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# https://gis.stackexchange.com/questions/421651/merging-two-multipolygon-shapefiles-and-removing-one-of-overlapping-polygons-usi



# creating grids ----------------------------------------------------------

# g1

g1_sf <- india_sf %>% 
  st_make_grid(cellsize = grid_sizes_deg[1],
               n = (c(diff(st_bbox(india_sf)[c(1, 3)]), diff(st_bbox(india_sf)[c(2, 4)]))/grid_sizes_deg[1]) %>% 
                 ceiling()) %>% 
  st_as_sf() %>% 
  rename(geometry = x)

# neighbours
# refer this issue https://github.com/r-spatial/sf/issues/234
# https://en.wikipedia.org/wiki/DE-9IM

st_rook <- function(a, b = a) st_relate(a, b, pattern = "F***1****")
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

g1_sf %>% mutate(NB_ROOK = st_rook(.),
                 NB_QUEEN = st_queen(.))


# g2

g2_sf <- india_sf %>% 
  st_make_grid(cellsize = grid_sizes_deg[2],
               n = (c(diff(st_bbox(india_sf)[c(1, 3)]), diff(st_bbox(india_sf)[c(2, 4)]))/grid_sizes_deg[2]) %>% 
                 ceiling()) %>% 
  st_as_sf() %>% 
  rename(geometry = x)

# g3

g3_sf <- india_sf %>% 
  st_make_grid(cellsize = grid_sizes_deg[3],
               n = (c(diff(st_bbox(india_sf)[c(1, 3)]), diff(st_bbox(india_sf)[c(2, 4)]))/grid_sizes_deg[3]) %>% 
                 ceiling()) %>% 
  st_as_sf() %>% 
  rename(geometry = x)

# g4

g4_sf <- india_sf %>% 
  st_make_grid(cellsize = grid_sizes_deg[4],
               n = (c(diff(st_bbox(india_sf)[c(1, 3)]), diff(st_bbox(india_sf)[c(2, 4)]))/grid_sizes_deg[4]) %>% 
                 ceiling()) %>% 
  st_as_sf() %>% 
  rename(geometry = x)


# intersecting grids with admin boundaries --------------------------------



# getting total number of cells ------------------------------------------


totcells_india <- g1cells_sf %>% 
  st_drop_geometry() %>% 
  dplyr::summarise(TOT.CELLS = n_distinct(CELL.ID))


states_cells <- states_sf %>% 
  st_set_crs(st_crs(g1cells_sf)) %>% 
  st_join(g1cells_sf) %>% 
  st_drop_geometry()

totcells_states <- states_cells %>% 
  group_by(STATE) %>% 
  dplyr::summarise(TOT.CELLS = n_distinct(CELL.ID))


districts_cells <- districts_sf %>% 
  st_set_crs(st_crs(g1cells_sf)) %>% 
  st_join(g1cells_sf) %>% 
  st_drop_geometry() 
# losing some districts and grid cells, but doesn't matter.

totcells_dist <- districts_cells %>% 
  group_by(DISTRICT.NAME) %>% 
  dplyr::summarise(TOT.CELLS = n_distinct(CELL.ID))