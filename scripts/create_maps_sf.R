# saves RData files "maps_sf.RData", "grids_sf_nb.RData", "grids_sf_full.RData"


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

sf_use_s2(FALSE)

india_sf <- st_read(dsn = country_path, layer = country_file) %>% mutate(DISTRICT = NULL) %>% 
  st_set_crs("OGC:CRS84")

states_sf <- st_read(dsn = states_path, layer = states_file) %>% 
  dplyr::select(stname, geometry) %>% 
  magrittr::set_colnames(c("STATE.NAME", "geometry")) %>% 
  mutate(STATE.NAME = str_to_title(STATE.NAME)) %>% 
  # replacing ampersand with "and"
  mutate(STATE.NAME = str_replace(STATE.NAME, "&", "and")) %>% 
  # corrections for Madhu's SPDF values
  mutate(STATE.NAME = case_when(STATE.NAME == "Dadra and Nagar Have" ~ "Dadra and Nagar Haveli",
                                STATE.NAME == "Andaman and Nicobar" ~ "Andaman and Nicobar Islands",
                                TRUE ~ STATE.NAME)) %>% 
  st_set_crs("OGC:CRS84") %>% 
  rename(STATE.GEOM = geometry)

dists_sf <- st_read(dsn = dists_path, layer = dists_file) %>% 
  dplyr::select(dtname, stname) %>% 
  rename(DISTRICT.NAME = dtname, 
         STATE.NAME = stname,
         DISTRICT.GEOM = geometry) %>% 
  # some districts have two different rows (two different polygons) so need to combine
  # them into one polygon
  group_by(STATE.NAME, DISTRICT.NAME) %>% 
  summarise() %>% # dplyr-sf magic :) 
  ungroup() %>% 
  mutate(STATE.NAME = str_to_title(STATE.NAME)) %>% 
  # replacing ampersand with "and"
  mutate(STATE.NAME = str_replace(STATE.NAME, "&", "and"),
         DISTRICT.NAME = str_replace(DISTRICT.NAME, "&", "and")) %>% 
  # corrections for Madhu's SPDF values
  mutate(STATE.NAME = case_when(STATE.NAME == "Dadra and Nagar Have" ~ "Dadra and Nagar Haveli",
                                STATE.NAME == "Andaman and Nicobar" ~ "Andaman and Nicobar Islands",
                                TRUE ~ STATE.NAME),
         DISTRICT.NAME = case_when(DISTRICT.NAME == "Sivaganga" ~ "Sivagangai",
                                   DISTRICT.NAME == "Thoothukkudi" ~ "Thoothukudi",
                                   DISTRICT.NAME == "Raigarh" ~ "Raigad",
                                   DISTRICT.NAME == "Nicobars" ~ "Nicobar",
                                   DISTRICT.NAME == "Darjiling" ~ "Darjeeling",
                                   DISTRICT.NAME == "Ahmadabad" ~ "Ahmedabad",
                                   DISTRICT.NAME == "West District" ~ "West Sikkim",
                                   DISTRICT.NAME == "Jagatsinghapur" ~ "Jagatsinghpur",
                                   DISTRICT.NAME == "Mahrajganj" ~ "Maharajganj",
                                   str_detect(DISTRICT.NAME, "North Twenty Four") ~ "North 24 Parganas",
                                   str_detect(DISTRICT.NAME, "South Twenty Four") ~ "South 24 Parganas",
                                   str_detect(DISTRICT.NAME, "Sri Potti Sriramulu") ~ "	
Sri Potti Sriramulu Nellore",
                                   TRUE ~ DISTRICT.NAME)) %>% 
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
  rename(GEOM.G1 = x) %>% 
  # cell IDs
  rownames_to_column("GRID.G1")

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
  rename(GEOM.G2 = x) %>% 
  # cell IDs
  rownames_to_column("GRID.G2")

g2_nb_r <- poly2nb(g2_sf, queen = FALSE)
g2_nb_q <- poly2nb(g2_sf)


# g3
res <- 3
cs <- grid_sizes_deg[res]
n <- (c(diff(st_bbox(india_sf)[c(1, 3)]), diff(st_bbox(india_sf)[c(2, 4)]))/cs) %>% ceiling()

g3_sf <- india_sf %>% 
  st_make_grid(cellsize = cs, n = n) %>% 
  st_as_sf() %>% 
  rename(GEOM.G3 = x) %>% 
  # cell IDs
  rownames_to_column("GRID.G3")

g3_nb_r <- poly2nb(g3_sf, queen = FALSE)
g3_nb_q <- poly2nb(g3_sf)


# g4
res <- 4
cs <- grid_sizes_deg[res]
n <- (c(diff(st_bbox(india_sf)[c(1, 3)]), diff(st_bbox(india_sf)[c(2, 4)]))/cs) %>% ceiling()

g4_sf <- india_sf %>% 
  st_make_grid(cellsize = cs, n = n) %>% 
  st_as_sf() %>% 
  rename(GEOM.G4 = x) %>% 
  # cell IDs
  rownames_to_column("GRID.G4")

g4_nb_r <- poly2nb(g4_sf, queen = FALSE)
g4_nb_q <- poly2nb(g4_sf)


rm(cs, n)



# intersecting grids with admin boundaries and calculating areas, totcells -----------

# already in sq.m because switched off S2 so using planar
india_sf <- india_sf %>% mutate(AREA = units::set_units(round(st_area(geometry)), "km2"))
states_sf <- states_sf %>% mutate(AREA = units::set_units(round(st_area(STATE.GEOM)), "km2"))
dists_sf <- dists_sf %>% mutate(AREA = units::set_units(round(st_area(DISTRICT.GEOM)), "km2"))


g1_in_sf <- st_intersection(g1_sf, india_sf) %>% 
  mutate(AREA.G1 = units::set_units(round(st_area(GEOM.G1)), "km2"),
         TOT.G1 = n_distinct(GRID.G1))
g2_in_sf <- st_intersection(g2_sf, india_sf) %>% 
  mutate(AREA.G2 = units::set_units(round(st_area(GEOM.G2)), "km2"),
         TOT.G2 = n_distinct(GRID.G2))
g3_in_sf <- st_intersection(g3_sf, india_sf) %>% 
  mutate(AREA.G3 = units::set_units(round(st_area(GEOM.G3)), "km2"),
         TOT.G3 = n_distinct(GRID.G3))
g4_in_sf <- st_intersection(g4_sf, india_sf) %>% 
  mutate(AREA.G4 = units::set_units(round(st_area(GEOM.G4)), "km2"),
         TOT.G4 = n_distinct(GRID.G4))



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
