library(tidyverse)
library(sf)
library(spdep)
library(glue)
library(tictoc)

load("outputs/maps_sf.RData")
sf_use_s2(FALSE)
states_sf <- states_sf %>% 
  dplyr::select(-AREA) %>% 
  st_make_valid()

# intersecting grids with state boundaries and calculating areas, totcells

g1_st_sf <- map(states_sf$STATE.NAME, ~ {
  
  tic(glue("Grid cells intersected with {.x}"))
  
  data_new <- st_intersection(g1_in_sf, 
                              states_sf %>% filter(STATE.NAME == .x)) %>% 
    mutate(AREA.G1 = units::set_units(round(st_area(GEOM.G1)), "km2"),
           TOT.G1 = n_distinct(GRID.G1)) %>% 
    dplyr::select(-AREA)
  
  toc()
  
  return(data_new)
  
}) %>% 
  list_rbind()

# # check
# g2_st_sf %>%
#   filter(STATE.NAME == "Goa") %>%
#   ggplot() +
#   geom_sf(aes(geometry = GEOM.G1))


g2_st_sf <- map(states_sf$STATE.NAME, ~ {
  
  tic(glue("Grid cells intersected with {.x}"))
  
  data_new <- st_intersection(g2_in_sf, 
                              states_sf %>% filter(STATE.NAME == .x)) %>% 
    mutate(AREA.G2 = units::set_units(round(st_area(GEOM.G2)), "km2"),
           TOT.G2 = n_distinct(GRID.G2)) %>% 
    dplyr::select(-AREA)
  
  toc()
  
  return(data_new)
  
}) %>% 
  list_rbind()


g3_st_sf <- map(states_sf$STATE.NAME, ~ {
  
  tic(glue("Grid cells intersected with {.x}"))
  
  data_new <- st_intersection(g3_in_sf, 
                              states_sf %>% filter(STATE.NAME == .x)) %>% 
    mutate(AREA.G3 = units::set_units(round(st_area(GEOM.G3)), "km2"),
           TOT.G3 = n_distinct(GRID.G3)) %>% 
    dplyr::select(-AREA)
  
  toc()
  
  return(data_new)
  
}) %>% 
  list_rbind()


g4_st_sf <- map(states_sf$STATE.NAME, ~ {
  
  tic(glue("Grid cells intersected with {.x}"))
  
  data_new <- st_intersection(g4_in_sf, 
                              states_sf %>% filter(STATE.NAME == .x)) %>% 
    mutate(AREA.G4 = units::set_units(round(st_area(GEOM.G4)), "km2"),
           TOT.G4 = n_distinct(GRID.G4)) %>% 
    dplyr::select(-AREA)
  
  toc()
  
  return(data_new)
  
}) %>% 
  list_rbind()


# writing
save(g1_st_sf, g2_st_sf, g3_st_sf, g4_st_sf,
     file = "outputs/grids_st_sf.RData")
