library(geodata)
library(sf)
library(tidyverse)


bt_sf <- geodata::gadm(country = "BTN", path = "outputs/temp_outputs/", level = 0) %>% 
  st_as_sf() %>% 
  dplyr::select(COUNTRY, geometry)

np_sf <- geodata::gadm(country = "NPL", path = "outputs/temp_outputs/", level = 0) %>% 
  st_as_sf() %>% 
  dplyr::select(COUNTRY, geometry)

bt_states_sf <- geodata::gadm(country = "BTN", path = "outputs/temp_outputs/", level = 1) %>% 
  st_as_sf() %>% 
  rename(STATE.NAME = NAME_1) %>% 
  dplyr::select(COUNTRY, STATE.NAME, geometry)

bt_dists_sf <- geodata::gadm(country = "BTN", path = "outputs/temp_outputs/", level = 2) %>% 
  st_as_sf() %>% 
  rename(DISTRICT.NAME = NAME_2) %>% 
  dplyr::select(COUNTRY, DISTRICT.NAME, geometry)

np_states_sf <- geodata::gadm(country = "NPL", path = "outputs/temp_outputs/", level = 1) %>% 
  st_as_sf() %>% 
  rename(STATE.NAME = NAME_1) %>% 
  dplyr::select(COUNTRY, STATE.NAME, geometry)

np_dists_sf <- geodata::gadm(country = "NPL", path = "outputs/temp_outputs/", level = 2) %>% 
  st_as_sf() %>% 
  rename(DISTRICT.NAME = NAME_2) %>% 
  dplyr::select(COUNTRY, DISTRICT.NAME, geometry)


# saving maps
save(bt_sf, bt_states_sf, bt_dists_sf,
     np_sf, np_states_sf, np_dists_sf,
     file = "outputs/maps_BT_NP_sf.RData")