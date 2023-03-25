library(geodata)
library(sf)
library(tidyverse)


bt_sf <- geodata::gadm(country = "BTN", path = "outputs/temp_outputs/", level = 0) %>% 
  st_as_sf() %>% 
  dplyr::select(COUNTRY, geometry) %>% 
  st_set_crs("OGC:CRS84")

np_sf <- geodata::gadm(country = "NPL", path = "outputs/temp_outputs/", level = 0) %>% 
  st_as_sf() %>% 
  dplyr::select(COUNTRY, geometry) %>% 
  st_set_crs("OGC:CRS84")

bt_states_sf <- geodata::gadm(country = "BTN", path = "outputs/temp_outputs/", level = 1) %>% 
  st_as_sf() %>% 
  rename(STATE.NAME = NAME_1) %>% 
  dplyr::select(COUNTRY, STATE.NAME, geometry) %>% 
  st_set_crs("OGC:CRS84") %>% 
  rename(STATE.GEOM = geometry)

bt_dists_sf <- geodata::gadm(country = "BTN", path = "outputs/temp_outputs/", level = 2) %>% 
  st_as_sf() %>% 
  rename(DISTRICT.NAME = NAME_2) %>% 
  dplyr::select(COUNTRY, DISTRICT.NAME, geometry) %>% 
  st_set_crs("OGC:CRS84") %>% 
  rename(DISTRICT.GEOM = geometry)

np_states_sf <- geodata::gadm(country = "NPL", path = "outputs/temp_outputs/", level = 1) %>% 
  st_as_sf() %>% 
  rename(STATE.NAME = NAME_1) %>% 
  dplyr::select(COUNTRY, STATE.NAME, geometry) %>% 
  st_set_crs("OGC:CRS84") %>% 
  rename(STATE.GEOM = geometry)

np_dists_sf <- geodata::gadm(country = "NPL", path = "outputs/temp_outputs/", level = 2) %>% 
  st_as_sf() %>% 
  rename(DISTRICT.NAME = NAME_2) %>% 
  dplyr::select(COUNTRY, DISTRICT.NAME, geometry) %>% 
  st_set_crs("OGC:CRS84") %>% 
  rename(DISTRICT.GEOM = geometry)


# saving maps
save(bt_sf, bt_states_sf, bt_dists_sf,
     np_sf, np_states_sf, np_dists_sf,
     file = "outputs/maps_BT_NP_sf.RData")