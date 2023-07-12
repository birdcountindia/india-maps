library(sf)
library(tidyverse)
library(glue)

# BHUTAN
# (probable) source: https://data.humdata.org/dataset/cod-ab-btn?
# Country > dzongkhag district > gewog
bt_sf <- st_read(dsn = "data/bt_2020", layer = "bt_2020_adm0") %>% 
  dplyr::select(ADM0_EN, geometry) %>% 
  rename(COUNTRY = ADM0_EN) %>% 
  st_set_crs("OGC:CRS84")

bt_states_sf <- st_read(dsn = "data/bt_2020", layer = "bt_2020_adm1") %>% 
  dplyr::select(ADM1_EN, ADM0_EN, geometry) %>% 
  rename(STATE.NAME = ADM1_EN,
         COUNTRY = ADM0_EN,
         STATE.GEOM = geometry) %>% 
  st_set_crs("OGC:CRS84")

bt_dists_sf <- st_read(dsn = "data/bt_2020", layer = "bt_2020_adm2") %>% 
  dplyr::select(ADM2_EN, ADM1_EN, ADM0_EN, geometry) %>% 
  rename(DISTRICT.NAME = ADM2_EN,
         STATE.NAME = ADM1_EN,
         COUNTRY = ADM0_EN,
         DISTRICT.GEOM = geometry) %>% 
  st_set_crs("OGC:CRS84")


# NEPAL
# source: https://opendatanepal.com/dataset/new-political-and-administrative-boundaries-shapefile-of-nepal
# Country > province > district
np_sf0 <- st_read(dsn = "data/np_2021", layer = "np_2021")

np_sf <- np_sf0 %>% 
  mutate(COUNTRY = "Nepal") %>% 
  group_by(COUNTRY) %>% 
  dplyr::summarise() %>% 
  st_transform("OGC:CRS84")

np_states_sf <- np_sf0 %>% 
  mutate(COUNTRY = "Nepal") %>% 
  group_by(COUNTRY, Province) %>% 
  dplyr::summarise() %>% 
  rename(STATE.NAME = Province,
         STATE.GEOM = geometry) %>% 
  st_transform("OGC:CRS84") %>% 
  # adding "Province" to names
  mutate(STATE.NAME = case_when(STATE.NAME %in% c(1, 2, 5) ~ glue("Province {STATE.NAME}"),
                                TRUE ~ STATE.NAME))

np_dists_sf <- np_sf0 %>% 
  mutate(COUNTRY = "Nepal") %>% 
  group_by(COUNTRY, Province, DISTRICT) %>% 
  dplyr::summarise() %>% 
  rename(STATE.NAME = Province,
         DISTRICT.NAME = DISTRICT,
         DISTRICT.GEOM = geometry) %>% 
  st_transform("OGC:CRS84") %>% 
  # adding "Province" to names
  mutate(STATE.NAME = case_when(STATE.NAME %in% c(1, 2, 5) ~ glue("Province {STATE.NAME}"),
                                TRUE ~ STATE.NAME))


# saving maps
save(bt_sf, bt_states_sf, bt_dists_sf,
     np_sf, np_states_sf, np_dists_sf,
     file = "outputs/maps_BT_NP_sf.RData")
