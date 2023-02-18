library(stars)
library(tidyverse)
library(rgee)
library(sf)
library(innovar)

ee_Initialize()
data("Peru")

region <- Peru %>%
  st_bbox() %>%
  st_as_sfc() %>%
  sf_as_ee()

start_temp_year <-
  ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  select(c("tmmn","tmmx"))$
  filter(ee$Filter$calendarRange(2021,2021,"year"))$
  mean()$
  multiply(0.1) %>%
  ee$Image() %>%
  ee_as_stars(
    region = region,
    scale = 1000
    )

start_temp_march <-
  ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  select(c("tmmn","tmmx"))$
  filterDate("2021-03-01","2021-03-31")$
  mean()$
  multiply(0.1) %>%
  ee$Image() %>%
  ee_as_stars(
    region = region,
    scale = 1000
  )

gpkg_original <- st_read("sources/IE_P.gpkg")

names(gpkg_original) <- names(gpkg_original) %>% 
  str_to_lower()

gpkg_code <- gpkg_original %>%
  select(codlocal)

temp_year <- st_extract(
  start_temp_year,gpkg_code) %>%
  st_as_sf() %>%
  st_join(gpkg_code) %>%
  mutate(
    tmmn = round(tmmn,4),
    tmmx = round(tmmx,4),
    tmean = (tmmn + tmmx)/2 %>% round(.,4)) %>%
  st_set_geometry(NULL)

temp_march <- st_extract(
  start_temp_march,gpkg_code) %>%
  st_as_sf() %>%
  st_join(gpkg_code) %>%
  mutate(
    tmmn = round(tmmn,4),
    tmmx = round(tmmx,4),
    tmean_march = (tmmn + tmmx)/2 %>% round(.,4)) %>%
  rename(
    tmmn_march = tmmn,
    tmmx_march = tmmx
    ) %>%
  st_set_geometry(NULL)

db_temp <- temp_year %>%
  inner_join(y = temp_march,by = "codlocal") %>%
  inner_join(x = gpkg_original, by = "codlocal") %>%
  st_join(Peru) %>%
  select(17:24,1:16,geom)

dir.create("ouput")
write_sf(db_temp,"ouput/db_temp_2021.gpkg")