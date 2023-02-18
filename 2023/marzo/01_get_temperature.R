library(stars)
library(tidyverse)
library(rgee)
library(sf)

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
    region = 1000
  )

gpkg_original <- st_read("sources/IE_P.gpkg")
gpkg_code <- gpkg_original %>%
  select(CODLOCAL)

temp_year <- st_extract(
  start_temp_year,gpkg_code) %>%
  st_as_sf() %>%
  st_join(gpkg_code) %>%
  mutate(tmean = (tmmn + tmmx)/2) %>%
  st_set_geometry(NULL)

temp_march <- st_extract(
  start_temp_march,gpkg_code) %>%
  st_as_sf() %>%
  st_join(gpkg_code) %>%
  mutate(tmean_march = (tmmn + tmmx)/2) %>%
  rename(
    tmmn_march = tmmn,
    tmmx_march = tmmx
    ) %>%
  st_set_geometry(NULL)


db_temp <- temp_year %>%
  inner_join(y = temp_march,by = "CODLOCAL") %>%
  inner_join(x = ., y = gpkg, by = "CODLOCAL") %>%
  select(3,8:16,1:2,4,5:7)
dir.create("ouput")
write_sf(db_temp,"ouput/db_temp_2021.gpkg")
