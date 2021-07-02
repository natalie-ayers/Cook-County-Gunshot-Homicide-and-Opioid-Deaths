library("RSocrata")
library(sf)
library(lubridate)
library(rjson)
library(data.table)
library(tidyverse)
library(readxl)

crimes_01_present <- read.socrata(
  "https://data.cityofchicago.org/resource/ijzp-q8t2.json"
)

crimes_01_present <- as.data.frame(crimes_01_present)
crimes_01_present$month <- floor_date(crimes_01_present$date, "month")

# Assign zip codes to crime records
crimes_01_present_geo <- filter(crimes_01_present, (!is.na(crimes_01_present$latitude) & !is.na(crimes_01_present$longitude)))
crimes_01_present_geo <- st_as_sf(crimes_01_present_geo, 
                                  coords = c("longitude","latitude"), 
                                  crs = 4326)
chi_zips <- st_read('~/ME_2021/GunViolence_ME/chi_zips.shp', crs=4326)
crimes_01_present_geo <- st_join(crimes_01_present_geo, chi_zips, join=st_within)
crimes_01_present_geo <- as.data.frame(crimes_01_present_geo) %>% 
  select(-c('objectid','shape_area','shape_len','geometry'))

crimes_01_present_nogeo <- filter(crimes_01_present, (is.na(latitude) | is.na(longitude))) %>% 
  select(-c('latitude','longitude'))
crimes_01_present_nogeo$zip <- NA
crimes_01_present_zip <- union(crimes_01_present_geo, crimes_01_present_nogeo)


# Map crimes to categories to reduce size of dataset
crime_cat <- read_excel("~/ME_2021/GunViolence_ME/crime_categories_map.xlsx")
crimes_01_present_zip <- merge(crimes_01_present_zip, crime_cat, by.x='primary_type',by.y='PRIMARY_TYPE')

crimes_01_present_agg <- aggregate(crimes_01_present_zip$id, 
          by=list(crimes_01_present_zip$month, crimes_01_present_zip$CRIME_CATEGORY, 
                  crimes_01_present_zip$arrest, crimes_01_present_zip$community_area, crimes_01_present_zip$zip,
                   crimes_01_present_zip$year), 
          FUN=length) %>%
  rename(count_of_crimes = 'x', month = 'Group.1', crime_category = 'Group.2', arrest = 'Group.3', 
         community_area = 'Group.4', zip = 'Group.5', year = 'Group.6')

json_crimes <- toJSON(crimes_01_present_agg)
write(json_crimes, "chi_crimes_monthly.json")


crimes_01_present_agg <- fromJSON(file='chi_crimes_monthly.json') %>% as.data.frame()



chi_neighborhoods <- st_read('~/ME_2021/Python/chi_neighborhoods.shp', crs=4326)
chi_blockgroups <- st_read('~/ME_2021/GunViolence_ME/blockgroups.shp', crs=4326)

chi_communityareas <- st_read('~/ME_2021/GunViolence_ME/boundaries_community_areas/chi_com_areas.shp', crs=4326)
chi_communityareas <- as.data.frame(chi_communityareas) %>% 
  select(c('area_numbe','community'))


# Join community area names to json_crimes
crimes_01_present_agg <- merge(crimes_01_present_agg, chi_communityareas, by.x='community_area', by.y='area_numbe', all.x=TRUE)
crimes_01_present_agg$month <- as_datetime(crimes_01_present_agg$month)


write.csv(crimes_01_present_agg, 'chi_crimes_monthly.csv')



pubhth_guns$LATITUDE <- pubhth_guns$lat
pubhth_guns$LONGITUDE <- pubhth_guns$long

guns_geo <- st_as_sf(filter(pubhth_guns, !is.na(lat) & !is.na(long)), coords = c("long", "lat"), crs = 4326)

guns_neighborhoods <- st_join(guns_geo, chi_neighborhoods, join=st_within)




