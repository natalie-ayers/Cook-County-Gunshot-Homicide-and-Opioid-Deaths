library(tidyverse)
library(data.table)
library(sf)
library(lubridate)


me_opioid <- as_tibble(read.csv("~/ME_2021/GunViolence_ME/Medical_Examiner_Case_Archive.csv"))


me_opioid <- me_opioid %>% mutate(Race = ifelse(Race=="Am. Indian",'Native American',
                            ifelse((Race!='Black' & Race!='White' & Race!='Asian' & Race!='Native American'),
                                   'Other',Race)))  %>%
  select(-c('Primary.Cause.Line.A','Primary.Cause.Line.B','Primary.Cause.Line.C', 'Gun.Related',
            'Opioid.Related', 'Commissioner.District', 'OBJECTID')) %>%
  mutate(Race = ifelse(Latino, 'Latino', Race)) %>% filter(Case.Number != '')

chi_communityareas <- st_read('~/ME_2021/GunViolence_ME/boundaries_community_areas/chi_com_areas.shp', crs=4326)
chi_communityareas <- chi_communityareas %>% select(c('area_numbe','community', 'geometry'))

me_opioid_geo <- me_opioid %>% mutate(LAT = latitude, LON = longitude)
me_opioid_geo <- filter(me_opioid_geo, (!is.na(me_opioid_geo$LAT) & !is.na(me_opioid_geo$LON)))
me_opioid_geo <- st_as_sf(me_opioid_geo, 
                                  coords = c("LON","LAT"), 
                                  crs = 4326)
me_opioid_geo <- st_join(me_opioid_geo, chi_communityareas, join=st_within)
me_opioid_geo <- as.data.frame(me_opioid_geo) %>% 
  select(-c('geometry'))

me_opioid_nogeo <- filter(me_opioid, (is.na(latitude) | is.na(longitude)))
me_opioid_nogeo$community <- NA
me_opioid_nogeo$area_numbe <- NA
me_opioid_ca <- union(me_opioid_geo, me_opioid_nogeo)
me_opioid_ca <- me_opioid_ca %>% rename(community_area_id = 'area_numbe', community_area = 'community')

write.csv(me_opioid_ca, 'me_opioid_cases.csv')









