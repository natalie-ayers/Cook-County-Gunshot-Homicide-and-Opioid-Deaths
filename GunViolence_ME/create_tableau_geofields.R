library(tidyverse)
library(data.table)
library(sf)
library(lwgeom)


chi_neighborhoods <- st_read('~/ME_2021/Python/chi_neighborhoods.shp', crs=4326)
plot(chi_neighborhoods)

chi_blockgroups <- st_read('~/ME_2021/GunViolence_ME/blockgroups.shp', crs=4326)
plot(chi_blockgroups)

chi_zips <- st_read('~/ME_2021/GunViolence_ME/chi_zips.shp', crs=4326)
plot(chi_zips)

bg_neigh <- st_join(chi_blockgroups, chi_neighborhoods, join=st_intersects, left=TRUE, largest=TRUE)

ggplot(data = bg_neigh) +
  geom_sf() +
  geom_sf(data=chi_blockgroups, color='yellow', fill='blue') +
  geom_sf(data = chi_neighborhoods, color='red', fill=NA)


chi_blockgroups$bg_cent <- st_centroid(chi_blockgroups$geometry)
bg_zip <- st_join(chi_blockgroups, chi_zips, join=st_intersects, left=TRUE, largest=TRUE)

plot(bg_zip)
ggplot(data = bg_zip) +
  geom_sf() +
  geom_sf(data=chi_blockgroups, color='yellow', fill='blue') +
  geom_sf(data = chi_zips, color='red', fill=NA)

join_fields <- as.data.frame(bg_neigh) %>% select(c('block_grou','pri_neigh')) %>% 
  rename(`Block Group`=block_grou, `Neighborhood`=pri_neigh)

tableau_blockgroups <- as.data.frame(bg_zip) %>% select(c('zip','block_grou','bg_cent')) %>% 
  mutate(Longitude = unlist(map(bg_cent,1)), Latitude = unlist(map(bg_cent,2))) %>%
  select(`ZIP Code/Postcode`=zip, `Block Group`=block_grou,Latitude,Longitude) %>%
  mutate(`Country (Name)` = 'U.S.A.')
tableau_blockgroups <- inner_join(tableau_blockgroups, join_fields)

write.csv(tableau_blockgroups, 'tableau_blockgroups.csv')
