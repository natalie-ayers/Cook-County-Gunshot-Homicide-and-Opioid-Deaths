library(readxl)
library(lubridate)
library(stringi)
library(tidyverse)
library(data.table)
library(sf)

### Read in Data and Format ###

raw_2000to2019 <- read_excel("~/ME_2021/CMX_patho_publichth-2000-2020_01312021.xls", skip = 11)
raw_2000to2019 <- as_tibble(raw_2000to2019)
raw_1984to1999 <- read_excel("~/ME_2021/CMX_patho_publichth-1984-2000_01312021.xls", skip = 11)
raw_1984to1999 <- as_tibble(raw_1984to1999)
raw_2019to2020 <- read_excel("~/ME_2021/CMX_patho_publichth-2019-2020_03062021.xls", skip = 11)
raw_2019to2020 <- as_tibble(raw_2019to2020)



total_pubhth_init <- union(raw_1984to1999, raw_2000to2019)
total_pubhth <- union(total_pubhth_init, raw_2019to2020)
total_pubhth <- total_pubhth %>% rename(
  CASE_NO = `CASE NO`,
  LATINO = `...9`,
  PRIMARY_CAUSE_A = `PRIMARY CAUSE`,
  PRIMARY_CAUSE_B = `b`,
  PRIMARY_CAUSE_C = `...12`,
  SECONDARY_CAUSE = `...13`,
  DATE_OF_DEATH = `DATE OF DEATH`,
  INCIDENT_ADDRESS = `...19`,
  INCIDENT_CITY = `INCIDENT CITY`
) 

# # Confirm no data in columns to remove
#not_na <- total_pubhth %>% filter(!is.na(total_pubhth$...2) | !is.na(total_pubhth$...3) | !is.na(total_pubhth$...14) |
#                                    !is.na(total_pubhth$...16) | !is.na(total_pubhth$...17) |
#                                    !is.na(total_pubhth$...18))
#                           
total_pubhth <- subset(total_pubhth, select = -c(...2, ...3, ...14, ...16, ...17, ...18)) %>%  
  mutate(DATE_OF_DEATH = mdy(DATE_OF_DEATH), PRIMARY_CAUSE_A = toupper(PRIMARY_CAUSE_A), PRIMARY_CAUSE_B = toupper(PRIMARY_CAUSE_B),
         PRIMARY_CAUSE_C = toupper(PRIMARY_CAUSE_C), SECONDARY_CAUSE = toupper(SECONDARY_CAUSE),
         SEX = toupper(SEX), RACE = toupper(RACE), LATINO = toupper(LATINO), INCIDENT_ADDRESS = toupper(INCIDENT_ADDRESS),
         INCIDENT_CITY = toupper(INCIDENT_CITY)) %>% mutate(RACE = as.character(RACE))

pubhth_guns <- filter(total_pubhth, (grepl('GUNSHOT', PRIMARY_CAUSE_A) | grepl('GUNSHOT', PRIMARY_CAUSE_B) |
                                       grepl('GUNSHOT', PRIMARY_CAUSE_C) | grepl('GUNSHOT', SECONDARY_CAUSE))) %>% 
  mutate(RACE = ifelse(RACE=="AM. INDIAN",'NATIVE AMERICAN',
                       ifelse(RACE=='ORIENTAL','ASIAN',
                              ifelse((RACE!='BLACK' & RACE!='WHITE' & RACE!='ASIAN' & RACE!='NATIVE AMERICAN'),'OTHER',RACE))),
         PRIMARY_CAUSE = PRIMARY_CAUSE_A, SECONDARY_CAUSE = paste(ifelse(is.na(PRIMARY_CAUSE_B),'-',PRIMARY_CAUSE_B),
                                                                  ifelse(is.na(PRIMARY_CAUSE_C),'-',PRIMARY_CAUSE_C),
                                                                  ifelse(is.na(SECONDARY_CAUSE),'-',SECONDARY_CAUSE))) %>%
  mutate(SECONDARY_CAUSE = stri_trim_right(SECONDARY_CAUSE, "\\p{L}")) %>% select(-c('PRIMARY_CAUSE_A','PRIMARY_CAUSE_B','PRIMARY_CAUSE_C')) %>%
  mutate(RACE = ifelse(LATINO=='YES', 'LATINO', RACE))

write.csv(pubhth_guns, 'me_cases_1984-2020.csv')


### Attach geocoded locations to Case data:

## Attach manually-identified locations which were only provided a name, not an address (Eg, 'TRINITY HOSPITA')

# Use IPUMS to geocode manually located addresses for each named location
me_addresses_map <- as_tibble(read.csv("~/ME_2021/GunViolence_ME/me_addresses_map.csv"))
addresses_map <- subset(me_addresses_map, select = c(X, RAW, FULL_NAME, ADDRESS, LOC_TYPE, 
                                                     STREET, CITY, STATE, ZIP, STATUS, COUNTY_SYSTEM)) %>%
  filter(!is.na(ZIP))

ipums_addresses <- as_tibble(read.csv("~/ME_2021/GunViolence_ME/me_addresses_geo.csv"))
ipums_addresses <- select(ipums_addresses, c(ID = id, ADDRESS, CITY, STATE, ZIP, LAT = latitude, LON = longitude)) 
                                             
addresses_geo <- left_join(addresses_map, ipums_addresses, by=c('X'='ID'))  %>% 
  select(c(ID = X, RAW, FULL_NAME, ADDRESS = ADDRESS.x, LOC_TYPE, STREET, CITY=CITY.x, STATE=STATE.x, ZIP=ZIP.x, 
           STATUS, LAT, LON))
addresses_geo$JOIN_FIELD <- str_trim(gsub('[[:punct:] ]+',' ',addresses_geo$RAW))
addresses_geo[duplicated(addresses_geo$JOIN_FIELD),]

full_me_cases <- pubhth_guns %>% select(c(CASE_NO, INCIDENT_ADDRESS))
full_me_cases$JOIN_FIELD <- str_trim(gsub('[[:punct:] ]+',' ',full_me_cases$INCIDENT_ADDRESS))
full_me_cases[duplicated(full_me_cases$JOIN_FIELD),]

addresses_geo_join <- left_join(addresses_geo, full_me_cases, by=c('JOIN_FIELD'))
addresses_geo_join[is.na(addresses_geo_join$CASE_NO),]

addresses_geo_join <- addresses_geo_join %>% select(c(CASE_NO, full_address = ADDRESS, CITY, ZIP, lat = LAT, long = LON))
addresses_geo_join$ZIP <- as.character(addresses_geo_join$ZIP)

### Attach manually-identified locations

## First group of manually identified
manual_geos <- read.csv('manually_geoloc.csv') 
manual_geos <- manual_geos %>% select(c('RAW', 'STREET_FIN', 'CITY_FIN', 'COUNTY_FIN', 'STATE_FIN', 'ZIP_FIN',
                                        'GEO_QUALITY_FIN', 'GEO_QUAL_CODE_FIN', 'LAT_FIN', 'LON_FIN')) %>%
  mutate(full_address = paste(manual_geos$STREET_FIN, manual_geos$CITY_FIN, manual_geos$STATE_FIN, manual_geos$ZIP_FIN, sep=", " )) %>% 
  select(c(RAW, full_address, ZIP = 'ZIP_FIN', CITY = 'CITY_FIN', lat = 'LAT_FIN', long = 'LON_FIN'))
manual_geos$JOIN_FIELD <- str_trim(gsub('[[:punct:] ]+',' ',manual_geos$RAW))
manual_geos <- manual_geos %>% mutate(JOIN_FIELD = ifelse(full_address == '13906 S Indiana Ave, Riverdale, IL, 60827', 
                                                          '13906 S INDIANA 2W', JOIN_FIELD))

manual_geos <- manual_geos[!duplicated(manual_geos$JOIN_FIELD),] %>% mutate(CITY = str_to_upper(CITY))

manual_geos_join <- left_join(manual_geos, full_me_cases, by=c('JOIN_FIELD'))
manual_geos_join[is.na(manual_geos_join$CASE_NO),]

# Manually assign records which didn't join:
manual_geos_join <- manual_geos_join %>% mutate(CASE_NO = 
                                                  ifelse(manual_geos_join$full_address == '5212 S Cornell Ave, Chicago, IL, 60615',
                                                'ME2014-01567', ifelse(manual_geos_join$full_address == '235 N Mason Ave, Chicago, IL, 60644',
                                                'ME2014-01569', ifelse(manual_geos_join$full_address == '122 N Wood St, Chicago, IL, 60612',
                                                'ME2015-00590', ifelse(manual_geos_join$full_address == '410 E 72nd St, Chicago, IL, 60619',
                                                'ME2015-01483', ifelse(manual_geos_join$full_address == '2337 W Washington Blvd, Chicago, IL, 60612',
                                                'ME2015-02906', ifelse(manual_geos_join$full_address == '1932 Emerson St, Evanston, IL, 60201',
                                                'ME2015-04017', ifelse(manual_geos_join$full_address == '4428 W Monroe St, Chicago, IL, 60624',
                                                'ME2015-04884', ifelse(manual_geos_join$full_address == '1001 N Lamon Ave, Chicago, IL, 60651',
                                                'ME2015-05537', ifelse(manual_geos_join$full_address == '5400 W 87th St, Burbank, IL, 60459',
                                                'ME2016-00448', ifelse(manual_geos_join$full_address == '411 E 62nd St, Chicago, IL, 60637',
                                                'ME2016-00521', ifelse(manual_geos_join$full_address == '3008 W 60th St, Chicago, IL, 60629',
                                                'ME2016-01327', ifelse(manual_geos_join$full_address == '5632 S Wabash Ave, Chicago, IL, 60637',
                                                'ME2016-02615', ifelse(manual_geos_join$full_address == '2631 S Indiana Ave, Chicago, IL, 60616',
                                                'ME2016-04084', ifelse(manual_geos_join$full_address == '8031 S Essex Ave, Chicago, IL, 60617',
                                                'ME2017-02295', ifelse(manual_geos_join$full_address == '2920 S State St, Chicago, IL, 60616',
                                                'ME2017-02441', ifelse(manual_geos_join$full_address == '1600 S 56th Ct, Cicero, IL, 60804',
                                                'ME2017-03938', ifelse(manual_geos_join$full_address == '2519 W 58th St, Chicago, IL, 60629',
                                                'ME2017-04365', ifelse(manual_geos_join$full_address == '5351 W Iowa St, Chicago, IL, 60651',
                                                'ME2019-00893', ifelse(manual_geos_join$full_address == '6220 N Ridge Blvd, Chicago, IL, 60660',
                                                'ME2019-04694', ifelse(manual_geos_join$full_address == '6359 S King Dr, Chicago, IL, 60637',
                                                'ME2016-05809', ifelse(manual_geos_join$full_address == '1112 Parkwood Dr, Joliet, IL, 60432',
                                                'ME2020-03680', ifelse(manual_geos_join$full_address == '19712 Terrace Ave, Lynwood, IL, 60411',
                                                'ME2020-10055', ifelse(manual_geos_join$full_address == '312 E 53rd St, Chicago, IL, 60615',
                                                'ME2020-14728', ifelse(manual_geos_join$full_address == '8125 W Thomas St, Chicago, IL, 60651',
                                                'ME2020-13620', CASE_NO
                                                )))))))))))))))))))))))))

manual_geos_join <- manual_geos_join %>% filter(!is.na(CASE_NO)) %>% select(-c(RAW, JOIN_FIELD, INCIDENT_ADDRESS))
manual_geos_join$ZIP <- as.character(manual_geos_join$ZIP)

manual_geos_join <- manual_geos_join[!duplicated(manual_geos_join$CASE_NO),]


## Second group of manually identified

manual_geos_2 <- read.csv('locs_to_geo.csv') 
manual_geos_2 <- filter(manual_geos_2, (!is.na(lat) & lat != '')) %>% select(c(CASE_NO, full_address, CITY, ZIP, lat, long)) %>%
  mutate(ZIP = as.character(ZIP))

manual_geos_join <- union(manual_geos_join, manual_geos_2 )



# Use Open Cage and Nominatim Geocoding services to identify most of cases with address provided:
# (see geocode_addresses.R for source code)

oc_nom_locs <- read.csv('oc_nom_locs.csv') %>% select(-c('X'))

locs_to_join <- union(addresses_geo_join, manual_geos_join)
locs_to_join <- union(locs_to_join, oc_nom_locs)

# Determine cases already manually assigned
dup_cases <- locs_to_join[duplicated(locs_to_join$CASE_NO),]$CASE_NO
locs_dup <- filter(locs_to_join, (CASE_NO %in% dup_cases))

# Prefer manually assigned cases to API geoloc'd 

oc_nom_filt <- filter(oc_nom_locs, !(CASE_NO %in% dup_cases))
locs_join <- union(addresses_geo_join, manual_geos_join)
locs_join <- union(locs_join, oc_nom_filt)

# Check for additional dups
dup_cases_2 <- locs_join[duplicated(locs_join$CASE_NO),]$CASE_NO
locs_dup_2 <- filter(locs_join, (CASE_NO %in% dup_cases_2))

# Remove wrong dups
locs_join <- filter(locs_join, !(CASE_NO %in% dup_cases_2 & is.na(lat)))


## Add geo data to me cases

pubhth_guns <- pubhth_guns %>% left_join(locs_join, by=c('CASE_NO')) %>% as.data.table() %>%
  setkey(cols=CASE_NO)


unlocated <- pubhth_guns[is.na(pubhth_guns$lat),]
unlocated_possible <- unlocated %>% filter(!is.na(unlocated$INCIDENT_ADDRESS) & (substr(unlocated$INCIDENT_ADDRESS,1, 4) != 'SCEN') &
                                             (substr(unlocated$INCIDENT_ADDRESS, 1, 1) != 'S') & 
                                             (substr(unlocated$INCIDENT_ADDRESS, 1, 3) != 'UNK') & 
                                             (ifelse(unlocated$DATE_OF_DEATH <= '2015-01-01', 
                                              str_detect(unlocated$INCIDENT_ADDRESS, "^\\d"), TRUE)))
write.csv(unlocated_possible, 'locs_to_geo.csv')




## Attach Chicago neighborhood attribute
# Neighborhood boundaries from: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Neighborhoods/bbvz-uum9

chi_neighborhoods <- st_read('~/ME_2021/Python/chi_neighborhoods.shp', crs=4326)
pubhth_guns$LAT <- pubhth_guns$lat
pubhth_guns$LON <- pubhth_guns$long
guns_geo <- st_as_sf(filter(pubhth_guns, !is.na(lat) & !is.na(long)), coords = c("long", "lat"), crs = 4326)
guns_neighborhoods <- st_join(guns_geo, chi_neighborhoods, join=st_within)

# Confirm no Chicago neighborhoods aren't assigned
filter(guns_neighborhoods, is.na(guns_neighborhoods$pri_neigh) & guns_neighborhoods$CITY == 'CHICAGO')

# Use to identify outlying Chicago points without neighborhood assignments
ggplot() + geom_sf(data=chi_neighborhoods) + geom_sf(data=filter(guns_neighborhoods,is.na(pri_neigh) & CITY=='CHICAGO')) 

guns_neighborhoods <- guns_neighborhoods %>% select(c(CASE_NO, PRI_NEIGHBORHOOD = 'pri_neigh', SEC_NEIGHBORHOOD = 'sec_neigh')) 

# Link community areas to cases
chi_communityareas <- st_read('~/ME_2021/GunViolence_ME/boundaries_community_areas/chi_com_areas.shp', crs=4326)
chi_communityareas <- select(chi_communityareas, c('area_numbe','community','geometry'))
guns_comareas <- st_join(guns_geo, chi_communityareas, join=st_within)
guns_comareas <- as.data.frame(guns_comareas) %>% select(c(CASE_NO),'area_numbe','community') %>%
  rename(comarea_id = 'area_numbe', community_area = 'community')
guns_comareas <- guns_comareas %>% filter(!is.na(community_area))


## Attach Chicago Blockgroup, Neighborhood, Community Area attributes

chi_blockgroups <- st_read('~/ME_2021/GunViolence_ME/blockgroups.shp', crs=4326)
guns_blockgroups <- st_join(guns_geo, chi_blockgroups, join=st_within)
guns_blockgroups <- as.data.frame(guns_blockgroups) %>% select(c(CASE_NO, `Block Group`=block_grou)) %>%
  filter(!is.na(`Block Group`))



pubhth_guns <- left_join(pubhth_guns, guns_blockgroups)
pubhth_guns <- left_join(pubhth_guns, guns_comareas)



guns_fin <- left_join(pubhth_guns, guns_neighborhoods) %>% mutate(MANNER = as.character(MANNER), SEX = as.character(SEX), 
                                                                  RACE = as.character(RACE), LATINO = as.character(LATINO), 
                                                                  SECONDARY_CAUSE = as.character(SECONDARY_CAUSE), 
                                                                  INCIDENT_CITY = as.character(INCIDENT_CITY), 
                                                                  PRIMARY_CAUSE = as.character(PRIMARY_CAUSE))

guns_fin <- guns_fin %>% mutate(CITY = ifelse(is.na(CITY), INCIDENT_CITY, CITY), ADDRESS = full_address) %>% 
  mutate(PRI_NEIGHBORHOOD = ifelse(is.na(PRI_NEIGHBORHOOD), CITY, PRI_NEIGHBORHOOD)) %>% select(-c('lat','long','full_address'))
guns_fin <- guns_fin %>% mutate(CITY=stri_trans_totitle(CITY),ADDRESS=stri_trans_totitle(ADDRESS),
                                PRI_NEIGHBORHOOD=stri_trans_totitle(PRI_NEIGHBORHOOD),
         SEC_NEIGHBORHOOD=stri_trans_totitle(SEC_NEIGHBORHOOD), NAME=stri_trans_totitle(NAME),RACE=ifelse(is.na(RACE),'Other',stri_trans_totitle(RACE)),
         SEX=stri_trans_totitle(SEX),LATINO=stri_trans_totitle(LATINO),PRIMARY_CAUSE=stri_trans_totitle(PRIMARY_CAUSE),
         SECONDARY_CAUSE=stri_trans_totitle(SECONDARY_CAUSE))  %>% subset(select=-c(geometry))

write.csv(guns_fin, "full_geo_data.csv")





