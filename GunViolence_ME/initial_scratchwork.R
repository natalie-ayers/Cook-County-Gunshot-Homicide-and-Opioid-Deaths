library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)
library(sf)
library(mapview)
library(ggplot2)
library(data.table)
library(rjson)
library(purrr)
library(jsonlite)


### Read in Data and Format ###

raw_2000to2020 <- read_excel("~/ME_2021/CMX_patho_publichth-2000-2020_01312021.xls", skip = 11)
raw_2000to2020 <- as_tibble(raw_2000to2020)
raw_1984to2020 <- read_excel("~/ME_2021/CMX_patho_publichth-1984-2000_01312021.xls", skip = 11)
raw_1984to2020 <- as_tibble(raw_1984to2020)

total_pubhth <- union(raw_1984to2020, raw_2000to2020)
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
# not_na <- total_pubhth %>% filter(!is.na(total_pubhth$...2) | !is.na(total_pubhth$...3) | !is.na(total_pubhth$...14) |
#                                     !is.na(total_pubhth$...16) | !is.na(total_pubhth$...17) | 
#                                     !is.na(total_pubhth$...18)) 
#                           
total_pubhth <- subset(total_pubhth, select = -c(...2, ...3, ...14, ...16, ...17, ...18)) %>%  
  mutate(DATE_OF_DEATH = mdy(DATE_OF_DEATH), PRIMARY_CAUSE_A = toupper(PRIMARY_CAUSE_A), PRIMARY_CAUSE_B = toupper(PRIMARY_CAUSE_B),
         PRIMARY_CAUSE_C = toupper(PRIMARY_CAUSE_C), SECONDARY_CAUSE = toupper(SECONDARY_CAUSE),
         SEX = toupper(SEX), RACE = toupper(RACE), LATINO = toupper(LATINO), INCIDENT_ADDRESS = toupper(INCIDENT_ADDRESS),
         INCIDENT_CITY = toupper(INCIDENT_CITY))

pubhth_guns <- filter(total_pubhth, (grepl('GUNSHOT', PRIMARY_CAUSE_A) | grepl('GUNSHOT', PRIMARY_CAUSE_B) |
                                       grepl('GUNSHOT', PRIMARY_CAUSE_C) | grepl('GUNSHOT', SECONDARY_CAUSE))) %>% 
                      mutate(MANNER = factor(MANNER), SEX = factor(SEX), RACE = factor(RACE), LATINO = factor(LATINO),
                             PRIMARY_CAUSE_A = factor(PRIMARY_CAUSE_A), PRIMARY_CAUSE_B = factor(PRIMARY_CAUSE_B),
                             PRIMARY_CAUSE_C = factor(PRIMARY_CAUSE_C), SECONDARY_CAUSE = factor(SECONDARY_CAUSE),
                             INCIDENT_CITY = factor(INCIDENT_CITY))

### Only run if need to add new locations ###
me_addresses_map <- as_tibble(read.csv("~/ME_2021/GunViolence_ME/me_addresses_map.csv"))
addresses_map <- subset(me_addresses_map, select = c(X, RAW, FULL_NAME, ADDRESS, LOC_TYPE, 
                                  STREET_1, STREET_2, CITY, STATE, ZIP, STATUS, COUNTY_SYSTEM))

ipums_addresses <- as_tibble(read.csv("~/ME_2021/GunViolence_ME/me_addresses_geo.csv"))
ipums_addresses <- select(ipums_addresses, c(ID = id, ADDRESS, CITY, STATE, ZIP, LAT = latitude, LON = longitude, 
                                                      TRACT = TRACTA, PROP_UNEMP, PROP_POVERTY, MED_INCOME, INCOME_INEQ, PROP_SINGLE_WOMAN,
                                                      PROP_OWN_OCC, PROP_AF_AM, PROP_HS, PERS_SQ_KILO, UNITS_SQ_KILO))

addresses_geo <- left_join(addresses_map, ipums_addresses, by=c('X'='ID'))  %>% 
  select(c(ID = X, RAW, FULL_NAME, ADDRESS = ADDRESS.x, LOC_TYPE, STREET_1, STREET_2, CITY=CITY.x, STATE=STATE.x, ZIP=ZIP.x, 
                  STATUS, LAT, LON, TRACT, PROP_UNEMP, PROP_POVERTY, MED_INCOME, INCOME_INEQ, PROP_SINGLE_WOMAN,
                  PROP_OWN_OCC, PROP_AF_AM, PROP_HS, PERS_SQ_KILO, UNITS_SQ_KILO))


pubhth_guns_add <- pubhth_guns %>% left_join(addresses_geo, by=c('INCIDENT_ADDRESS'='RAW')) %>%  mutate(LOC_TYPE = factor(LOC_TYPE),
  CITY = factor(CITY), STATE = factor(STATE), ZIP = factor(ZIP), STATUS = factor(STATUS), TRACT = factor(TRACT)) %>% as.data.table() %>%
  setkey(cols=CASE_NO)
pubhth_guns_add <- pubhth_guns_add %>% left_join(as.data.frame(table(pubhth_guns_add$ADDRESS, dnn = 'ADDRESS')), by=c('ADDRESS' = 'ADDRESS')) %>%
  rename(CNT_AT_LOC = Freq) 
pubhth_guns_add <- pubhth_guns_add %>% mutate(CNT_FINAL = ifelse((is.na(ADDRESS) | ADDRESS == ''), -1, CNT_AT_LOC))



json_addresses <- fromJSON(file = "~/ME_2021/GunViolence_ME/json_addresses.json")

json_df <- data.frame(HASH_KEY = character(),
                      RAW = character(),
                      STREET = character(),
                      CITY = character(),
                      COUNTY = character(),
                      STATE = character(),
                      ZIP = numeric(),
                      GEO_QUALITY = character(),
                      GEO_QUAL_CODE = character(),
                      LAT = numeric(),
                      LON = numeric(),
                      STREET_2 = character(),
                      CITY_2 = character(),
                      COUNTY_2 = character(),
                      STATE_2 = character(),
                      ZIP_2 = numeric(),
                      GEO_QUALITY_2 = character(),
                      GEO_QUAL_CODE_2 = character(),
                      LAT_2 = numeric(),
                      LON_2 = numeric(),
                      STREET_3 = character(),
                      CITY_3 = character(),
                      COUNTY_3 = character(),
                      STATE_3 = character(),
                      ZIP_3 = numeric(),
                      GEO_QUALITY_3 = character(),
                      GEO_QUAL_CODE_3 = character(),
                      LAT_3 = numeric(),
                      LON_3 = numeric(),
                      stringsAsFactors = FALSE)

for(i in seq(1, length(json_addresses))){
  json_df[i, 1] <- names(json_addresses[i])
  json_df[i, 2] <- json_addresses[[i]]$results[[1]]$providedLocation$street
  
  # Account for cases when < 3 results returned from MapQuest API
  for(j in seq(1,length(json_addresses[[i]]$results[[1]]$locations))){
    scale = (j - 1) * 9
    #print(i, j)
    json_df[i, scale + 3] <- json_addresses[[i]]$results[[1]]$locations[[j]]$street
    json_df[i, scale + 4] <- json_addresses[[i]]$results[[1]]$locations[[j]]$adminArea5
    json_df[i, scale + 5] <- json_addresses[[i]]$results[[1]]$locations[[j]]$adminArea4
    json_df[i, scale + 6] <- json_addresses[[i]]$results[[1]]$locations[[j]]$adminArea3
    json_df[i, scale + 7] <- json_addresses[[i]]$results[[1]]$locations[[j]]$postalCode
    json_df[i, scale + 8] <- json_addresses[[i]]$results[[1]]$locations[[j]]$geocodeQuality
    json_df[i, scale + 9] <- json_addresses[[i]]$results[[1]]$locations[[j]]$geocodeQualityCode
    json_df[i, scale + 10] <- json_addresses[[i]]$results[[1]]$locations[[j]]$latLng$lat
    json_df[i, scale + 11] <- json_addresses[[i]]$results[[1]]$locations[[j]]$latLng$lng
    #print('Made it to end')
  }
}

write.csv(json_df, "addresses_latlong.csv")

pubhth_guns_geo <- pubhth_guns_add %>% left_join(json_df, by=c("INCIDENT_ADDRESS" = "RAW")) %>% 
  mutate(STREET_1 = ifelse(is.na(STREET_1), toupper(STREET), STREET_1), CITY = ifelse(is.na(CITY.x), toupper(CITY.y), CITY.x),
         STATE = ifelse(is.na(STATE.x), STATE.y, STATE.x), ZIP = ifelse(is.na(ZIP.x), ZIP.y, ZIP.x),
         LAT = ifelse(is.na(LAT.x), LAT.y, LAT.x), LON = ifelse(is.na(LON.x), LON.y, LON.x),
         STREET_2 = toupper(STREET_2.y)) %>% select(c(CASE_NO, NAME, MANNER, AGE, SEX, RACE, LATINO,          
                                                      PRIMARY_CAUSE_A, PRIMARY_CAUSE_B, PRIMARY_CAUSE_C, SECONDARY_CAUSE, DATE_OF_DEATH, 
                                                      INCIDENT_ADDRESS, INCIDENT_CITY, ID, HASH_KEY, INCIDENT_ADDRESS, FULL_LOC_NAME=FULL_NAME, 
                                                      ADDRESS, LOC_TYPE, STREET_1, CITY, STATE, ZIP, 
                                             STATUS, LAT, LON, GEO_QUALITY, GEO_QUAL_CODE,
                                             TRACT, PROP_UNEMP, PROP_POVERTY, MED_INCOME, INCOME_INEQ, PROP_SINGLE_WOMAN,
                                             PROP_OWN_OCC, PROP_AF_AM, PROP_HS, PERS_SQ_KILO, UNITS_SQ_KILO))
write.csv(pubhth_guns_geo, "full_geo_data.csv")




# Read in created datasets

pubhth_data <- read.csv("full_geo_data.csv")

guns_sf <- st_as_sf(filter(pubhth_data, !is.na(LAT) & !is.na(LON)), coords = c("LON", "LAT"), crs = 4326) 

summary(pubhth_guns_geo$CNT_FINAL)

mapview(guns_sf, zcol = "SEX", at = seq(.35,.65,.05), legend = TRUE, label = "FULL_NAME")
mapview(guns_sf, zcol = "CNT_FINAL", at = seq(0, 2500, 250), legend = TRUE, label = "FULL_NAME")

#look for tract geo-data to have as background layer?

ggplot() + geom_sf(data=guns_sf)






### Export Locations to add Addresses - before adding to above ###

addresses <- sort(table(pubhth_guns$INCIDENT_ADDRESS), decreasing = T)
write.csv(addresses, file = "me_addresses.csv")

nonscene_addresses <- as_tibble(unique(filter(pubhth_guns, grepl('^[123456789]+', INCIDENT_ADDRESS))$INCIDENT_ADDRESS)) %>% 
  mutate(RAW = value, ADDRESS = value, CITY = '', STATE = 'IL', ZIP = '', ID = seq.int(nrow(nonscene_addresses))) %>% select(-c('value'))
write.csv(nonscene_addresses, file = "nonscene_addresses.csv")



### Explore Data ###

barplot(table(pubhth_guns$INCIDENT_CITY))
barplot(table(filter(pubhth_guns, INCIDENT_CITY != 'Chicago')$INCIDENT_CITY))

ggplot(pubhth_guns, aes(x = DATE_OF_DEATH, color = SEX)) +
  geom_freqpoly()



