library(opencage)
library(dplyr)
library(stringr)
library(jsonlite)

####
# Sources Used:
# https://towardsdatascience.com/breaking-down-geocoding-in-r-a-complete-guide-1d0f8acd0d4b
# https://docs.ropensci.org/opencage/articles/customise_query.html
# https://opencagedata.com/api#quickstart


####




#set open cage key with oc_config()

oc_config()

# Get addresses to be geolocated from me case data; filter addresses for those most complete; add additional identifying fields
locs <- guns_fin[,c('CASE_NO','INCIDENT_ADDRESS','INCIDENT_CITY')]
locs <- locs %>% filter(!(INCIDENT_ADDRESS %like% 'UNK' | (INCIDENT_ADDRESS %like%  )))

locs_filt <- unique(subset(locs, !grepl('^[[:digit:]{2}[:punct:]{1}.*]', locs$CASE_NO) & substr(locs$INCIDENT_ADDRESS,1,3) != 'UNK'))
locs_filt$INCIDENT_CITY <- ifelse(is.na(locs_filt$INCIDENT_CITY), 'CHICAGO', as.character(locs_filt$INCIDENT_CITY))
locs_filt <- locs_filt[-c(1),]

locs_filt$full_address <- paste(locs_filt$INCIDENT_ADDRESS,', ' ,locs_filt$INCIDENT_CITY, sep="")
locs_filt[1,4]

# Use Open Cage API to geocode full list of addresses

#temp <- locs_filt[1:50,]
#temp <- locs_filt[51:500,]
#temp <- locs_filt[501:1500,]
#temp <- locs_filt[1501:2400,]
#temp <- locs_filt[2401:4286,]

#locs_2401_end <- oc_forward_df(data=temp, placename = full_address, countrycode='US', 
#              bounds=oc_bbox(-90.06527,39.82326,-86.55094,43.22390), proximity = oc_points(41.87394839029574, -87.68008235783819), 
#               output="short")
              
              
full_locs <- union(locs_1_50, locs_51_500)
full_locs <- union(full_locs, locs_501_1500)
full_locs <- union(full_locs, locs_1501_2400)
full_locs <- union(full_locs, locs_2401_end)

write.csv(full_locs, 'opencage_locs.csv')


test_addresses <- function(geoloc_addresses, address_field){
  locs_good <- unique(subset(geoloc_addresses, grepl('[.*[:digit:]{2,}.*]', oc_formatted) & 
                               (str_detect(oc_formatted, str_extract(all_of(address_field), "[0-9]+")))))
  locs_good <- locs_good %>% select(c('CASE_NO','oc_lat','oc_lng','oc_formatted')) %>% rename(lat = 'oc_lat', long = 'oc_lng')
}


# Initial test for geocoding accuracy: whether a house address number was coded, and whether this number matches with the
# original address provided
locs_good <- unique(subset(full_locs, grepl('[.*[:digit:]{2,}.*]', full_locs$oc_formatted) & 
                                          (str_detect(full_locs$oc_formatted, str_extract(full_locs$INCIDENT_ADDRESS, "[0-9]+")))))
locs_good <- locs_good %>% select(c('CASE_NO','oc_lat','oc_lng','oc_formatted')) %>% rename(lat = 'oc_lat', long = 'oc_lng')

locs_todo <- unique(subset(full_locs, !grepl('[.*[:digit:]{2,}.*]', full_locs$oc_formatted) | 
                             !(str_detect(full_locs$oc_formatted, str_extract(full_locs$INCIDENT_ADDRESS, "[0-9]+")))))


# For those addresses which didn't pass the above tests, attempt to clean and augment provided addresses for second attempt

locs_todo$full_address <- paste(locs_todo$full_address, ", United States", sep="")
locs_todo$full_address_new <- str_replace(str_replace(str_replace(str_replace(str_replace(str_replace(str_replace(str_replace(locs_todo$full_address, 
                                                                                                                              fixed(" S. "), " SOUTH "), 
                                                                  fixed(" S "), " SOUTH "), fixed(" N. "), " NORTH "), 
                                                                  fixed(" N "), " NORTH "), fixed(" W. "), " WEST "),
                                          fixed(" W "), " WEST "), fixed(" E. "), " EAST "), fixed(" E "), " EAST ")

locs_todo_clean <- select(locs_todo, -c('oc_lat','oc_lng','oc_formatted')) 


# Try Open Cage API againwith new, more restrictive bounds: 
# adding 'United States' to all addresses, replacing N/S/W/E with full North, South, etc
todo_clean_full <- oc_forward_df(data=locs_todo_clean, placename = full_address_new, countrycode='US', 
                               bounds=oc_bbox(-88.45017,41.49624,-87.25895,42.30971), proximity = oc_points(41.87394839029574, -87.68008235783819), 
                               output="short", limit=3)
todo_locs_a <-  unique(subset(todo_clean_full, !grepl('[.*[:digit:]{2,}.*]', todo_clean_full$oc_formatted) | 
                                !(str_detect(todo_clean_full$oc_formatted, str_extract(todo_clean_full$INCIDENT_ADDRESS, "[0-9]+")))))

full_locs_a <- unique(subset(todo_clean_full, grepl('[.*[:digit:]{2,}.*]', todo_clean_full$oc_formatted) & 
                               (str_detect(todo_clean_full$oc_formatted, str_extract(todo_clean_full$INCIDENT_ADDRESS, "[0-9]+")))))

# Check for duplicates in the addresses that pass the test: if found and no good address options, remove from 'good'
# list and add back to 'todo' list
dups_full_locs_a <- filter(full_locs_a, CASE_NO %in% c(full_locs_a[duplicated(full_locs_a$CASE_NO),]$CASE_NO))
full_locs_a <- full_locs_a %>% subset(CASE_NO != 'ME2017-04648' & CASE_NO != 'ME2016-06128')
todo_locs_a <- union(todo_locs_a, dups_full_locs_a)

full_locs_a <- full_locs_a %>% select(c('CASE_NO','oc_lat','oc_lng','oc_formatted')) %>% 
  rename(lat = 'oc_lat', long = 'oc_lng')

write.csv(todo_locs_a, 'oc_locs_tocheck.csv')



# Manually clean addresses with apartment/suite/etc, named location, or cross streets;
# Use these manually-updated addresses in the Open Cage API again

still_todo <- read.csv("oc_locs_tocheck.csv")

todo_agg <- still_todo %>% group_by(CASE_NO, full_address_new) %>% 
  summarize(cleaned_address = first(cleaned_address), lat = first(lat), long = first(long), oc_formatted = first(oc_formatted))
todo_agg_good <- filter(todo_agg, !is.na(lat)) %>% select(c('CASE_NO','lat','long','oc_formatted'))

todo_agg <- filter(todo_agg, (cleaned_address != '') & is.na(lat)) %>% select(-c('lat','long','oc_formatted','full_address_new'))

todo_agg_oc <- oc_forward_df(data=todo_agg, placename = cleaned_address, countrycode='US', 
                                bounds=oc_bbox(-88.45017,41.49624,-87.25895,42.30971), 
                                proximity = oc_points(41.87394839029574, -87.68008235783819), 
                                output="short")

# Test success of current batch of addresses
#todo_agg_oc_good <- test_addresses(todo_agg_oc, 'cleaned_address')
todo_agg_oc_good <- unique(subset(todo_agg_oc, grepl('[.*[:digit:]{2,}.*]', todo_agg_oc$oc_formatted) & 
                                    (str_detect(todo_agg_oc$oc_formatted, str_extract(todo_agg_oc$cleaned_address, "[0-9]+")))))
todo_agg_oc_good <- todo_agg_oc_good %>% select(c('CASE_NO','oc_lat','oc_lng','oc_formatted')) %>% rename(lat = 'oc_lat', long = 'oc_lng')


# First 3 Rounds of addresses geoloc'd

locs_confirmed <- union(locs_good, todo_agg_good)
locs_confirmed <- union(locs_confirmed, todo_agg_oc_good)
locs_confirmed <- union(locs_confirmed, full_locs_a)


# Filtering for locations that still need to be geoloc'd

confirmed_cases <- locs_confirmed$CASE_NO
locs_undone <- filter(full_locs, !(CASE_NO %in% confirmed_cases))
locs_undone <- select(locs_undone, c('CASE_NO','INCIDENT_ADDRESS','INCIDENT_CITY','full_address'))





#####

# Locating still-unlocated addresses with Nominatim API:


## geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler 
## modified by: N. Ayers


nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1&countrycodes="US"&email="nayers@uchicago.edu"')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  #print(d)
  return(data.frame(long = as.numeric(d$lon), lat = as.numeric(d$lat), full_address = d$display_name))
}

locs_undone['full_address'][1,]

nom_locs <- data.frame(lon=double(),
                  lat=double(),
                  full_address=character(),
                  CASE_NO=character(),
                  stringsAsFactors=FALSE)

for(i in 1:nrow(locs_undone)){
  print(i)
  geoloc <- nominatim_osm(locs_undone['full_address'][i,])
  if(nrow(geoloc)>0) geoloc$CASE_NO <- locs_undone['CASE_NO'][i,][[1]]
  #print(geoloc)
  nom_locs <- rbind(nom_locs, geoloc)
}

write.csv(nom_locs, 'nom_locs.csv')

# Filter out nom_locs which aren't full/correct?
nom_locs_join <- inner_join(nom_locs, locs_filt, by='CASE_NO')
nom_locs_join <- nom_locs_join %>% select(-c('full_address.y','INCIDENT_CITY')) %>%
  rename(full_address='full_address.x')

nom_locs_good <- unique(subset(nom_locs_join, grepl('[.*[:digit:]{2,}.*]', nom_locs_join$full_address) & 
                  (str_detect(nom_locs_join$full_address, str_extract(nom_locs_join$INCIDENT_ADDRESS, "[0-9]+")))))
nom_locs_good <- nom_locs_good %>% select(-c('INCIDENT_ADDRESS'))

# Union Nominatim addresses with OC addresses:

locs_confirmed <- rename(locs_confirmed, full_address=oc_formatted)
locs_confirmed <- union(locs_confirmed, nom_locs_good)


# Filtering for locations that still need to be geoloc'd

confirmed_cases <- locs_confirmed$CASE_NO
locs_undone <- filter(full_locs, !(CASE_NO %in% confirmed_cases))
locs_undone <- select(locs_undone, c('CASE_NO','INCIDENT_ADDRESS','INCIDENT_CITY','full_address'))



###

# Separate out zip codes and cities from geolocated addresses

locs_confirmed$ZIP <- str_extract_all(locs_confirmed$full_address, "[0-9]{5}")
locs_confirmed$CITY <- str_match_all(locs_confirmed$full_address, ",\\s[A-Za-z\\s]+,\\s")

for(i in 1:nrow(locs_confirmed)){
  print(i)
  if(length(locs_confirmed$ZIP[[i]]) > 1) locs_confirmed$ZIP[[i]] = locs_confirmed$ZIP[[i]][-1]
  if(length(locs_confirmed$CITY[[i]]) == 2)  locs_confirmed$CITY[[i]] = locs_confirmed$CITY[[i]][1]
  if(length(locs_confirmed$CITY[[i]]) > 2)  locs_confirmed$CITY[[i]] = locs_confirmed$CITY[[i]][2]
}

locs_confirmed$CITY <- str_trim(gsub(",","",locs_confirmed$CITY))

locs_confirmed <- data.frame(lapply(locs_confirmed, as.character), stringsAsFactors = FALSE)



write.csv(locs_confirmed, 'oc_nom_locs.csv')

















