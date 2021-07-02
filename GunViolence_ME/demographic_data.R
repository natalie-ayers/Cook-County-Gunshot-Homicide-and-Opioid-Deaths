library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(data.table)



#PolicyMap Data!?


pubhth_guns_geo_fin <- read.csv("full_geo_data.csv")
full_ipums_demo <- read.csv("C:/Users/natra/Documents/ME_2021/ipums_nhgis_demo1_csv/nhgis0001_csv/nhgis0001_ts_geog2010_zcta.csv")

# Use ME Public Health Reports to identify all zip codes of interest to use as filter for IPUMS data
zips <- unique(pubhth_guns_geo_fin$ZIP)
zips <- zips[!is.na(zips)] %>% as.character() %>% as.data.frame()

ipums_chi <- inner_join(full_ipums_demo, zips, by=c('ZCTAA'="."))
write.csv(ipums_chi, "ipums_demo_chi.csv")

ipums_chi <- read.csv("ipums_demo_chi.csv")


# Rename IPMUS demographic fields to friendly names
chi_demo_data <- ipums_chi %>% select(c("GEOGYEAR", "DATAYEAR", "ZCTAA", "CL8AA", "CM0AA", "CM0AB", "CM1AA", "CM1AB", "CM1AC", 
"CM1AD", "CM1AE", "CM1AF", "CM1AG", "CM9AA", "CM9AB", "CN1AA", "CN1AB", "CY5AA", "CY5AB",
"CY5AC", "CY5AD", "CY5AE", "CY5AF", "CY5AG", "CY5AH", "CY5AI", "CY5AJ", "CY5AK", "CY5AL")) %>% 
  rename(total_pop = "CL8AA", total_males = "CM0AA", total_females = "CM0AB",
    pop_race_white = "CM1AA", pop_race_black = "CM1AB", pop_race_native_am = "CM1AC", pop_race_asian = "CM1AD",
    pop_race_pacific_island = "CM1AE", pop_race_other = "CM1AF", pop_race_mult = "CM1AG", 
    occupied_units = "CM9AA", vacant_units = "CM9AB", owner_occupied = "CN1AA", renter_occupied = "CN1AB",
    own_white_occupied = "CY5AA", own_black_occupied = "CY5AB", own_native_am_occupied = "CY5AC",
    own_asian_occupied = "CY5AD", own_other_occupied = "CY5AE", own_mult_race_occupied = "CY5AF",
    rent_white_occupied = "CY5AG", rent_black_occupied = "CY5AH", rent_native_am_occupied = "CY5AI",
    rent_asian_occupied = "CY5AJ", rent_other_occupied = "CY5AK", rent_mult_race_occupied = "CY5AL")


# Expand dataset to include a row per year - individual years filled in according to their closest decades
chi_demo_filled <- filter(chi_demo_data, DATAYEAR == 1990)
chi_demo_filled['join_year'] = 1984

start_yr <- 1984
for(i in seq(1, 5)){
  sec <- filter(chi_demo_data, DATAYEAR == 1990)
  sec['join_year'] = start_yr + i
  chi_demo_filled <- union(chi_demo_filled, sec)
}

for(yr in c(1990,2000,2010)){
  for(i in seq(0,9)){
    sec <- filter(chi_demo_data, DATAYEAR == yr)
    sec['join_year'] = yr + i
    chi_demo_filled <- union(chi_demo_filled, sec)
  }
}
sec <- filter(chi_demo_data, DATAYEAR == 2010)
sec['join_year'] = 2020
chi_demo_filled <- union(chi_demo_filled, sec)

# Pivot tables by desired dimensions (sex, race, owner-occupied race, renter-occupied race) to include these as values in attributes
chi_demo_longer <- pivot_longer(chi_demo_filled, cols=c('total_males','total_females'),names_to = 'SEX', values_to = 'POP_SEX', names_prefix = 'total_')
# Initial table by Sex, Race
chi_demo_race <- pivot_longer(chi_demo_longer, 
                                cols=c('pop_race_white','pop_race_black','pop_race_native_am','pop_race_asian',
                                       'pop_race_pacific_island','pop_race_other','pop_race_mult'),
                                names_to = 'RACE', values_to = 'POP_RACE', names_prefix = 'pop_race_') %>%
  mutate(RACE = ifelse(RACE == 'mult' | RACE == 'pacific_island', 'other', RACE))
chi_demo_race <- chi_demo_race[!is.na(chi_demo_race$POP_RACE), ]

chi_demo_main <- unique(chi_demo_race[,c("GEOGYEAR","DATAYEAR","ZCTAA","total_pop","occupied_units","vacant_units",
                                         "owner_occupied","renter_occupied","join_year","SEX","POP_SEX","RACE")])


chi_demo_race <- chi_demo_race %>% select(c('ZCTAA','join_year','RACE','POP_RACE','SEX'))

# Initial table by Sex, Race of Owner Occupiers
chi_demo_own_occ <- pivot_longer(chi_demo_longer,
                                cols=c("own_white_occupied", "own_black_occupied",
                                       "own_native_am_occupied","own_asian_occupied","own_other_occupied","own_mult_race_occupied"),
                                names_to = 'OWN_RACE',
                                values_to = 'POP_OWN_RACE',
                                names_prefix = 'own_'
                                ) %>% mutate(OWN_RACE = sub('_occupied','',OWN_RACE)) %>% 
  mutate(RACE = ifelse(OWN_RACE=='mult_race','other',OWN_RACE)) %>% 
  select(c('ZCTAA','join_year','RACE','POP_OWN_RACE','SEX')) %>% na.omit()

# Initial table by Sex, Race of Renter Occupiers
chi_demo_rent_occ <- pivot_longer(chi_demo_longer,
                                cols=c("rent_white_occupied","rent_black_occupied","rent_native_am_occupied",
                                       "rent_asian_occupied","rent_other_occupied","rent_mult_race_occupied"),
                                names_to='RENT_RACE',
                                values_to='POP_RENT_RACE',
                                names_prefix='rent_'
                                ) %>% mutate(RENT_RACE = sub('_occupied','',RENT_RACE)) %>% mutate(RACE = ifelse(RENT_RACE=='mult_race','other',RENT_RACE)) %>%
  select(c('ZCTAA','join_year','RACE','POP_RENT_RACE','SEX')) %>% na.omit()


# Aggregate 'Other' race values for each table above

agg_other <- function(df, sum_field){
  df_other <- filter(df, RACE=='other')
  print(nrow(df_other))
  df_agg <- aggregate(as.numeric(select(df_other,all_of(sum_field))[[1]]), 
                      by=list(df_other$join_year, df_other$ZCTAA, df_other$SEX, df_other$RACE), FUN=sum, na.rm=TRUE) %>%
    rename({{sum_field}} := 'x', join_year = 'Group.1', ZCTAA = 'Group.2', SEX = 'Group.3', RACE = 'Group.4')
  print(nrow(df_agg))
  df_rest <- filter(df, RACE != 'other')
  print(nrow(df_rest))
  df_final <- union(df_rest, df_agg)
}

chi_demo_race <- agg_other(chi_demo_race, "POP_RACE")
chi_demo_own_occ <- agg_other(chi_demo_own_occ, "POP_OWN_RACE")
chi_demo_rent_occ <- agg_other(chi_demo_rent_occ, "POP_RENT_RACE")


# Merge pivoted tables by sex, race, owner-occupied race, and renter-occupied race
chi_demo <- merge(chi_demo_main, chi_demo_race, by=c('ZCTAA','join_year','RACE','SEX'),all=TRUE)
chi_demo <- merge(chi_demo, chi_demo_own_occ, by=c('ZCTAA','join_year','RACE','SEX'),all=TRUE)
chi_demo <- merge(chi_demo, chi_demo_rent_occ, by=c('ZCTAA','join_year','RACE','SEX'),all=TRUE)
chi_demo <- select(chi_demo, c("ZCTAA","join_year","RACE","SEX","GEOGYEAR", "DATAYEAR" ,"total_pop", "occupied_units","vacant_units",
                               "owner_occupied","renter_occupied" ,"POP_SEX","POP_RACE","POP_OWN_RACE","POP_RENT_RACE")) %>%
  mutate(SEX = ifelse(SEX == 'males', 'Male',ifelse(SEX=='females','Female',SEX)), 
         RACE = ifelse(RACE=='black','Black', 
                       ifelse(RACE=='white','White',
                              ifelse(RACE=='asian','Asian',
                                     ifelse(RACE=='native_am','Native American',
                                            'Other')))))




write.csv(chi_demo_final, 'chi_demo_filled.csv')




### Yearly 5-Year ACS Demo Data

acs_blockgroups <- st_read('~/ME_2021/Python/acs_blockgroup.geojson', crs=4326)

chi_neighborhoods <- st_read('~/ME_2021/Python/chi_neighborhoods.shp', crs=4326)
acs_bg_neigh <- st_join(acs_blockgroups, chi_neighborhoods, join=st_intersects, left=TRUE, largest=TRUE)

chi_zips <- st_read('~/ME_2021/GunViolence_ME/chi_zips.shp', crs=4326)
acs_bg_neigh_zip <- st_join(acs_bg_neigh, chi_zips, join=st_intersects, left=TRUE, largest=TRUE)
plot(acs_bg_neigh_zip)

chi_communityareas <- st_read('~/ME_2021/GunViolence_ME/boundaries_community_areas/chi_com_areas.shp', crs=4326)
chi_communityareas <- chi_communityareas %>% select(c('area_numbe','community', 'geometry'))
acs_bg_neigh_ca_zip <- st_join(acs_bg_neigh_zip, chi_communityareas, join=st_intersects, left=TRUE, largest=TRUE)


acs_bg_neigh_ca_zip <- as.data.frame(acs_bg_neigh_ca_zip) %>% select(-c(geometry, shape_area.x, shape_len.x, shape_area.y,
                                                                  shape_len.y, objectid)) %>% rename(`Block Group`=geoid10,
                                                                                                     community_area_id = 'area_numbe',
                                                                                                     community_area = 'community')

# Handle -666666666 Median Income values

acs_bg_neigh_ca_zip <- acs_bg_neigh_ca_zip %>% mutate(Median_Income_All = ifelse(Median_Income_All == -666666666, NA, Median_Income_All),
                          Median_Income_White = ifelse(Median_Income_White == -666666666, NA, Median_Income_White),
                          Median_Income_Black = ifelse(Median_Income_Black == -666666666, NA, Median_Income_Black),
                          Median_Income_Native_American = ifelse(Median_Income_Native_American == -666666666, NA, Median_Income_Native_American),
                          Median_Income_Asian = ifelse(Median_Income_Asian == -666666666, NA, Median_Income_Asian)
                          )

write_csv(acs_bg_neigh_ca_zip, 'acs_5year_blockgroups_wide.csv')


acs_bg_neigh_ca_income <- pivot_longer(acs_bg_neigh_ca_zip,
                                 cols=c("Median_Income_White", "Median_Income_Black",
                                        "Median_Income_Native_American","Median_Income_Asian"),
                                 names_to = 'RACE',
                                 values_to = 'MEDIAN_INCOME_RACE',
                                 names_prefix = 'Median_Income_') 

acs_bg_neigh_ca_population_race <- pivot_longer(acs_bg_neigh_ca_zip, cols=c("Population_White","Population_Black",
                          "Population_Native_American","Population_Asian"
                          ),names_to='RACE',
                            values_to='POPULATION_RACE',
                            names_prefix='Population_')
acs_bg_neigh_ca_population_sex <- pivot_longer(acs_bg_neigh_ca_zip, cols=c("Population_Female","Population_Male"
                                                   ),names_to='SEX',
                                                    values_to='POPULATION_SEX',
                                                    names_prefix='Population_')

acs_bg_neigh_ca_population_race <- acs_bg_neigh_ca_population_race %>%
  select(c(`Block Group`,ACS_YEAR,RACE,POPULATION_RACE))
acs_bg_neigh_ca_population_sex <- acs_bg_neigh_ca_population_sex %>%
  select(c(`Block Group`,ACS_YEAR,SEX,POPULATION_SEX))

acs_bg_neigh_ca_long <- inner_join(acs_bg_neigh_ca_income, acs_bg_neigh_ca_population_race, by=c("Block Group", "ACS_YEAR", "RACE"))
acs_bg_neigh_ca_long <- inner_join(acs_bg_neigh_ca_long, acs_bg_neigh_ca_population_sex, by=c("Block Group","ACS_YEAR"))

acs_bg_neigh_ca_2020_dummy <- acs_bg_neigh_ca_long %>% filter(ACS_YEAR == 2019)
acs_bg_neigh_ca_2020_dummy$ACS_YEAR <- '2020'

acs_bg_neigh_ca_long <- union(acs_bg_neigh_ca_long, acs_bg_neigh_ca_2020_dummy)


write.csv(acs_bg_neigh_ca_long, 'acs_5year_blockgroups.csv')


### Resources Referenced: 

#https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html













