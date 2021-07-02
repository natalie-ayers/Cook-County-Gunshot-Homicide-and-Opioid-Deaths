# Cook County Gunshot Homicide and Opioid Deaths

## Introduction

This repo contains the R and Python scripts used to create the [Cook County Gunshot Homicide and Opioid Deaths](https://public.tableau.com/app/profile/natalie.ayers/viz/CookCountyGunshotHomicideandOpioidDeaths/GunandOpioidDeaths) interactive dashboard and associated [presentation](https://docs.google.com/presentation/d/15MvU1IsGYTSDqNEzOoKX-G8zy4eZfAKL26h_HxnBu44/edit?usp=sharing). These are the final products of an academic year internship from 2020-2021 with the Cook County Medical Examiner's Office, working under the direction of Dr. Ponni Arunkumar, Chief Medical Examiner of Cook County. Utilizing Medical Examiner case reports for gunshot homicide deaths from 1984-2020 and for opioid deaths from 2014-2020, this work incorporates demographic data, Chicago and Cook County budget appropriations, Chicago crime statistics, public leadership, and Chicago community areas to provide a tool for analysis of gunshot homicide and opioid deaths in the context of other relevant factors. 

## Repo Contents

### GunViolence_ME (Primary code in R)

**me_cases** Primary script to consolidate gunshot homicide deaths from ME case reports and associated geographic information compiled from the [OpenCage Geocoding API](https://opencagedata.com/), [Nominatim OpenStreetMap API](https://nominatim.org/release-docs/latest/), [IPUMS NHGIS](https://www.nhgis.org/), and manual geocoding with [Google Maps](https://www.google.com/maps). 

**chi_crimes** Ingestion and preparation of Chicago crime data from the [City of Chicago Data Portal](https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2).

**chi_opioid_deaths** Ingestion, preparation, and geocoding of Chicago Opioid deaths from the [Cook County Open Data Portal's Medical Examiner Case Archive](https://datacatalog.cookcountyil.gov/Public-Safety/Medical-Examiner-Case-Archive/cjeq-bs86).

**chicago_budget** Ingestion, categorization, and other preparation of Chicago budget appropriations from the [City of Chicago Data Portal](https://data.cityofchicago.org/). The mapping in chi_budg_documentation.xlsx was used to standardize namings which have changed throughout the years and assign spending to Public Health, Public Safety, or Other. 

**cook_county_budget** Ingestion, categorization, and other preparation of Cook County budget appropriations from the [Cook County Open Data Portal](https://datacatalog.cookcountyil.gov/). Multiple years of Cook County budgets were compiled, and many of the years were inconsistent in their labeling of departments, funds, etc to which appropriations were assigned. The mapping in cc_budget_mapping.xlsx was used to standardize these categories and assign to Public Health, Public Safety, or Other spending. 

**geocode_addresses** Utilize [OpenCage Geocoding API](https://opencagedata.com/) and [Nominatim OpenStreetMap API](https://nominatim.org/release-docs/latest/) to geocode incident addresses. 

**demographic_data** Ingest and prepare 5-year ACS Survey data and IPUMS NHGIS data for use in associating demographic information with Chicago neighborhoods. 

**initial_scratchwork** Initial work done prior to splitting into defined scripts. 

**me_addresses_map** Results of manual geolocation for incident addresses reported as a place name instead of an address (eg, identifying 'TRINITY HOSPITA' from the source data as 'ADVOCATE TRINITY HOSPITAL' and providing a precise address for use in geocoding). These addresses are present in records prior to 2015 due to a different case management system and change in recording practices. Many refer to locations which closed prior to this analysis in 2020, and as such some of the locations are uncertain. These uncertainties are marked as such in this mapping file, as is evidence for potentially controversial decisions. 

**create_tableau_geofields** Work associating Chicago blockgroups, neighborhoods, and zip codes.  

### Python 

**chi_demo_data** Download 5-year ACS Survey data with `censusdata` API

**chi_neighborhoods** Download of Chicago neighborhood boundaries with Socrata API

**mapquest_api** Script to geocode addresses using Mapquest API - ultimately unused due to terms of use





