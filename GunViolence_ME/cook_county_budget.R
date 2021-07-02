library(RSocrata)
library(tidyverse)
library(dplyr)
library(readxl)
library(data.table)
library(stringi)
library(sparkR)
library(stringr)



#https://aigraduate.com/beginner-s-guide-to-pivoting-data-frames-in-r/

budget_approps_2016_2019 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/s6n6-yr7v.json"
)
budget_approps_2016_2019 <- rename(budget_approps_2016_2019, 'function_area'=X_1_function_area, department = X_2_modified_bureau_hierachy)

budget_mapping_2016 <- read_xlsx('~/ME_2021/cc_budget_mapping.xlsx', sheet = 'budget-mapping16-19', range=cell_cols("B:E"))
budget_16_clean <- merge(budget_approps_2016_2019, budget_mapping_2016, by=c("funds","function_area","department"))


budget_16_clean <- budget_16_clean %>% select(c(fund = funds,department, category, year, amount))
budget_16_clean <- aggregate(as.numeric(budget_16_clean$amount), 
                             by=list(budget_16_clean$year, budget_16_clean$fund,
                                     budget_16_clean$department, budget_16_clean$category), FUN=sum, na.rm=TRUE) %>%
  rename(amount = 'x', year = 'Group.1', fund = 'Group.2', department = 'Group.3', category = 'Group.4')
yearly_funds <- aggregate(as.numeric(budget_16_clean$amount), by=list(budget_16_clean$year), FUN=sum, na.rm=TRUE)
budget_16_clean <- left_join(budget_16_clean, yearly_funds, by = c('year' = 'Group.1')) 
budget_16_clean <- rename(budget_16_clean, 'year_total' = x) 
budget_16_clean$year <- as.integer(budget_16_clean$year)
budget_16_clean$amount <- as.numeric(budget_16_clean$amount)
budget_16_clean$genfundtype <- NA
anyDuplicated(budget_16_clean, by=c('fund', 'genfundtype', 'department', 'category','year'))



budget_2015 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/fu87-28e8.json"
)
budget_2015 <- budget_2015 %>% mutate(fund = str_trim(fund))

budget_mapping_2015 <- read_xlsx('~/ME_2021/cc_budget_mapping.xlsx', sheet = 'budget-mapping15', range=cell_cols("A:C"))

budget_15_clean <- merge(budget_2015, budget_mapping_2015, by=c('fund','tab'))
budget_15_clean <- budget_15_clean %>% 
  mutate(fund = fund_type, department = department_name, genfundtype = general_fund_type) %>% 
  select(c(fund, genfundtype, department, category, amount = fy2015_adopted)) %>% na.omit()
  
budget_15_clean <- aggregate(as.numeric(budget_15_clean$amount), by=list(budget_15_clean$fund, budget_15_clean$genfundtype,
                                                                         budget_15_clean$department, budget_15_clean$category), FUN=sum) %>%
  rename(amount = 'x', fund = 'Group.1', genfundtype = 'Group.2', department = 'Group.3', category = 'Group.4')
budget_15_clean$year = 2015
budget_15_clean$year_total <- sum(budget_15_clean$amount)
anyDuplicated(budget_15_clean, by=c('fund', 'genfundtype', 'department', 'category'))




budget_2014 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/rbkx-fj74.json"
)
budget_mapping_2014 <- read_xlsx('~/ME_2021/cc_budget_mapping.xlsx', sheet = 'budget-mapping14', range=cell_cols("A:C"))

budget_14_clean <- merge(budget_2014, budget_mapping_2014, by=c('fund','tab'))

budget_14_clean <- budget_14_clean %>% 
  mutate(fund = fund_type, department = department_name, genfundtype = general_fund_type) %>%  
  select(c(fund, genfundtype, department, category, amount = fy2014_adopted)) %>% na.omit()
budget_14_clean <- aggregate(as.numeric(budget_14_clean$amount), by=list(budget_14_clean$fund, budget_14_clean$genfundtype,
                                                                         budget_14_clean$department, budget_14_clean$category), FUN=sum) %>%
  rename(amount = 'x', fund = 'Group.1', genfundtype = 'Group.2', department = 'Group.3', category = 'Group.4')
budget_14_clean$year <- 2014
budget_14_clean$year_total <- sum(budget_14_clean$amount)
anyDuplicated(budget_14_clean, by=c('fund', 'genfundtype', 'department', 'category'))




budget_1993_2011 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/hd8u-wzhx.json"
)
budget_mapping_1993 <- read_xlsx('~/ME_2021/cc_budget_mapping.xlsx', sheet = 'budget-mapping93-11', range=cell_cols("A:C"))

budget_93_clean <- budget_1993_2011 %>% 
  select(-c(short_title, department_id, url, department_description, control_officer)) %>% 
  pivot_longer(-c(fund, department), names_to='type_year', values_to='amount') %>% separate(type_year, c("type", "year"), "_") %>%
  mutate(expenditures = ifelse(type=='expenditures', as.integer(amount), 0), 
         appropriations = ifelse(type=='appropriations', as.integer(amount), 0)) %>%
  select(-c(type, amount)) %>% as.data.frame() 

budget_93_clean <- merge(budget_93_clean, budget_mapping_1993, by=c('fund','department'))

budget_93_clean <- aggregate(as.numeric(budget_93_clean$appropriations), 
                             by=list(budget_93_clean$year, budget_93_clean$fund,
                                     budget_93_clean$department, budget_93_clean$category), FUN=sum, na.rm=TRUE) %>%
  rename(amount = 'x', year = 'Group.1', fund = 'Group.2', department = 'Group.3', category = 'Group.4')
budget_93_clean$year = as.integer(budget_93_clean$year)
yearly_funds <- aggregate(as.numeric(budget_93_clean$amount), by=list(budget_93_clean$year), FUN=sum, na.rm=TRUE)
budget_93_clean <- left_join(budget_93_clean, yearly_funds, by = c('year' = 'Group.1'))
budget_93_clean <- budget_93_clean[!is.na(budget_93_clean$amount),] %>% rename(year_total = 'x')
budget_93_clean$genfundtype <- NA
anyDuplicated(budget_93_clean, by=c('fund', 'genfundtype', 'department', 'category'))



budget_2012 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/dvk8-b8rs.json"
)
budget_mapping_2012 <- read_xlsx('~/ME_2021/cc_budget_mapping.xlsx', sheet = 'budget-mapping12', range=cell_cols("A:D"))

budget_12_clean <- merge(budget_2012, budget_mapping_2012, by=c('genfundtype','sort_for_summary','depttitle'))

budget_12_clean <- budget_12_clean %>% 
  mutate(fund = fundtype, department = depttitle, amount = adopted) %>%
  select(c('fund', 'genfundtype', 'department', 'category', 'amount'))
budget_12_clean <- budget_12_clean[!is.na(budget_12_clean$amount),]
budget_12_clean <- aggregate(as.numeric(budget_12_clean$amount), by=list(budget_12_clean$fund, budget_12_clean$genfundtype,
                                                                         budget_12_clean$department, budget_12_clean$category), FUN=sum) %>%
  rename(amount = 'x', fund = 'Group.1', genfundtype = 'Group.2', department = 'Group.3', category = 'Group.4')
budget_12_clean$year_total <- sum(budget_12_clean$amount)
budget_12_clean$year = 2012




budget_2013 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/awmg-c7xz.json"
)
# has adopted and origadopted - not sure which is better, going with origadopted since dataset was from 2013 but last updated Oct 2014, so 
# adopted likely is the adjusted value
budget_2013 <- budget_2013 %>% mutate(depttitle = str_trim(depttitle))
budget_mapping_2013 <- read_xlsx('~/ME_2021/cc_budget_mapping.xlsx', sheet = 'budget-mapping13', range=cell_cols("A:D"))

budget_13_clean <- merge(budget_2013, budget_mapping_2013, by=c('genfundtype','sort_for_summary','depttitle'))


budget_13_clean <- budget_13_clean %>% 
  mutate(fund = fundtype, department = depttitle, amount = origadopted) %>%
  select(c('fund', 'genfundtype', 'department', 'category', 'amount'))
budget_13_clean <- budget_13_clean[!is.na(budget_13_clean$amount),]
budget_13_clean <- aggregate(as.numeric(budget_13_clean$amount), by=list(budget_13_clean$fund, budget_13_clean$genfundtype,
                                                                         budget_13_clean$department, budget_13_clean$category), FUN=sum) %>%
  rename(amount = 'x', fund = 'Group.1', genfundtype = 'Group.2', department = 'Group.3', category = 'Group.4')
budget_13_clean$year_total <- sum(budget_13_clean$amount)
budget_13_clean$year = 2013
anyDuplicated(budget_13_clean, by=c('fund', 'genfundtype', 'department', 'category'))



#budget_mapping_2020 <- read_xlsx('~/ME_2021/cc_budget_mapping.xlsx', sheet = 'budget-mapping20', range=cell_cols("A:C"))
#budget_2020 <- read_xlsx('~/ME_2021/2020_budget_images.xlsx') %>% select(c(fund_type = 'Fund', department = 'Department', amount = 'Amount')) %>%
#  mutate(year = 2020) %>% na.omit()
  
#budget_2020 <- merge(budget_2020, budget_mapping_2020, by=c('fund_type','department'))
  
#budget_2020$year_total <- sum(budget_2020$amount)
#budget_2020$genfundtype <- NA
#budget_2020 <- budget_2020 %>% rename(fund=fund_type)


budget_93_clean <- budget_93_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]
budget_12_clean <- budget_12_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]
budget_13_clean <- budget_13_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]
budget_14_clean <- budget_14_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]
budget_15_clean <- budget_15_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]
budget_16_clean <- budget_16_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]

budget <- union (budget_16_clean,
                 budget_15_clean)
budget <- union(budget, 
                budget_14_clean)
budget <- union(budget, 
                budget_13_clean)
budget <- union(budget, 
                budget_12_clean)
budget <- union(budget, 
                budget_93_clean)
#budget <- union(budget, 
#                budget_2020)


#NOTE: Discrepancy b/w 2020 appropriations manually pulled from site and approps from 2016-present data table - look into


write.csv(budget, file = 'cook_county_budget.csv')















#####################################################################

budget_approps_2016_2019 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/s6n6-yr7v.json"
)
budget_16_clean <- budget_approps_2016_2019 %>% 
  mutate(category = ifelse(X_1_function_area == 'Public Safety', X_1_function_area, 
                       ifelse(funds == 'Health Enterprise Fund' | X_1_function_area %like% 'Health' | X_1_function_area %like% 'Hospital', 
                              'Public Health', 'Other'))) %>% select(c(fund = funds,  
                                                                   department = X_2_modified_bureau_hierachy, category, year, amount))
budget_16_clean <- aggregate(as.numeric(budget_16_clean$amount), 
                     by=list(budget_16_clean$year, budget_16_clean$fund,
                     budget_16_clean$department, budget_16_clean$category), FUN=sum, na.rm=TRUE) %>%
  rename(amount = 'x', year = 'Group.1', fund = 'Group.2', department = 'Group.3', category = 'Group.4')
yearly_funds <- aggregate(as.numeric(budget_16_clean$amount), by=list(budget_16_clean$year), FUN=sum, na.rm=TRUE)
budget_16_clean <- left_join(budget_16_clean, yearly_funds, by = c('year' = 'Group.1')) 
budget_16_clean <- rename(budget_16_clean, 'year_total' = x) 
budget_16_clean$year <- as.integer(budget_16_clean$year)
budget_16_clean$amount <- as.numeric(budget_16_clean$amount)
budget_16_clean$genfundtype <- NA
anyDuplicated(budget_16_clean, by=c('fund', 'genfundtype', 'department', 'category','year'))


budget_2015 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/fu87-28e8.json"
)
budget_15_clean <- budget_2015 %>% mutate(category = ifelse(tab == 'COOK COUNTY HEALTH AND HOSPITALS SYSTEM', 'Public Health', 
                                                        ifelse(general_fund_type == 'Public Safety Fund' | 
                                                                 tab %in% c('CHIEF JUDGE', 'CLERK OF THE CIRCUIT COURT',
                                                                            'HOMELAND SECURITY AND EMERGENCY MANAGEMENT', 
                                                                            "STATE'S ATTORNEY", 'PUBLIC DEFENDER',
                                                                            'SHERIFF') | fund == 'Law Library', 'Public Safety', 
                                                               'Other' ))) %>%
  mutate(year = 2015, fund = fund_type, department = department_name, genfundtype = general_fund_type) %>%  
  select(c(fund, genfundtype, department, category, amount = fy2015_adopted)) %>% na.omit()
budget_15_clean <- aggregate(as.numeric(budget_15_clean$amount), by=list(budget_15_clean$fund, budget_15_clean$genfundtype,
     budget_15_clean$department, budget_15_clean$category), FUN=sum) %>%
  rename(amount = 'x', fund = 'Group.1', genfundtype = 'Group.2', department = 'Group.3', category = 'Group.4')
budget_15_clean$year = 2015
budget_15_clean$year_total <- sum(budget_15_clean$amount)
anyDuplicated(budget_15_clean, by=c('fund', 'genfundtype', 'department', 'category'))


budget_2014 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/rbkx-fj74.json"
)
budget_14_clean <- budget_2014 %>% mutate(category = ifelse(tab == 'COOK COUNTY HEALTH AND HOSPITALS SYSTEM', 'Public Health', 
                                                        ifelse(general_fund_type == 'Public Safety Fund' | 
                                                                 tab %in% c('CHIEF JUDGE', 'CLERK OF THE CIRCUIT COURT',
                                                                            'HOMELAND SECURITY AND EMERGENCY MANAGEMENT', 
                                                                            "STATE'S ATTORNEY", 'PUBLIC DEFENDER',
                                                                            'SHERIFF') | fund == 'Law Library', 'Public Safety', 
                                                               'Other' ))) %>%
  mutate(fund = fund_type, department = department_name, genfundtype = general_fund_type) %>%  
  select(c(fund, genfundtype, department, category, amount = fy2014_adopted)) %>% na.omit()
budget_14_clean <- aggregate(as.numeric(budget_14_clean$amount), by=list(budget_14_clean$fund, budget_14_clean$genfundtype,
                                 budget_14_clean$department, budget_14_clean$category), FUN=sum) %>%
  rename(amount = 'x', fund = 'Group.1', genfundtype = 'Group.2', department = 'Group.3', category = 'Group.4')
budget_14_clean$year <- 2014
budget_14_clean$year_total <- sum(budget_14_clean$amount)
anyDuplicated(budget_14_clean, by=c('fund', 'genfundtype', 'department', 'category'))


budget_1993_2011 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/hd8u-wzhx.json"
)
budget_93_clean <- budget_1993_2011 %>% 
  select(-c(short_title, department_id, url, department_description, control_officer)) %>% 
  pivot_longer(-c(fund, department), names_to='type_year', values_to='amount') %>% separate(type_year, c("type", "year"), "_") %>%
  mutate(expenditures = ifelse(type=='expenditures', as.integer(amount), 0), 
                               appropriations = ifelse(type=='appropriations', as.integer(amount), 0)) %>%
  select(-c(type, amount)) %>% as.data.frame() 
budget_93_clean <- budget_93_clean %>% mutate(category = ifelse(fund == 'Health Fund' | department %like% 'Lead Poisoning' | 
                                                                  department %like% 'TB Sanitarium' | department %like% 'Managed Care',
                                 'Public Health', 
                               ifelse(fund == 'Public Safety Fund' | department %like% 'Court' | department %like% "State's Attorney" |
                                        department %like% 'Probation' | department %like% 'Security' |
                                        department %like% 'Sheriff' | department %like% '911' | department %like% 'ETSB' |
                                        department %like% 'Mental Health' | department %like% "Children's Waiting Room" |
                                        department %like% "Women's Justice" | department %like% 'Law Library' |
                                      department %like% 'Medical Examiner', 'Public Safety', 'Other')))
budget_93_clean <- aggregate(as.numeric(budget_93_clean$appropriations), 
                             by=list(budget_93_clean$year, budget_93_clean$fund,
                             budget_93_clean$department, budget_93_clean$category), FUN=sum, na.rm=TRUE) %>%
  rename(amount = 'x', year = 'Group.1', fund = 'Group.2', department = 'Group.3', category = 'Group.4')
budget_93_clean$year = as.integer(budget_93_clean$year)
yearly_funds <- aggregate(as.numeric(budget_93_clean$amount), by=list(budget_93_clean$year), FUN=sum, na.rm=TRUE)
budget_93_clean <- left_join(budget_93_clean, yearly_funds, by = c('year' = 'Group.1'))
budget_93_clean <- budget_93_clean[!is.na(budget_93_clean$amount),] %>% rename(year_total = 'x')
budget_93_clean$genfundtype <- NA
anyDuplicated(budget_93_clean, by=c('fund', 'genfundtype', 'department', 'category'))


budget_2012 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/dvk8-b8rs.json"
)
budget_12_clean <- budget_2012 %>% 
  mutate(category = ifelse(sort_for_summary %like% 'COOK COUNTY HEALTH AND HOSPITALS SYSTEM',
                          'Public Health', 
    ifelse(genfundtype == 'Public Safety Fund' | sort_for_summary %like% 'CHIEF JUDGE' | sort_for_summary %like% 'SHERIFF' | 
             sort_for_summary %like% 'CLERK OF THE CIRCUIT COURT' | sort_for_summary %like% "STATE'S ATTORNEY" |
             depttitle %like% 'Security' | depttitle %like% 'Law Library' | depttitle %like% 'Medical Examiner' |
             depttitle %like% 'Judicial Advisory', 'Public Safety',
              'Other')), fund = fundtype, department = depttitle, amount = adopted) %>% 
  select(c('fund', 'genfundtype', 'department', 'category', 'amount'))
budget_12_clean <- budget_12_clean[!is.na(budget_12_clean$amount),]
budget_12_clean <- aggregate(as.numeric(budget_12_clean$amount), by=list(budget_12_clean$fund, budget_12_clean$genfundtype,
                                                                         budget_12_clean$department, budget_12_clean$category), FUN=sum) %>%
  rename(amount = 'x', fund = 'Group.1', genfundtype = 'Group.2', department = 'Group.3', category = 'Group.4')
budget_12_clean$year_total <- sum(budget_12_clean$amount)
budget_12_clean$year = 2012
anyDuplicated(budget_12_clean, by=c('fund', 'genfundtype', 'department', 'category'))


budget_2013 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/awmg-c7xz.json"
)
# has adopted and origadopted - not sure which is better, going with origadopted since dataset was from 2013 but last updated Oct 2014, so 
# adopted likely is the adjusted value
budget_13_clean <-budget_2013 %>% mutate(category = ifelse(sort_for_summary %like% 'COOK COUNTY HEALTH AND HOSPITALS SYSTEM',
                      'Public Health', 
                      ifelse(genfundtype == 'Public Safety Fund' | sort_for_summary %like% 'CHIEF JUDGE' | sort_for_summary %like% 'SHERIFF' | 
                               sort_for_summary %like% 'CLERK OF THE CIRCUIT COURT' | sort_for_summary %like% "STATE'S ATTORNEY" |
                               sort_for_summary %like% 'PUBLIC DEFENDER' | 
                               depttitle %like% 'Security' | depttitle %like% 'Law Library' | depttitle %like% 'Medical Examiner' |
                               depttitle %like% 'Judicial Advisory', 'Public Safety',
                             'Other')), fund = fundtype, department = depttitle, amount = origadopted) %>% 
  select(c('fund', 'genfundtype', 'department', 'category', 'amount'))
budget_13_clean <- budget_13_clean[!is.na(budget_13_clean$amount),]
budget_13_clean <- aggregate(as.numeric(budget_13_clean$amount), by=list(budget_13_clean$fund, budget_13_clean$genfundtype,
                                                                         budget_13_clean$department, budget_13_clean$category), FUN=sum) %>%
  rename(amount = 'x', fund = 'Group.1', genfundtype = 'Group.2', department = 'Group.3', category = 'Group.4')
budget_13_clean$year_total <- sum(budget_13_clean$amount)
budget_13_clean$year = 2013
anyDuplicated(budget_13_clean, by=c('fund', 'genfundtype', 'department', 'category'))



budget_2020 <- read_xlsx('~/ME_2021/2020_budget_images.xlsx') %>% select(c(fund_type = 'Fund', department = 'Department', amount = 'Amount')) %>%
  mutate(year = 2020) %>% mutate(fund = ifelse(fund_type == 'Health Enterprise Fund' | department %like% 'Health' | department %like% 'Hospital',
                                               'Public Health', ifelse(department %in% c('Chief Judge', 'Clerk of the Circuit Court',
                                                                                         'Public Defender', 'Sheriff', "State's Attorney"),
                                                                       'Public Safety', department))) %>% na.omit()
budget_2020$year_total <- sum(budget_2020$amount)



budget_93_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]
budget_12_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]
budget_13_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]
budget_14_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]
budget_15_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]
budget_16_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')]

budget <- union (budget_16_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')],
                 budget_15_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')])
budget <- union(budget, 
                budget_14_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')])
budget <- union(budget, 
                budget_13_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')])
budget <- union(budget, 
                budget_12_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')])
budget <- union(budget, 
                budget_93_clean[c('fund', 'genfundtype', 'department', 'category', 'year', 'amount', 'year_total')])


write.csv(budget, file = 'cook_county_budget.csv')


























## For Expenditures:

budget_2015 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/fu87-28e8.json"
)
budget_15_clean <- budget_2015 %>% mutate(fund = ifelse(tab == 'COOK COUNTY HEALTH AND HOSPITALS SYSTEM' | 
                                                          fund %like% 'Solid Waste', 'Public Health', 
                                                        ifelse(general_fund_type == 'Public Safety Fund' | 
                                                                 tab %in% c('CHIEF JUDGE', 'CLERK OF THE CIRCUIT COURT',
                                                                            'HOMELAND SECURITY AND EMERGENCY MANAGEMENT', 
                                                                            "STATE'S ATTORNEY", 'PUBLIC DEFENDER',
                                                                            'SHERIFF') | fund == 'Law Library', 'Public Safety', 
                                                               stri_trans_totitle(tab) ))) %>%
  select(c(fund, fund_type, amount = fy2015_expenditures)) %>%  mutate(year = 2015) %>% na.omit() 
budget_15_clean$amount <- as.numeric(budget_15_clean$amount)
budget_15_clean$year_total <- sum(budget_15_clean$amount)

budget_2014 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/rbkx-fj74.json"
)
budget_14_clean <- budget_2014 %>% mutate(fund = ifelse(tab == 'COOK COUNTY HEALTH AND HOSPITALS SYSTEM' | 
                                                          fund %like% 'Solid Waste', 'Public Health', 
                                                        ifelse(general_fund_type == 'Public Safety Fund' | 
                                                                 tab %in% c('CHIEF JUDGE', 'CLERK OF THE CIRCUIT COURT',
                                                                            'HOMELAND SECURITY AND EMERGENCY MANAGEMENT', 
                                                                            "STATE'S ATTORNEY", 'PUBLIC DEFENDER',
                                                                            'SHERIFF') | fund == 'Law Library', 'Public Safety', 
                                                               stri_trans_totitle(tab) ))) %>%
  select(c(fund, fund_type, amount = fy2014_expenditures)) %>%  mutate(year = 2014) %>% na.omit()
budget_14_clean$amount <- as.numeric(budget_14_clean$amount)
budget_14_clean$year_total <- sum(budget_14_clean$amount)

budget_1993_2011 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/hd8u-wzhx.json"
)
budget_93_clean <- budget_1993_2011 %>% 
  select(-c(short_title, department_id, url, department_description, control_officer)) %>% 
  pivot_longer(-c(fund, department), names_to='type_year', values_to='amount') %>% separate(type_year, c("type", "year"), "_") %>%
  mutate(expenditures = ifelse(type=='expenditures', as.integer(amount), 0), 
         appropriations = ifelse(type=='appropriations', as.integer(amount), 0)) %>%
  select(-c(type, amount)) %>% as.data.frame() %>% rename(fund_type = 'fund')
budget_93_clean <- budget_93_clean %>% mutate(fund = ifelse(fund_type == 'Health Fund' | department %like% 'Lead Poisoning' | department %like% 'TB Sanitarium' |
                                                              department %like% 'Medical Examiner', 'Public Health', 
                                                            ifelse(fund_type == 'Public Safety Fund' | department %like% 'Court' | department %like% "State's Attorney" |
                                                                     department %like% 'Probation' | department %like% 'Security' |
                                                                     department %like% 'Sheriff' | department %like% '911' | department %like% 'ETSB' |
                                                                     department %like% 'Mental Health' | department %like% "Children's Waiting Room" |
                                                                     department %like% "Women's Justice" | department %like% 'Law Library', 'Public Safety', department)))
budget_93_clean$amount = budget_93_clean$expenditures
budget_93_clean$year = as.integer(budget_93_clean$year)
yearly_funds <- aggregate(as.numeric(budget_93_clean$amount), by=list(budget_93_clean$year), FUN=sum, na.rm=TRUE)
budget_93_clean <- left_join(budget_93_clean, yearly_funds, by = c('year' = 'Group.1'))
budget_93_clean <- budget_93_clean[!is.na(budget_93_clean$amount),] %>% rename(year_total = 'x')

budget_2020 <- read_excel('~/ME_2021/2020_budget_images.xlsx') %>% select(c(fund_type = 'Fund', department = 'Department', amount = 'Amount')) %>%
  mutate(year = 2020) %>% mutate(fund = ifelse(fund_type == 'Health Enterprise Fund' | department %like% 'Health' | department %like% 'Hospital',
                                               'Public Health', ifelse(department %in% c('Chief Judge', 'Clerk of the Circuit Court',
                                                                                         'Public Defender', 'Sheriff', "State's Attorney"),
                                                                       'Public Safety', department))) %>% na.omit()
budget_2020$year_total <- sum(budget_2020$amount)










