library(RSocrata)


budget_approps_2016_2019 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/s6n6-yr7v.json"
)

budget_2015 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/fu87-28e8.json"
)

budget_2014 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/rbkx-fj74.json"
)

budget_1993_2011 <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/hd8u-wzhx.json"
)

# Use if necessary - daily appropriations from 2011:
# https://datacatalog.cookcountyil.gov/Finance-Administration/Budget-Management-Daily-Appropriations-Report/t84e-297e

# Or potentially 2012 year-end estimates (prepped in Sept 2012, so not full year, but maybe best available?)
#https://datacatalog.cookcountyil.gov/Finance-Administration/Budget-Management-Department-General-Fund-2012-Yea/98av-9vwe
#https://datacatalog.cookcountyil.gov/Finance-Administration/Budget-Management-Department-Special-Purpose-Funds/3sep-s652

#1996-2012 General Fund Expenditures by Category (Public Safety, Health Care, General Gov't, etc)
# https://datacatalog.cookcountyil.gov/Finance-Administration/General-Fund-Expenditures-By-Category-1996-To-2012/3rn9-29p6

