library(RSocrata)
library(tidyverse)
library(dplyr)
library(data.table)
library(readxl)


dept_map <- read_excel("~/ME_2021/chi_budg_documentation.xlsx")
dept_map <- dept_map %>% data.frame() %>% select('department_description', 'department_clean_orig',
                                                 'department_clean_current', 'category')

years <- c(2011, 2012, 2013, 2014,2015,2016,2017,2018,2019,2020)
dataset <- c('drv3-jzqp','8ix6-nb7q','b24i-nwag','ub6s-xy6e','qnek-cfpp','36y7-5nnf','7jem-9wyw','6g7p-xnsy','h9rt-tsn7','fyin-2vyd')

cols <- c("fund_type", "fund_description","department_description", "amount","category", "year") 
full_budget <- data.frame(fund_type=character(),
          fund_description=character(),
          department_description=character(),
          amount=double(),
          category=character(),
          year=integer(),
          department_clean_orig=character(),
          department_clean_current=character(),
          year_total=double(),
          stringsAsFactors=FALSE)
#full_budget <- data.frame(matrix(nrow = 0, ncol = length(cols)))
#colnames(full_budget) <- cols

idx = 1
for(data in dataset){
  print(years[idx])
  url <- paste("https://data.cityofchicago.org/resource/", data, ".json", sep='')
  budg <- read.socrata(
    url
  )
  if(idx > 1 & idx != 3){budg <- budg %>% rename('amount' = 'X_ordinance_amount_')}
  if(idx == 3){budg <- budg %>% rename('amount' = 'appropriation_ordinance')}
  budg <- budg %>% select(c('fund_type','fund_description','department_description','amount'))
  budget <-  aggregate(as.numeric(budg$amount), 
                       by=list(budg$fund_type, budg$fund_description,
                               budg$department_description), FUN=sum, na.rm=TRUE) %>%
    rename(amount = 'x', fund_type = 'Group.1', fund_description = 'Group.2', department_description = 'Group.3') %>%
    mutate(year=years[idx])
  budget <- merge(budget, dept_map, on='department_description')
  budget['year_total'] <- sum(budget$amount)
  
  full_budget <- union(full_budget, budget)
  idx = idx + 1
}

write.csv(full_budget, 'chi_budget.csv')










