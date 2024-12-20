library(tidyverse)

op_priorities <-read_csv('actuals_from_warehouse\\staging\\operational_priority_staging.csv')

# filter to just ICB figures but excluding EM38 because region is already in the data due to a different
# calculation for region meaning that aggregating ICBs gives the wrong number

region <- op_priorities |> 
  filter(str_sub(org_code,1,1) == 'Q' &
           !planning_ref %in% c('E.M.38','E.M.40','E.M.41')) 

region <- region |> 
  select(-c(org_code,icb_code))

# trim down to only those measures appropriate for aggregation
region <- region |> 
  filter(!measure_type %in% c('Mean','90th Centile'))

# now summarise
grouping <- region |> select(-metric_value) |> names()

region <-  region |> 
  summarise(metric_value = sum(metric_value),
            .by = all_of(grouping)) 

region <- region |> mutate(icb_code = 'Y59',
                           org_code = 'Y59')

op_priorities <- bind_rows(op_priorities,region)

write_csv(op_priorities,paste0('actuals_from_warehouse\\staging\\operational_priority_staging.csv_2.csv'))

rm(list=ls())