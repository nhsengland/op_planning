library(tidyverse)

current_actuals <-read_csv('actuals_from_warehouse\\staging\\current_actuals_staging.csv')

region <- current_actuals |> 
  filter(str_sub(org_code,1,1) == 'Q')

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

current_actuals <- bind_rows(current_actuals,region)

write_csv(current_actuals,'actuals_from_warehouse\\staging\\current_actuals_staging_2.csv')

rm(list=setdiff(ls(),keep))