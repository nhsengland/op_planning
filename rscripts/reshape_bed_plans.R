library(tidyverse)

plan_data <- read_csv('data_csvs\\Plans_2425.csv')

# need to build in aggregations for the bed capacity elements to match
# the aggregations we created way back in "reshape_bed_capacity"
# we will also need to calculate their percentages

# pull out the denominators
bed_aggs <- plan_data |> 
  filter(planning_ref %in% c('E.M.30g','E.M.30h','E.M.30i','E.M.30j')) |> 
  select(!ref_key)

grouping_names <- names(bed_aggs |> select(!metric_value))

EM30a_denom <- bed_aggs |> 
  filter(planning_ref %in% c('E.M.30g','E.M.30h')) |> 
  mutate(planning_ref = 'E.M.30a') |> 
  summarise(metric_value = sum(metric_value),
            .by = all_of(grouping_names))

EM30b_denom <- bed_aggs |> 
  filter(planning_ref %in% c('E.M.30i','E.M.30j')) |> 
  mutate(planning_ref = 'E.M.30b') |> 
  summarise(metric_value = sum(metric_value),
            .by = all_of(grouping_names))

# pull out the numerators
EM30x_numerators <- plan_data |> 
  filter(planning_ref %in% c('E.M.30a','E.M.30b')) |> 
  select(!ref_key)

bed_aggs <- bind_rows(EM30x_numerators,EM30a_denom,EM30b_denom)

#create percentages
bed_aggs <- bed_aggs |> 
  pivot_wider(names_from = measure_type,
              values_from = metric_value) |> 
  mutate(Percentage = Numerator/Denominator*100) |> 
  pivot_longer(!c(icb_code,org_code,dimension_name,source,planning_ref),
               names_to = 'measure_type',
               values_to = 'metric_value') |> 
  mutate(ref_key = paste0(planning_ref,measure_type))

# remove numerators to avoid duplication
bed_aggs <- bed_aggs |> filter(!measure_type == 'Numerator')

plan_data <- bind_rows(plan_data,bed_aggs)


write_csv(plan_data,'data_csvs\\Plans_2425.csv')

# cleanup unneeded objects
rm(list=setdiff(ls(),keep))