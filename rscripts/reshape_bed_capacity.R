library(tidyverse)

# We are now going to create denominators for the individual adult and child bed occupancy

# Take in the combined actuals data

actuals <- read_csv('actuals_from_warehouse\\staging\\current_actuals_staging.csv',
                    show_col_types = FALSE)

# Aggregations that need to happen: 
  # E.M.30a needs EM30g and EM30h
  # E.M.30b needs EM30i and EM30j

# then we need to sum up system and region totals

grouping_names <- names(actuals |> select(!metric_value))

EM30a_denom <- actuals |> 
  filter(planning_ref %in% c('E.M.30g','E.M.30h')) |> 
  mutate(planning_ref = 'E.M.30a') |> 
  summarise(metric_value = sum(metric_value),
            .by = all_of(grouping_names))

EM30b_denom <- actuals |> 
  filter(planning_ref %in% c('E.M.30i','E.M.30j')) |> 
  mutate(planning_ref = 'E.M.30b') |> 
  summarise(metric_value = sum(metric_value),
            .by = all_of(grouping_names))

actuals <- bind_rows(actuals,EM30a_denom,EM30b_denom)

system_em30 <- actuals |> 
  filter(str_sub(planning_ref,1,6)=='E.M.30') |> 
  mutate(org_code = icb_code) |> 
  summarise(metric_value = sum(metric_value),.by = all_of(grouping_names))

#region_em30 <- system_em30 |>   
#  filter(str_sub(planning_ref,1,6)=='E.M.30') |> 
#  mutate(icb_code = 'Y59',
#         org_code = 'Y59') |> 
#  summarise(metric_value = sum(metric_value),.by = all_of(grouping_names))

actuals <- rbind(actuals,system_em30)#,region_em30)

write_csv(actuals,'actuals_from_warehouse\\staging\\current_actuals_staging.csv')
# cleanup unneeded objects
rm(list=setdiff(ls(),keep))