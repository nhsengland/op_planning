library(tidyverse)

target_staging <- read_csv('target_lookup_staging.csv')

plans <- read_csv('data_csvs\\Plans_2425.csv')

orgs <- plans |> 
  select(org_code,planning_ref) |> 
  unique()
#################################################################################
## when we have targets specific to organisations we will need to add those in ##
#################################################################################

target_staging_generic <- target_staging |> 
  filter(!is.null(org_code)) |> 
  select(-org_code)

target_lookup <- left_join(target_staging_generic,
          orgs,
          by = 'planning_ref')

target_lookup <- target_lookup |>  mutate(key_1 = paste0(planning_ref,measure_type),
                                          full_key = paste0(planning_ref,
                                                            measure_type,
                                                            org_code))

write_csv(target_lookup,'data_csvs\\target_lookup.csv')

rm(list=setdiff(ls(),keep))