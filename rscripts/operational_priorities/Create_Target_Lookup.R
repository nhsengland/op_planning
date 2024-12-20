library(tidyverse)

target_staging <- read_csv('target_lookup_staging.csv')

plans <- read_csv('data_csvs\\Plans_2425.csv')
op_priorities <- read_csv('data_csvs\\op_priorities_data\\op_priorities.csv')

# get the organisations where there are plans against each planning ref
orgs <- plans |> 
  select(org_code,planning_ref) |> 
  unique()

# add the organisations where we have data against non-planning operational priorities
orgs <- rbind(orgs,(op_priorities |> select(org_code,planning_ref) |> unique())) |> unique()

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

write_csv(target_lookup,paste0('data_csvs\\op_priorities_data\\target_lookup.csv'))