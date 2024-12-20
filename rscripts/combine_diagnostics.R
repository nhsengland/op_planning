library(tidyverse)
# this creates overall sums for E.B.26 and E.B.28 as the plans were collected per-test
# we don't need to do it for current actuals because we create the aggregations in the SQL itself

plan_data <- read_csv('data_csvs\\Plans_2425.csv')
historic_actuals <- read_csv(paste0('from_submission_processing\\SE_historic_',submission_period,'.csv'))

fn_combine_diagnostics <- function(dataframe) {
  dataframe <- dataframe
  data <- dataframe
  
  data <- data |> 
    filter(str_sub(planning_ref,1,6) %in% c('E.B.26','E.B.28') &
           measure_type %in% c('Denominator','Numerator','Count'))
  
  data <- data |> mutate(planning_ref = str_sub(planning_ref,1,6))
  data <- data |> select(-ref_key)
  grouping <- data |> select(-metric_value) |> names()
  data <- data |> summarise(metric_value = sum(metric_value),.by =all_of(grouping))
  data <- data |> pivot_wider(names_from = measure_type,
                            values_from = metric_value)
  data <- data |> mutate(Percentage = Numerator/Denominator*100)
  data <- data |> pivot_longer(cols = c(Count,Numerator,Denominator,Percentage),
                               names_to = 'measure_type',
                               values_to = 'metric_value',
                               )
  data <- data |> mutate(ref_key = paste0(planning_ref,measure_type))
  data <- data |> 
    filter((planning_ref == 'E.B.26' & measure_type == 'Count') |
             (planning_ref == 'E.B.28' & measure_type != 'Count'))
    
  dataframe_2 <- bind_rows(dataframe,data)
  return(dataframe_2)
  }
  
plan_data <- fn_combine_diagnostics(plan_data)
historic_actuals <- fn_combine_diagnostics(historic_actuals)

write_csv(plan_data,'data_csvs\\Plans_2425.csv')
write_csv(historic_actuals,'data_csvs\\historic_actuals.csv')

rm(list=setdiff(ls(),keep))