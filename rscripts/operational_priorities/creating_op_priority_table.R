library(tidyverse)

options(readr.show_col_types = FALSE)
current_actuals <- read_csv('data_csvs\\current_actuals.csv')
dimension_sequence <- read_csv('data_csvs\\dimension_sequence.csv')
metrics_lookup <- read_csv('data_csvs\\measure_lookup.csv')
op_priorities <- read_csv('data_csvs\\op_priorities_data\\op_priorities.csv')
target_lookup <- read_csv('data_csvs\\op_priorities_data\\target_lookup.csv')
op_priorities_addn_metrics <- read_csv('data_csvs\\op_priorities_data\\op_priority_additional_metrics_lookup.csv')

actuals <- rbind(current_actuals,op_priorities)

# get the most recent actuals for each provider

max_dates <- inner_join(actuals,
                        dimension_sequence[c('dimension_name','dim_sequence','dim_type')],
                        by = 'dimension_name')
#### Temporarily removing the YTD aggregates because they're breaking the scorecard
#### it seems like we don't have YTD aggregate plans to compare to. 

max_dates <- max_dates |> filter(dim_type != 'Aggregate')

max_dates <- max_dates[c('ref_key','dim_sequence')] 

max_dates <- max_dates |> summarise(dim_sequence = max(dim_sequence),.by = ref_key)

max_dates <- left_join(max_dates,
                       dimension_sequence[c('dimension_name','dim_sequence')],
                       by = 'dim_sequence')

max_dates <- max_dates[c('ref_key','dimension_name')] |> unique()

actuals <- inner_join(actuals,
                      max_dates,
                      by = c('ref_key','dimension_name'))

# Inverting diagnostic percentages and op_capacity_use metrics

actuals <- actuals |> 
  mutate(metric_value = case_when(str_sub(planning_ref,1,6) == 'E.B.28'
                                     & measure_type == 'Percentage' ~ 100-metric_value,
                                  planning_ref == 'E.M.38'
                                  & measure_type == 'Percentage' ~ 100-metric_value,
                                     .default = metric_value),
         full_key = paste0(ref_key,org_code))

# Adding goals

actuals <- left_join(actuals,
                     target_lookup |> select(Goal, full_key),
                     by = 'full_key')

actuals <- actuals |> filter(!is.na(Goal))

# calculating variance

actuals <- actuals |> 
  mutate(variance = metric_value - Goal)
         
# adding rag ratings

directionality = c(
  'AE_attends_op_priority' = 'over'
)

op_priorities_addn_metrics <- op_priorities_addn_metrics |> 
  mutate(ref_key = paste0(planning_ref,measure_type),
         directionality = case_when(presentation_metric == 'yes' ~ directionality[planning_ref],
                                  .default = NA),
         threshold1 = case_when(presentation_metric == 'yes' ~ 0.01),
         threshold2 = case_when(presentation_metric == 'yes' ~ 0.05))

metrics_lookup <- rbind(metrics_lookup,op_priorities_addn_metrics)

actuals <- left_join(actuals,
                           metrics_lookup |> select(ref_key,directionality,threshold1,threshold2,presentation_metric),
                           by = 'ref_key')

actuals <- actuals |> mutate(rag_status = case_when(
  #Aiming to be above goal
  #if you want to go up and your variance is 0 or more then good (on or above goal)
  directionality == 'over' & variance >= 0 ~ 'G', 
  #if you want to go up and your variance is below 0 but within 5% then amber
  directionality == 'over' & variance < 0 & variance >= -1*(Goal*threshold2) ~ 'A', 
  #if you want to go up and your variance more than 5% below goal then red
  directionality == 'over' & variance < -1*(Goal*threshold2) ~ 'R',
  
  #Aiming to be below goal
  #if you want to go down and your variance is 0 or less then good (on or below goal)
  directionality == 'under' & variance <= 0 ~ 'G', 
  #if you want to go down and your variance is above 0 but within 5% then amber
  directionality == 'under' & variance > 0 & variance <= Goal*threshold2 ~ 'A', 
  #if you want to go down and your variance more than 5% above goal then red
  directionality == 'under' & variance > Goal*threshold2 ~ 'R',  
  
  #Aiming to hit goal
  #if you want to hit goal and your variance is 5% or more above goal then bad (far above goal)
  directionality == 'on' & variance > Goal*threshold2 ~ 'R',
  #if you want to hit goal and your variance is 5% or more below goal then bad (far below goal)
  directionality == 'on' & variance < -1*(Goal*threshold2) ~ 'R',
  #if you want to hit goal and your variance is between 1% and 5% below goal then amber (not close enough)
  directionality == 'on' & variance >= -1*(Goal*threshold2) & variance < -1*(Goal*threshold1) ~ 'A',
  #if you want to hit goal and your variance is less than or equal to 5% above but more than 1% above goal then amber (not close enough)
  directionality == 'on' & variance <= Goal*threshold2 & variance > Goal*threshold1 ~ 'A',
  #if you want to hit goal and you are within + or - 1% then green (close enough)
  directionality == 'on' & variance >= -1*(Goal*threshold1) & variance <= Goal*threshold1 ~ 'G'
))

# calculate percent variance then remove infinite value errors
actuals <- actuals |> mutate(percent_variance = variance/Goal*100)
actuals <- actuals |> mutate(percent_variance = case_when(is.infinite(percent_variance) ~ NA,
                                                          .default = percent_variance))


actuals <- actuals |> select(-c(threshold1,threshold2))

actuals <- actuals |> mutate(full_key = paste0(ref_key,org_code))

actuals <- actuals |> select(
  icb_code,
  org_code,
  planning_ref,
  measure_type,
  dimension_name,
  ref_key,
  metric_value,
  Goal,
  variance,
  directionality,
  rag_status,
  percent_variance,
  full_key)

actuals <- actuals |> rename(latest_position = metric_value,
                             goal = Goal) 

write_csv(actuals,'data_csvs\\Operational_priorities.csv')
write_csv(metrics_lookup,'data_csvs\\measure_lookup_final.csv')
