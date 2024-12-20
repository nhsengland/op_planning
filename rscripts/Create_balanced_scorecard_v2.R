library(tidyverse)


plan_data <- read_csv('data_csvs\\Plans_2425.csv')
current_actuals <- read_csv('data_csvs\\current_actuals.csv')
dimension_sequence <- read_csv('data_csvs\\dimension_sequence.csv')
metrics_lookup <- read_csv('data_csvs\\measure_lookup.csv')
# target_lookup <- read_csv('data_csvs\\target_lookup.csv')

metrics_for_scorecard <- c('E.A.5','E.B.20','E.B.23c','E.B.27','E.B.28','E.B.33','E.B.35','E.M.13','E.M.38')


combined_data <- bind_rows(current_actuals,plan_data)

max_dates <- inner_join(current_actuals,
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

rm(plan_data,current_actuals)

combined_data <- left_join(combined_data,
                           dimension_sequence,
                           by = 'dimension_name')

combined_data <- inner_join(combined_data,
                            max_dates,
                            by = c('ref_key','dimension_name'))

combined_data <-  combined_data |> 
  select(icb_code,
         org_code,
         source,
         planning_ref,
         measure_type,
         dim_type,
         fin_year,
         dimension_name,
         ref_key,
         metric_value)

combined_data <- combined_data |> 
  filter(source %in% c('current_actuals','June24_plan')
         & planning_ref %in% metrics_for_scorecard)

combined_data <- combined_data |> pivot_wider(
  names_from = source,
  values_from = metric_value
)

#########################################
#### Inverting diagnostic percentages ###
#########################################

combined_data <- combined_data |> 
  mutate(current_actuals = case_when(str_sub(planning_ref,1,6) == 'E.B.28'
                                     & measure_type == 'Percentage' ~ 100-current_actuals,
                                     .default = current_actuals),
         June24_plan = case_when(str_sub(planning_ref,1,6) == 'E.B.28'
                                     & measure_type == 'Percentage' ~ 100-June24_plan,
                                     .default = June24_plan))
         

combined_data <- combined_data |> mutate(
  variance = case_when(!is.na(current_actuals) ~ current_actuals - June24_plan,
                       .default = NA))

#######################
###### rag logic ######
#######################


combined_data <- left_join(combined_data,
                           metrics_lookup |> select(ref_key,directionality,threshold1,threshold2,presentation_metric),
                           by = 'ref_key')

combined_data <- combined_data |> filter(presentation_metric=='yes') |> select(-presentation_metric)

combined_data <-  rename(combined_data, plan = June24_plan)

combined_data <- combined_data |> mutate(rag_status = case_when(
  #Aiming to be above plan
  #if you want to go up and your variance is 0 or more then good (on or above plan)
  directionality == 'over' & variance >= 0 ~ 'G', 
  #if you want to go up and your variance is below 0 but within 5% then amber
  directionality == 'over' & variance < 0 & variance >= -1*(plan*threshold2) ~ 'A', 
  #if you want to go up and your variance more than 5% below plan then red
  directionality == 'over' & variance < -1*(plan*threshold2) ~ 'R',
  
  #Aiming to be below plan
  #if you want to go down and your variance is 0 or less then good (on or below plan)
  directionality == 'under' & variance <= 0 ~ 'G', 
  #if you want to go down and your variance is above 0 but within 5% then amber
  directionality == 'under' & variance > 0 & variance <= plan*threshold2 ~ 'A', 
  #if you want to go down and your variance more than 5% above plan then red
  directionality == 'under' & variance > plan*threshold2 ~ 'R',  
  
  #Aiming to hit plan
  #if you want to hit plan and your variance is 5% or more above plan then bad (far above plan)
  directionality == 'on' & variance > plan*threshold2 ~ 'R',
  #if you want to hit plan and your variance is 5% or more below plan then bad (far below plan)
  directionality == 'on' & variance < -1*(plan*threshold2) ~ 'R',
  #if you want to hit plan and your variance is between 1% and 5% below plan then amber (not close enough)
  directionality == 'on' & variance >= -1*(plan*threshold2) & variance < -1*(plan*threshold1) ~ 'A',
  #if you want to hit plan and your variance is less than or equal to 5% above but more than 1% above plan then amber (not close enough)
  directionality == 'on' & variance <= plan*threshold2 & variance > plan*threshold1 ~ 'A',
  #if you want to hit plan and you are within + or - 1% then green (close enough)
  directionality == 'on' & variance >= -1*(plan*threshold1) & variance <= plan*threshold1 ~ 'G'
))

combined_data <- combined_data |> mutate(percent_variance = variance/plan*100)

combined_data <- combined_data |> select(-c(threshold1,threshold2))

combined_data <- combined_data |> mutate(full_key = paste0(ref_key,org_code))

write_csv(combined_data,paste0('data_csvs\\balanced_scorecard_2.csv'))

rm(list=setdiff(ls(),keep))