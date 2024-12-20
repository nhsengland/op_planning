#create latest actual list

plan_data <- read_csv(paste0('data_csvs\\Plans_2425.csv'))
historic_actuals <- read_csv('data_csvs\\historic_actuals.csv')
dimension_sequence <- read_csv('data_csvs\\dimension_sequence.csv')
metrics_lookup <- read_csv(paste0('from_submission_processing\\SE_metrics_lookup',submission_period,'.csv'))

#These variables will be used to generate the plan end dates. The main end date will typically be 
#for March for that planning year.
#The variants exist for any earlier end dates that have been assigned. 

main_end_date <- 202512
variant_end_date_1 <- 202506
source_var <- 'June24_plan'

all_data <- union_all(plan_data,historic_actuals)

all_data <- left_join(all_data,
                      dimension_sequence,
                      by = "dimension_name") 

all_data <- all_data |> 
  filter(!dim_type %in% c('FOT','Aggregate'))

plans <- all_data |> 
  filter(source == source_var) |> 
  select(ref_key,
         dim_sequence)
  
# manually controlling for historic actuals outside of range, due to commissioned
# units of dental activity messing stuff up
historic <- all_data |> 
  filter(source == 'historic_actuals' &
           metric_value >0 &
           !dimension_name %in% c('Quarter 1 2024/25',
                                  'Quarter 2 2024/25',
                                  'Quarter 3 2024/25',
                                  'Quarter 4 2024/25')) |> 
  select(ref_key,
         dim_sequence) 
 
#this gets the latest period in which a non-zero historic value is available 
historic <- historic |> 
  summarise(key_date = max(dim_sequence),
            .by = all_of(c('ref_key'))) |> 
  mutate(key_date_flag = 'Latest Actual')
  
# this flags the march delivery dates for everything except 65ww where it will be 
# september
plans <- plans |> 
  summarise(key_date = max(dim_sequence),
            .by = all_of(c('ref_key'))) |>  
  mutate(key_date = case_when(
    ref_key == 'E.B.20Count' ~ variant_end_date_1,
    .default = key_date)) |> 
  mutate(key_date_flag = 'Plan End')

key_dates <- union_all(historic,
                       plans)

key_dates <- left_join(key_dates,
                       dimension_sequence,
                       by = c('key_date' = 'dim_sequence'))
# now we need to join the metric lookup in order to create a key field that 
# works with the power BI data

mets_columns <- metrics_lookup |> 
  select(ref_key,
         planning_ref,
         measure_type)

key_dates <- left_join(key_dates,
                       mets_columns,
                       by = 'ref_key') 

key_dates <- key_dates |> 
  mutate(key_1 = paste0(planning_ref,measure_type,dimension_name))
                       
write_csv(key_dates,'data_csvs\\key_dates.csv')  

rm(list=setdiff(ls(),keep))