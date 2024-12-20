library(tidyverse)

plan_data <- read_csv(paste0('from_submission_processing\\SE_plans_',submission_period,'.csv'))
calendar <- read_csv('data_csvs\\calendar.csv')

em38 <- plan_data |> filter(
  planning_ref == 'E.M.38'&
    measure_type %in% c('Numerator','Denominator')
)

col_order <- names(plan_data)

months <- calendar |> 
  select(month_short_year,
         year_num,
         month_name_short,
         fin_month,
         fin_year) |> 
  unique()

em38 <- inner_join(em38,
                   months,
                   by = c('dimension_name' = 'month_short_year'))
aggs_list <- list()

for(i in 1:max(em38$fin_month)) {

  a <- em38 |> filter(between(fin_month,1,i))
  b <- a |> select(month_name_short, fin_month, year_num) |> unique()
  lower_range <- b |> filter(fin_month == min(b$fin_month))
  lower_range <- paste0(lower_range$month_name_short,' ',lower_range$year_num)
  upper_range <- b |> filter(fin_month == max(b$fin_month))  
  upper_range <- paste0(upper_range$month_name_short,' ',upper_range$year_num)
  range <- paste(lower_range,upper_range,sep = ' - ')
  rm(b,lower_range,upper_range)
  a <- a |> select(-c(dimension_name,
                      year_num,
                      month_name_short,
                      fin_month,
                      fin_year))
  grouping <- a |> select(-metric_value) |> names()
  
  agg <- a |> summarise(metric_value = sum(metric_value),.by =all_of(grouping))
  
  rm(a)
  
  agg <- agg |> mutate(dimension_name = range) |> select(all_of(col_order))
  
  aggs_list[[i]] <- agg
  
  rm(agg,range)
}

agg_data <- bind_rows(aggs_list)

rm(aggs_list,i)

agg_data <- agg_data |> 
  select(-ref_key) |> 
  pivot_wider(names_from = measure_type,
              values_from = metric_value) |> 
  mutate(Percentage = Numerator/Denominator*100) |> 
  pivot_longer(!c(icb_code,org_code,dimension_name,source,planning_ref),
               names_to = 'measure_type',
               values_to = 'metric_value') |> 
  mutate(ref_key = paste0(planning_ref,measure_type))

plan_data <- bind_rows(plan_data,agg_data)

write_csv(plan_data,'data_csvs\\Plans_2425.csv')

# cleanup unneeded objects
rm(list=setdiff(ls(),keep))