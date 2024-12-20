library(tidyverse)

current_actuals <-read_csv('actuals_from_warehouse\\staging\\current_actuals_staging.csv')
calendar <- read_csv('data_csvs\\calendar.csv')

em38 <- current_actuals |> filter(
  planning_ref == 'E.M.38'&
    measure_type %in% c('Numerator','Denominator')
)

if (nrow(em38)>0) {
col_order <- names(current_actuals)

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

current_actuals <- bind_rows(current_actuals,agg_data)

write_csv(current_actuals,'actuals_from_warehouse\\staging\\current_actuals_staging.csv')
}
# cleanup unneeded objects
rm(list=setdiff(ls(),keep))
