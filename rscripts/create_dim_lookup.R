library(tidyverse)

plan_data <- read_csv('data_csvs\\Plans_2425.csv')
historic_actuals <- read_csv('data_csvs\\historic_actuals.csv')

source('rscripts\\gen_tools_and_fns\\calendar_builder.R')

month_seq <- calendar |> 
  select(month_short_year,fin_year,fin_month) |> 
  unique()

month_seq <- month_seq |> 
  mutate(dimension_name = as.character(month_short_year),
         fin_year = as.character(fin_year)) |> 
  mutate(dim_sequence = paste0("20",
                          substr(fin_year,nchar(fin_year)-1,nchar(fin_year)),
                          case_when(fin_month < 10 ~ paste0("0",as.character(fin_month)),
                                    .default = as.character(fin_month)))) |> 
  select(dimension_name,
         dim_sequence)

plan_data <- plan_data |> select(dimension_name)
historic_actuals <- historic_actuals |>  select(dimension_name)

date_list <- union_all(plan_data,historic_actuals) |> 
  unique()

date_list <- left_join(date_list,
                       month_seq,
                       by = 'dimension_name')

date_list <- date_list |> 
  mutate(dim_sequence = case_when(
    str_sub(dimension_name,1, 7) == 'Quarter' ~ paste0('320',
                                                       str_sub(dimension_name,
                                                               nchar(dimension_name)-1,
                                                               nchar(dimension_name)),
                                                       str_sub(dimension_name,9,9)),
    str_sub(dimension_name,1,6) == 'Apr 20' ~ paste0('4',
                                                     str_sub(dimension_name,7,8),
                                                     '04',
                                                     str_sub(dimension_name,
                                                             nchar(dimension_name)-1,
                                                             nchar(dimension_name)),
                                                     case_when(
                                                       str_sub(dimension_name,12,14) == 'Apr' ~ '01',
                                                       str_sub(dimension_name,12,14) == 'May' ~ '02',
                                                       str_sub(dimension_name,12,14) == 'Jun' ~ '03',
                                                       str_sub(dimension_name,12,14) == 'Jul' ~ '04',
                                                       str_sub(dimension_name,12,14) == 'Aug' ~ '05',
                                                       str_sub(dimension_name,12,14) == 'Sep' ~ '06',
                                                       str_sub(dimension_name,12,14) == 'Oct' ~ '07',
                                                       str_sub(dimension_name,12,14) == 'Nov' ~ '08',
                                                       str_sub(dimension_name,12,14) == 'Dec' ~ '09',
                                                       str_sub(dimension_name,12,14) == 'Jan' ~ '10',
                                                       str_sub(dimension_name,12,14) == 'Feb' ~ '11',
                                                       str_sub(dimension_name,12,14) == 'Mar' ~ '12')),
    str_detect(dimension_name,'FOT estimate') ~ paste0('50020',str_sub(dimension_name,6,7),'01'),
    str_detect(dimension_name,'FOT final') ~ paste0('50020',str_sub(dimension_name,6,7),'02'),
    .default = dim_sequence)) |> 
  mutate(dim_type = case_when(
    str_detect(dimension_name,'FOT') ~ 'FOT',
    str_detect(dimension_name,'Quarter') ~ 'Quarter',
    str_sub(dimension_name,1,6) == 'Apr 20' ~ 'Aggregate',
    .default = 'Month'
  ))


date_list <- date_list |> 
  mutate(period_grouping = case_when(
    dim_type == 'Month' ~ str_sub(dimension_name,1,3),
    dim_type == 'Quarter' ~ str_sub(dimension_name,1,9),
    dim_type == 'FOT' & str_sub(dim_sequence,9,9) == 1 ~ paste0(dim_type," Estimate"),
    dim_type == 'FOT' & str_sub(dim_sequence,9,9) == 2 ~ paste0(dim_type," Final"),
    dim_type == 'Aggregate' ~ dimension_name))

date_list <- date_list |> 
  mutate(period_sequence = case_when(
    dim_type == 'Month' ~ case_when(
        period_grouping == 'Apr' ~ 1,
        period_grouping == 'May' ~ 2,
        period_grouping == 'Jun' ~ 3,
        period_grouping == 'Jul' ~ 4,
        period_grouping == 'Aug' ~ 5,
        period_grouping == 'Sep' ~ 6,
        period_grouping == 'Oct' ~ 7,
        period_grouping == 'Nov' ~ 8,
        period_grouping == 'Dec' ~ 9,
        period_grouping == 'Jan' ~ 10,
        period_grouping == 'Feb' ~ 11,
        period_grouping == 'Mar' ~ 12),
    dim_type == 'Quarter' ~ 12+as.numeric(str_sub(period_grouping,9,9)),
    .default = as.numeric(dim_sequence)
    ))

date_list <- date_list |> 
  mutate(fin_year = case_when(
    dim_type == 'Quarter' ~ str_sub(dimension_name,11,17),
    dim_type == 'FOT' ~ str_sub(dimension_name,1,7),
    dim_type == 'Month' ~ paste0(
      as.character(as.numeric(str_sub(dim_sequence,1,4))-1),
      '/',
      str_sub(dim_sequence,3,4)),
    dim_type == 'Aggregate' ~ paste0(
      str_sub(dimension_name,5,8),
      '/',
      as.character(as.numeric(str_sub(dimension_name,7,8))+1)
      )))

date_list <- date_list |> 
  mutate(period_end_month = case_when(
    dim_type == 'Month' ~ period_grouping,
    dim_type == 'FOT' ~ period_grouping,
    dim_type == 'Quarter' & str_sub(period_grouping,9,9) == '1' ~ 'Jun',
    dim_type == 'Quarter' & str_sub(period_grouping,9,9) == '2' ~ 'Sep',
    dim_type == 'Quarter' & str_sub(period_grouping,9,9) == '3' ~ 'Dec',
    dim_type == 'Quarter' & str_sub(period_grouping,9,9) == '4' ~ 'Mar',
    dim_type == 'Aggregate' ~ str_sub(date_list$period_grouping,12,14)
  ))

date_list <- date_list |> 
  mutate(end_month_sequence = case_when(
    period_end_month == "Apr" ~ 1,
    period_end_month == "May" ~ 2,
    period_end_month == "Jun" ~ 3,
    period_end_month == "Jul" ~ 4,
    period_end_month == "Aug" ~ 5,
    period_end_month == "Sep" ~ 6,
    period_end_month == "Oct" ~ 7,
    period_end_month == "Nov" ~ 8,
    period_end_month == "Dec" ~ 9,
    period_end_month == "Jan" ~ 10,
    period_end_month == "Feb" ~ 11,
    period_end_month == "Mar" ~ 12,
    period_end_month == "FOT Estimate" ~ 13,
    period_end_month == "FOT Final" ~ 14))
    
write_csv(date_list,'data_csvs\\dimension_sequence.csv')
rm(list=setdiff(ls(),keep))