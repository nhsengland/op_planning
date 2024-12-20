## pull in or create a calendar
source('rscripts\\gen_tools_and_fns\\calendar_builder.R')

## create a separate data frame counting the number of working days per month     
monthly_working_days <- calendar |> 
  filter(working_day == 'yes') |> 
  select (month_name_short,
          month_num,
          year_num,
          fin_year,
          fin_month) |>  
  mutate(month_commencing = case_when(
    month_num < 10 ~ paste0(year_num,'-0',month_num,'-01'),
    month_num > 9 ~ paste0(year_num,'-',month_num,'-01'))) |>  
  count(month_name_short,
        month_num,
        year_num,
        month_commencing,
        fin_year,
        fin_month) |>  
  mutate(working_days = n) |> 
  select(-c(n)) |>  
  arrange(year_num,
          month_num,
          month_commencing)