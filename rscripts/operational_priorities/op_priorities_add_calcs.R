
op_priorities <- read_csv('actuals_from_warehouse\\staging\\operational_priority_staging.csv_2.csv',
                            show_col_types = FALSE)

measure_types <- op_priorities |> select(measure_type) |> unique()

# create percentages 
op_priorities <- op_priorities |> 
  pivot_wider(names_from = measure_type,
              values_from = metric_value)

op_priorities <- op_priorities |> 
    mutate(Percentage = case_when(is.nan(Numerator/Denominator*100) ~ NA,
                                  .default = (Numerator/Denominator*100))
           )

measure_types <- rbind(measure_types,'Percentage')

# now we put the data back to long

op_priorities <- op_priorities |> 
  pivot_longer(cols = all_of(measure_types$measure_type),
               names_to = 'measure_type',
               values_to = 'metric_value',
               values_drop_na = TRUE)

# deal with infinite value errors 

op_priorities <- op_priorities |> 
  mutate(metric_value = case_when(metric_value == 'Inf' ~ NA,
                                  .default = metric_value))

# finally add in the reference key 

op_priorities <- op_priorities |> 
  mutate(ref_key = paste0(planning_ref,measure_type))

write_csv(op_priorities,paste0('data_csvs\\op_priorities_data\\op_priorities.csv'))

rm(list=ls())