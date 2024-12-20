
metrics_lookup <- read_csv(paste0('from_submission_processing\\SE_metrics_lookup',submission_period,'.csv'),
                           show_col_types = FALSE)
#staging_1 <- read_csv('actuals_from_warehouse\\staging\\current_actuals_staging.csv',
#                      show_col_types = FALSE)
current_actuals <- read_csv('actuals_from_warehouse\\staging\\current_actuals_staging_2.csv',
                            show_col_types = FALSE)

# We are iteratively adding new metrics in so we need to be dynamic about the 
# measure types we will need to calculate 

available_refs <- current_actuals |> select(planning_ref) |> unique()

ref_types <- metrics_lookup |> select(planning_ref,measure_type) |> unique() |> filter(measure_type != '90th centile')

ref_measures <- inner_join(ref_types,
                           available_refs,
                           by = "planning_ref")

measure_types <- unique(ref_measures$measure_type)

current_actuals <- current_actuals |> 
  pivot_wider(names_from = measure_type,
              values_from = metric_value)

percentages <- ref_measures |> filter(measure_type == 'Percentage') |> select(planning_ref) |> unique()
rates <- ref_measures |> filter(measure_type == 'Rate') |> select(planning_ref) |> unique()

if (nrow(percentages) > 0) {
current_actuals <- current_actuals |> 
  mutate(Percentage = case_when(
    planning_ref %in% percentages$planning_ref ~ Numerator/Denominator*100,
    .default = NA))
}

if (nrow(rates) > 0) {
  current_actuals <- current_actuals |>  
    mutate(Rate = case_when(
      planning_ref %in% rates$planning_ref ~ Numerator/Denominator*1000000,
      .default = NA))  
}

# now we put the data back to long

current_actuals <- current_actuals |> 
  pivot_longer(cols = all_of(measure_types),
               names_to = 'measure_type',
               values_to = 'metric_value',
               values_drop_na = TRUE)

# deal with infinite value errors 

current_actuals <- current_actuals |> 
  mutate(metric_value = case_when(metric_value == 'Inf' ~ NA,
                                  .default = metric_value))

# finally add in the reference key 

current_actuals <- current_actuals |> 
  mutate(ref_key = paste0(planning_ref,measure_type))

write_csv(current_actuals,'data_csvs\\current_actuals.csv')

rm(list=setdiff(ls(),keep))