################

#### 
# We need both the variance and the percent variance
# Percent variance will need to be calculated differently for percentages (straight variance)
# than for other metrics
###

library(tidyverse)

plan_data <- read_csv('data_csvs\\Plans_2425.csv')
current_actuals <- read_csv('data_csvs\\current_actuals.csv')

#---------------------------------------------------------------

#Load in uplift data
uplift_data <- read_csv('data_csvs\\plans_uec_uplift.csv') %>%
  select(c(org_code, measure_type, planning_ref, uplift)) %>%
  filter (substr(org_code,1,1) == 'Y' | substr(org_code,1,1) == 'Q') 

# Join uplift data to plans
plan_data2 <- left_join(plan_data,uplift_data,
                        by = c("org_code"="org_code",
                               "measure_type" = "measure_type",
                               "planning_ref" = "planning_ref"))

# Remove NA introduced by join
plan_data2 <- plan_data2 %>%
  replace(is.na(.), 0)

# Add uplift to plan data
plan_data2 <- plan_data2 %>%
  mutate(metric_value = metric_value + uplift) %>%
  select(-uplift)

#---------------------------------------------------------------

combined_data <- bind_rows(current_actuals,plan_data)

#pull out some lists of useful items for later
col_names <- names(combined_data)
planning_events <-  plan_data |> select(source) |> unique() |> unlist()

wide <- combined_data |> 
  pivot_wider(names_from = source, values_from = metric_value)

# calculate absolute variance 
# ~current_actuals - . is going row by row and for each column ending with _plan 
# it creates a column contining the value of the current actuals minus the column ending in plan.
# the . is the syntax standing in for "each thing ending with plan" 

wide <- wide |> 
  mutate(across(ends_with("_plan"), ~current_actuals - ., .names = "{col}-total_var"))

# calculate percentage variance
# same idea note that we have two . placeholders because we are doing 
# (current_actuals - [plan_column]) / [plan_column]
# for some reason we cannot just reference the total_var column created above, 
# don't know why got fed up trying to figure it out.
# note that where the column is already a percentage we are copying the logic for the
# total variance

wide <- wide |> 
  mutate(case_when(measure_type != 'Percentage' ~ across(ends_with("_plan"), ~ (current_actuals - .) / ., .names = '{col}-perc_var'),
                   measure_type == 'Percentage' ~ across(ends_with("_plan"), ~current_actuals - ., .names = '{col}-perc_var')))


wide <- wide |> select(!c('current_actuals',all_of(planning_events)))

long <- wide |> pivot_longer(cols = contains('_plan-'),
                             names_to = c("source",".value"),
                             names_sep = '-',
                             values_drop_na = TRUE)

#replace the infinite value errors

long <- long |> mutate(perc_var = case_when(is.infinite(perc_var) ~ NA,
                                            .default = perc_var))

write_csv(long,'data_csvs\\variance_from_plan.csv')
rm(list=setdiff(ls(),keep))
