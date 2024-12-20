library(tidyverse)

# This script takes the metrics lookup created by the submission processing file 
# then adds elements that are not present in the submission files and are only
# needed for plan monitoring.

# This could probably replace the 08 step in the submission processing process
# but I don't have time to properly refactor things now

# This is also an opportunity to change the activity category column to clean up 
# the number of categories and remove the 'NA's

metrics_lookup <- read_csv(paste0('from_submission_processing\\SE_metrics_lookup',submission_period,'.csv'))

# add additional rows where new metrics have or will be created as part of this process

fn_append_item <- function(df,activity_category,component_name,component_type,
                           granularity,measure_id,measure_subject,measure_name,
                           measure_short_name,measure_type,planning_ref,
                           presentation_metric) {
  #turn arguments that are not df into a dataframe and create the reference key
  to_append <- data.frame(
    activity_category = activity_category,
    component_name = component_name,
    component_type = component_type,
    granularity = granularity,
    measure_id = measure_id,
    measure_subject = measure_subject,
    measure_name = measure_name,
    measure_short_name = measure_short_name,
    measure_type = measure_type,
    planning_ref = planning_ref,
    presentation_metric = presentation_metric,
    ref_key = paste0(planning_ref,measure_type))
  
  # get the column order of the main dataframe
  col_order <- names(df)
  # explicitly organise the to_append dataframe into the order of the main dataframe
  to_append <- to_append |> select(all_of(col_order))
  # bind the new elements into the dataframe then return it
  x <- rbind(df,to_append)

  return(x)
}
                             
metrics_lookup <- fn_append_item(metrics_lookup,
                                 activity_category =	'Beds',
                                 component_name =	'Average number of overnight G&A beds available - adult',
                                 component_type =	'Numerator',
                                 granularity	= 'Provider',
                                 measure_id =	'SE_EM30a_D',
                                 measure_subject =	'Capacity',
                                 measure_name	= 'General and Acute overnight bed occupancy - adult',
                                 measure_short_name	= 'G&A core beds overnight',
                                 measure_type	= 'Denominator',
                                 planning_ref	= 'E.M.30a',
                                 presentation_metric	= 'no')

metrics_lookup <- fn_append_item(metrics_lookup,
                                 activity_category =	'Beds',
                                 component_name =	'Average number of overnight G&A beds available - paediatric',
                                 component_type =	'Numerator',
                                 granularity	= 'Provider',
                                 measure_id =	'SE_EM30b_D',
                                 measure_subject =	'Capacity',
                                 measure_name	= 'General and Acute overnight bed occupancy - paediatric',
                                 measure_short_name	= 'G&A core beds overnight',
                                 measure_type	= 'Denominator',
                                 planning_ref	= 'E.M.30b',
                                 presentation_metric	= 'no')

# create a directionality column that will tell subsequent processes whether 
# being over plan is desired, or under plan is better. On indicates that over or under are bad

# create lists of the planning references where over or under are desired. 

directionality <- c(
  'E.A.4a' = 'over',
  'E.A.4b' = 'over',
  'E.A.5' = 'under',
  'E.B.18' = 'under',
  'E.B.20' = 'under',
  'E.B.23c' = 'under',
  'E.B.24' = 'under',
  'E.B.26' = 'on',
  'E.B.26a' = 'on',
  'E.B.26b' = 'on',
  'E.B.26c' = 'on',
  'E.B.26d' = 'on',
  'E.B.26e' = 'on',
  'E.B.26f' = 'on',
  'E.B.26g' = 'on',
  'E.B.26h' = 'on',
  'E.B.26k' = 'on',
  'E.B.27' = 'over',
  'E.B.28' = 'over',
  'E.B.28a' = 'over',
  'E.B.28b' = 'over',
  'E.B.28c' = 'over',
  'E.B.28d' = 'over',
  'E.B.28e' = 'over',
  'E.B.28f' = 'over',
  'E.B.28g' = 'over',
  'E.B.28h' = 'over',
  'E.B.28k' = 'over',
  'E.B.33' = 'over',
  'E.B.34' = 'over',
  'E.B.35' = 'over',
  'E.B.3a' = 'under',
  'E.D.19' = 'over',
  'E.D.21' = 'over',
  'E.D.22' = 'over',
  'E.D.23' = 'over',
  'E.D.24' = 'on',
  'E.H.13' = 'over',
  'E.H.15' = 'on',
  'E.H.31' = 'over',
  'E.H.9' = 'on',
  'E.K.3' = 'over',
  'E.M.10' = 'on',
  'E.M.10a' = 'on',
  'E.M.10b' = 'on',
  'E.M.10c' = 'on',
  'E.M.10d' = 'on',
  'E.M.11' = 'on',
  'E.M.11a' = 'on',
  'E.M.11b' = 'on',
  'E.M.15' = 'on',
  'E.M.13' = 'over',
  'E.M.13a' = 'over',
  'E.M.13b' = 'over',
  'E.M.18' = 'over',
  'E.M.19' = 'on',
  'E.M.20' = 'over',
  'E.M.25' = 'under',
  'E.M.26b' = 'on',
  'E.M.26c' = 'on',
  'E.M.29' = 'under',
  'E.M.30' = 'on',
  'E.M.30a' = 'on',
  'E.M.30b' = 'on',
  'E.M.30g' = 'on',
  'E.M.30h' = 'on',
  'E.M.30i' = 'on',
  'E.M.30j' = 'on',
  'E.M.32' = 'on',
  'E.M.32' = 'over',
  'E.M.38' = 'over', #THIS ASSUMES YOUR GOING TO INVERT THE PERCENTAGE!!!!
  'E.M.40' = 'over',
  'E.M.41' = 'over',
  'E.M.72a' = 'on',
  'E.M.72b' = 'on',
  'E.M.8' = 'over',
  'E.M.9' = 'under',
  'E.T.2' = 'under',
  'E.T.2a' = 'under',
  'E.T.2b' = 'under',
  'E.T.3' = 'on',
  'E.T.3a' = 'on',
  'E.T.3b' = 'on',
  'E.T.3c' = 'on',
  'E.T.3c.i' = 'on',
  'E.T.3c.ii' = 'on',
  'E.T.3c.iii' = 'on',
  'E.T.3c.iv' = 'on',
  'E.T.3d' = 'on',
  'E.T.5' = 'on',
  'E.T.6' = 'on',
  'E.T.7' = 'under',
  'E.T.8' = 'on',
  'E.T.9' = 'under',
  'E.T.9a' = 'under',
  'E.T.9b' = 'under')

# add in a directionality column

metrics_lookup <- metrics_lookup |> 
  mutate(directionality = case_when(presentation_metric == 'yes' ~ directionality[planning_ref],
                                    .default = NA))

# now we are tidying up the activity categories so that we have fewer and the names make sense
# as before create a list 

activity_cats = c(
  'E.A.4a' = 'Mental Health',
  'E.A.4b' = 'Mental Health',
  'E.A.5' = 'Mental Health',
  'E.A.S.1' = 'Mental Health',
  'E.B.18' = 'RTT',
  'E.B.20' = 'RTT',
  'E.B.23c' = 'Ambulance',
  'E.B.24' = 'RTT',
  'E.B.26a' = 'Diagnostics',
  'E.B.26b' = 'Diagnostics',
  'E.B.26c' = 'Diagnostics',
  'E.B.26d' = 'Diagnostics',
  'E.B.26e' = 'Diagnostics',
  'E.B.26f' = 'Diagnostics',
  'E.B.26g' = 'Diagnostics',
  'E.B.26h' = 'Diagnostics',
  'E.B.26k' = 'Diagnostics',
  'E.B.27' = 'Cancer',
  'E.B.28a' = 'Diagnostics',
  'E.B.28b' = 'Diagnostics',
  'E.B.28c' = 'Diagnostics',
  'E.B.28d' = 'Diagnostics',
  'E.B.28e' = 'Diagnostics',
  'E.B.28f' = 'Diagnostics',
  'E.B.28g' = 'Diagnostics',
  'E.B.28h' = 'Diagnostics',
  'E.B.28k' = 'Diagnostics',
  'E.B.33' = 'Cancer',
  'E.B.34' = 'Cancer',
  'E.B.35' = 'Cancer',
  'E.B.3a' = 'RTT',
  'E.D.19' = 'GP',
  'E.D.21' = 'GP',
  'E.D.21' = 'GP Appoint',
  'E.D.22' = 'Dentistry',
  'E.D.22' = 'GP',
  'E.D.23' = 'Dentistry',
  'E.D.23' = 'GP',
  'E.D.24' = 'Dentistry',
  'E.D.24' = 'Dental activity',
  'E.H.13' = 'Mental Health',
  'E.H.15' = 'Mental Health',
  'E.H.31' = 'Mental Health',
  'E.H.9' = 'Mental Health',
  'E.K.1' = 'Learning Disabilities and Autism',
  'E.K.1c' = 'Learning Disabilities and Autism',
  'E.K.3' = 'Learning Disabilities and Autism',
  'E.M.10' = 'Electives',
  'E.M.10a' = 'Electives',
  'E.M.10b' = 'Electives',
  'E.M.10c' = 'Electives',
  'E.M.10d' = 'Electives',
  'E.M.11' = 'Non-Electives',
  'E.M.11a' = 'Non-Electives',
  'E.M.11b' = 'Non-Electives',
  'E.M.13' = 'A&E',
  'E.M.13a' = 'A&E',
  'E.M.13b' = 'A&E',
  'E.M.15' = 'Emergency care',
  'E.M.18' = 'RTT',
  'E.M.19' = 'RTT',
  'E.M.20' = 'RTT',
  'E.M.25' = 'UEC',
  'E.M.26b' = 'Beds',
  'E.M.26c' = 'Beds',
  'E.M.29' = 'Beds',
  'E.M.30' = 'Beds',
  'E.M.30a' = 'Beds',
  'E.M.30b' = 'Beds',
  'E.M.30g' = 'Beds',
  'E.M.30h' = 'Beds',
  'E.M.30i' = 'Beds',
  'E.M.30j' = 'Beds',
  'E.M.32' = 'Outpatients',
  'E.M.34' = 'Outpatients',
  'E.M.38' = 'Outpatients',
  'E.M.40' = 'Outpatients',
  'E.M.41' = 'Outpatients',
  'E.M.72a' = 'Community pharmacy',
  'E.M.72b' = 'Community pharmacy',
  'E.M.8' = 'Outpatients',
  'E.M.9' = 'Outpatients',
  'E.T.2' = 'Community',
  'E.T.2a' = 'Community',
  'E.T.2b' = 'Community',
  'E.T.3' = 'Community',
  'E.T.3a' = 'Community',
  'E.T.3b' = 'Community',
  'E.T.3c' = 'Community',
  'E.T.3c.i' = 'Community',
  'E.T.3c.ii' = 'Community',
  'E.T.3c.iii' = 'Community',
  'E.T.3c.iv' = 'Community',
  'E.T.3d' = 'Community',
  'E.T.5' = 'Community',
  'E.T.6' = 'Community',
  'E.T.7' = 'Community',
  'E.T.8' = 'Community',
  'E.T.9' = 'Community',
  'E.T.9a' = 'Community',
  'E.T.9b' = 'Community')

metrics_lookup <- metrics_lookup |> mutate(acat = activity_cats[planning_ref])

# the named list doesn't contain the activity categories for metrics created during
# submission processing, but we like those because we created them so we are just 
# going to pull them across to the new column then rewrite the activity_category 
# column with the contents of the new acat column. Finally we will drop the now
# duplicate acat column

metrics_lookup <- metrics_lookup |> mutate(acat = case_when(is.na(acat) ~ activity_category,
                                  .default = acat))

metrics_lookup <- metrics_lookup |> mutate(activity_category = acat)

metrics_lookup <- metrics_lookup |> select(-acat)

# Next we are changing the measure short name for the ambulance metrics because
# these are currently "NA"

metrics_lookup <- metrics_lookup |> mutate(measure_short_name = case_when(
  planning_ref == 'E.B.23c' ~ 'Cat 2 Response',
  .default = measure_short_name)
)

#  same for component name but we've only got one NA there so we will be more specific

metrics_lookup <- metrics_lookup |> mutate(component_name = case_when(
  planning_ref == 'E.B.23c' & measure_type == '90th Centile' ~ 'Category 2 - 90th Centile ambulance response time',
  .default = component_name)
)

# Next we are setting up a case_when inside a mutate to set the RAG threshold for comparison to plan.
# At the moment there is no threshold for measures that are not presentation metrics, 
# and for everything that is a presentation metric the thresholds are the same:
# Green if on plan or better than we hoped (so over if up is good, under if it is bad)
# Amber if not achieving but within 5% of plan
# Red if not achieving and over 5% off plan
# We could do this as a blanket column but I'm assuming we will have some bespoke thresholds down the line
# if that happens we can add a specific case_when line and set everything else where the presentation metric is 
# yes as 0.05. 
# We will use this column in the benchmarking_2 script.

metrics_lookup <-  metrics_lookup |> 
  mutate(threshold1 = case_when(presentation_metric == 'yes' ~ 0.01),
         threshold2 = case_when(presentation_metric == 'yes' ~ 0.05))

write_csv(metrics_lookup,'data_csvs\\measure_lookup.csv')

rm(list=setdiff(ls(),keep))