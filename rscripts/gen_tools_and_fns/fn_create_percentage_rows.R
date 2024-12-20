
# This is going to need to be a function
# first we strip out the elements that cause things not to pivot up properly
# then we filter to the planning ref (this is going to turn into a vector for the function itself)
# next we pivot wider the data so that we have numerators and denominators for planned and actual activity
# then we generate percentages for each by dividing the new fields by each other
# then we remove the numerator and denominator columns and rename the percentage columns
# then we restore measure name (again looping through another vector for names)
# then we assign the output to a list
# once the loop finishes we use bind_rows from dplyr to append the individual dataframes in the list 
# back into the first dataframe that was passed to the function
# the function then returns that enlarged dataframe


fn_create_percentage_rows <- function(df,planning_references,measure_names) {

percent_df_list <- list()

# going to need a try catch to warn if the number of planning references and the
# number of 

for (i in 1:length(planning_references)){

planning_reference <- planning_references[i] 
name_of_new_measure <- measure_names[i]


a <- df |> 
  select(-c(key,component_type,measure_id,component_name,measure_name)) |> 
  filter(planning_ref == planning_reference 
         & measure_type %in% c('Numerator','Denominator')) |> 
  pivot_wider(
    names_from = measure_type,
    values_from = c(planned_activity, actual_activity)
  ) |> 
  mutate(planned_activity = planned_activity_Numerator/planned_activity_Denominator,
         actual_activity = actual_activity_Numerator/actual_activity_Denominator,
         measure_type = 'Percentage',
         measure_name = name_of_new_measure,
         component_name = name_of_new_measure) |> 
  select(-c(planned_activity_Numerator,
            planned_activity_Denominator,
            actual_activity_Numerator,
            actual_activity_Denominator))

percent_df_list[[i]] <- a }

df <- bind_rows(df,percent_df_list)

return(df)}