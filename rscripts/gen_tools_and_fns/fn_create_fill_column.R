
fn_create_fill_column <- function(df,goal_direction,bad_colour,good_colour,neutral_colour){
df <- if(goal_direction == 'increase'){
  df |> mutate(fill_col = case_when(
    is.na(planned_activity) ~ neutral_colour,
    planned_activity < actual_activity ~ bad_colour,
    planned_activity >= actual_activity ~ good_colour))
} else if(goal_direction == 'decrease'){
  df |> mutate(fill_col = case_when(
    is.na(planned_activity) ~ neutral_colour,
    planned_activity > actual_activity ~ bad_colour,
    planned_activity <= actual_activity ~ good_colour))  
}} 