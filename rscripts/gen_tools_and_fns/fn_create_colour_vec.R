
fn_create_colour_vec <- function(df,goal_direction,bad_colour,good_colour,neutral_colour){
  df <- arrange(df,org_short_name,month_short_year)
  
  df <- if(goal_direction == 'decrease'){
    df |> mutate(vec_col = case_when(
      is.na(planned_activity) ~ neutral_colour,
      planned_activity < actual_activity ~ bad_colour,
      planned_activity >= actual_activity ~ good_colour))
  } else if(goal_direction == 'increase'){
    df |> mutate(vec_col = case_when(
      is.na(planned_activity) ~ neutral_colour,
      planned_activity > actual_activity ~ bad_colour,
      planned_activity <= actual_activity ~ good_colour))  
  }
  
  a <- df$vec_col
  
  a
} 


