library(tidyverse)
######################################################################################
### This script takes in the control tab from phase 1 of the submission processing ###
### At time of writing the current version if from the 2nd of May submission. ########
### This will need to be reviewed if there is a new submission #######################
######################################################################################


control_tab <- read_csv(paste0('from_submission_processing\\control_tab_',submission_period,'.csv'))

control_tab <- janitor::clean_names(control_tab)

monitoring_period_type <- control_tab |> 
  select(planning_ref,
         dimension_type)

monitoring_period_type <- monitoring_period_type |> 
  rename(monitoring_period_type = dimension_type)

# now we need to create a combined planning ref for the diagnostics metrics
monitoring_period_type <- monitoring_period_type |> 
  add_row(
    planning_ref = c('E.B.28','E.B.26'),
    monitoring_period_type = c('Month','Month')
  )
################################################################################
## then we need to change the monitoring periods for the metrics that are an ###
## aggregation over the course of the year #####################################
################################################################################

# EM38 is outpatient resource usage
# EB23c is ambulance response time

monitoring_period_type[monitoring_period_type$planning_ref == 'E.M.38','monitoring_period_type'] <- 'Aggregate'

monitoring_period_type[monitoring_period_type$planning_ref == 'E.B.23c','monitoring_period_type'] <- 'Aggregate'

write_csv(monitoring_period_type,'data_csvs\\monitoring_period_type.csv')
rm(list=setdiff(ls(),keep))