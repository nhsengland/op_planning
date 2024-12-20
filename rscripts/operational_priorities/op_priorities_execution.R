options(readr.show_col_types = FALSE)

source('rscripts\\operational_priorities\\combine_op_priorities.R')

source('rscripts\\operational_priorities\\op_priorities_region.R')

source('rscripts\\operational_priorities\\op_priorities_add_calcs.R')

source('rscripts\\operational_priorities\\Create_Target_Lookup.R')
# this also creates the metrics lookup final. Need to move or rename this as it is bad RAP to do two things
# in this one script
source('rscripts\\operational_priorities\\creating_op_priority_table.R')
