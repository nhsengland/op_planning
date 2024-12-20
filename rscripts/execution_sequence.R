options(readr.show_col_types = FALSE)

submission_period <- 'June_06'
keep <- c('submission_period','keep')

# dependencies are from pulling out the monthly actuals
source('rscripts\\combine_current_actuals.R')

# depends on combined current actuals
source('rscripts\\cumulative_aggregations.R')
source('rscripts\\reshape_bed_capacity.R')
source('rscripts\\current_actuals_region.R')

# depends on current actuals region

source('rscripts\\current_actuals_add_calcs.R')

# dependencies are from submission processing
# note that create progressive aggregations also outputs the plans2425 file

source('rscripts\\Create_progressive_aggregations.R')
source('rscripts\\Create_monitoring_period_type.R')
source('rscripts\\create_measure_lookup.R')

# The following depend on Create_progressive_aggregations
source('rscripts\\reshape_bed_plans.R')
source('rscripts\\combine_diagnostics.R')

# The following depend on combine_diagnostics
source('rscripts\\Create_Target_Lookup.R')
source('rscripts\\create_dim_lookup.R')

# The following depend on create_dim_lookup AND combine_diagnostics and current_actuals
source('rscripts\\create_variance_table.R')
source('rscripts\\create_key_dates.R')
source('rscripts\\Create_balanced_scorecard_v2.R')

# The following launches a sequence to set up the operational priorities table
# This also creates the final measure lookup table

source('rscripts\\operational_priorities\\op_priorities_execution.R')
