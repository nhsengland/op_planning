library(tidyverse)

folder_name <- 'actuals_from_warehouse\\live_actuals'

# get all of the file names from the datafiles folder
# we're accounting for both csv and xlsx because it controls for potential human error

file_names_csv <- list.files(folder_name,pattern = 'csv', ignore.case = TRUE)
file_names_xlsx <- list.files(folder_name,pattern = 'xlsx', ignore.case = TRUE)

# remove the file extension
#file_names <- str_remove(file_names,'.csv')

# find out how many file names you have
n_csv_names <- length(file_names_csv)

# set up an empty list to feed the data into
current_actuals <- list()


# set up a loop to extract the actuals data from each file into a list
if (n_csv_names > 0) {
  for (i in 1:n_csv_names) {
    file <- file_names_csv[[i]]
    
    live_actuals <- read_csv(file.path(folder_name,file),
                             na = c("","NA","NULL"))
    current_actuals[[i]] <- live_actuals
  
    rm(live_actuals)
  }
}

# bind everything together, as always we're doing this outside the loop because
# it's much faster

current_actuals <- bind_rows(current_actuals)

write_csv(current_actuals,'actuals_from_warehouse\\staging\\current_actuals_staging.csv')

# cleanup unneeded objects
rm(list=setdiff(ls(),keep))