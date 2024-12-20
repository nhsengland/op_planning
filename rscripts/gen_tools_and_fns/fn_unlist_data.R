#' This function is used to check the lists of historic data that have been
#' extracted from the historic tabs in the icb submission files. 
#' 
#' It checks whether the elements of the list are of the same length, which 
#' should be the case and if they are not it is a sign the file has been edited
#' in some way that it should not have been. If the function finds this, it 
#' prints an error to the console
#' 
#' If all of the rows are equal, then the function unpacks the list and binds 
#' the unique rows together into a single data frame which it then returns
#' 
#' @param rowcount a vector that has the row counts of each of the list elements
#' @param list a list containing each of the data frames to be unpacked and bound together
#' @return a dataframe of the unique rows from the dataframes contained in the list


unlist_data <- function(rowcount,list) {
  if(var(rowcount) == 0) {
    data_frame <- unique(bind_rows(list[[i]]))
    cat(deparse(substitute(list)),': All rows equal, all data bound.\n')
  } else {
    cat(deparse(substitute(list)),': Unequal number of rows. Check submission files.\n')
  } 
  return(data_frame)
}
