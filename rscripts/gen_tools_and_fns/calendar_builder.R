library(jsonlite)
library(tidyverse)


if (length(list.files('data_csvs',pattern = 'calendar', ignore.case = TRUE)) == 1) {
  file_name <- list.files('data_csvs',pattern = 'calendar', ignore.case = TRUE)
  
  calendar <- read_csv(paste0('data_csvs/',file_name),show_col_types = FALSE)
  
  rm(file_name)
  
} else if (length(list.files('data_csvs',pattern = 'calendar', ignore.case = TRUE)) > 1) {
  cat(paste0('Please ensure only one calendar data file is in the csv_exports folder, ',
             'archive duplicates and old versions\ncalendar data not imported'))  
} else {
  
bank_holiday <- "https://www.gov.uk/bank-holidays.json"
bank_holiday <- fromJSON(bank_holiday)

bank_holiday <- bank_holiday[["england-and-wales"]]
bank_holiday <- bank_holiday$events
bank_holiday <- bank_holiday$date
bank_holiday <- data.frame(date=bank_holiday)
bank_holiday <- rbind(data.frame(date=c('2017-01-02',
                                        '2017-04-14',
                                        '2017-04-17',
                                        '2017-05-01',
                                        '2017-05-29',
                                        '2017-08-28',
                                        '2017-12-25',
                                        '2017-12-26')),
                      bank_holiday)


## creates a list of the days between first April 2017 and 31 March 2025
date <- seq(as.Date('2017-01-01'), as.Date('2025-03-31'), by='days')

## bind the main dates into a data frame
calendar <-  data.frame(date)

## add useful calculated columns
calendar <- calendar |>  
  mutate(
    day_num = day(date),
    day_name = as.character(wday(date,getOption('lubridate.week.start',1))),
    month_num = month(date),
    year_num = year(date),
    month_name = as.character(month(date,label = TRUE,abbr=FALSE)),
    month_name_short = as.character(month(date,label = TRUE,abbr=TRUE)),
    month_commencing = floor_date(date,unit = 'month'),
    month_year = paste0(month_name_short,'-',as.character(year_num)),
    month_year_long = paste0(month_name," ",as.character((year_num))),
    month_short_year = paste0(month_name_short,
                              '-',
                              str_sub(as.character(year_num),
                                      str_count(year_num)-1,
                                      str_count(year_num))),
    fin_year = case_when(
      month(date)>=4 ~ paste0(as.character(year(date)),'-',substr(year(date)+1,3,4)),
      month(date)<=3 ~ paste0(as.character(year(date)-1),'-',substr(year(date),3,4))
    ),
    fin_month = case_when(
      month(date)<=3 ~ month(date)+9,
      month(date)>=4 ~ month(date)-3
    ),
    working_day = case_when(
      (wday(date,
            label=FALSE,
            getOption('lubridate.week.start',1)) %in% c(1,7)) 
      | date %in% bank_holiday ~ 'no',
      (wday(date,
            label=FALSE,
            getOption('lubridate.week.start',1)) %in% c(2,3,4,5,6)) 
      & !(date %in% bank_holiday) ~ 'yes'
    )
  )

## tidy up the vectors as don't need them now
rm(date,
   bank_holiday)

## store the calendar so that we don't have to keep hitting the API

write_csv(calendar,paste0('data_csvs\\calendar.csv'))
}

## Convert ordered columns to factors to keep explicit ordering

calendar <- calendar |> 
  mutate(fin_year = fct(fin_year,levels = unique(fin_year)),
         month_year = fct(month_year, levels = unique(month_year)),
         month_short_year = fct(month_short_year,levels = unique(month_short_year)),
         month_name = fct(month_name,levels = unique(month_name)),
         month_name_short = fct(month_name_short,levels = unique(month_name_short)))

   