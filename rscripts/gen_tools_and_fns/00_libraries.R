if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               rmarkdown,
               knitr,
               readxl,
               writexl,
               formattable,
               flextable,
               odbc,
               officer,
               jsonlite,
               #gt,
               #gtsummary,
               plotly,
               htmltools,
               janitor,
               hms)

message("package load complete")



