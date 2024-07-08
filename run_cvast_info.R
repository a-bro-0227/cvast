


# library bank ----

library(googledrive)
library(googlesheets4)
library(readxl)
library(scales)
library(tidyverse)
library(lubridate)
library(data.table)
library(openxlsx)
library(snakecase)

# run processing code ----

source('code/cvast_books.R')
source('code/pay_pal.R')

# write data ----

list_of_datasets <- list('pay_pal' = pay_pal,
                         'cvast_books' = cvast_books)

write.xlsx(list_of_datasets, file = paste0('outputs/cvast_info_',
                                           format(Sys.Date(), '%Y-%m'),
                                           '.xlsx'))
