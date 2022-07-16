
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

# directories ----

df_path <- 'inputs/CVAST books.xlsx'

# download books ----

drive_auth()
drive_auth(email = 'alexander.c.brown319@gmail.com')
drive_find('CVAST books')

drive_download('CVAST/finacial_management/CVAST books.xlsx',
               path = df_path,
               overwrite = T)

# read in file ----

cvast_books <- excel_sheets(df_path)[!grepl('Category', excel_sheets(df_path))] %>% 
  map(function(s) {read_excel(path = df_path, sheet = s, range = cell_cols('A:F'))}) %>% 
  bind_rows() %>% 
  rename_all(tolower) %>%
  select(-veridian, -`...6`, notes) %>% 
  filter(transaction != 'Initial Amount') %>% 
  mutate(trans_type = ifelse(amount < 0, 'Expense', 'Revenue'),
         year = year(date),
         qtr = paste0('Q', str_pad(quarter(date), width = 2, side = 'left', pad = '0')),
         month = month(date),
         year_month = format(date, '%Y-%m'),
         year_qtr = paste(year, qtr, sep = '-'))

# creating graphs ----

# revenue vs. expenses
cvast_books %>% filter(!is.na(trans_type)) %>% 
  group_by(year, trans_type) %>%
  summarize(amount = sum(amount)) %>% 
  ggplot(aes(x = year, y = abs(amount), fill = trans_type)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  xlab('Year') +
  ylab('Amount (USD)') +
  ggtitle('CVAST Revenue vs. Expenses Overtime') +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(breaks = seq(min(cvast_books$year),max(cvast_books$year),by = 1)) +
  labs(fill = 'Tansaction Type') +
  geom_text(aes(label = dollar(round(abs(amount), 0))),
            position = position_dodge(width = 0.9),
            size = 3, vjust = -0.3) +
  theme_classic()

# profit
cvast_books %>% filter(!is.na(trans_type)) %>% 
  group_by(year, trans_type) %>%
  summarize(amount = sum(amount)) %>% 
  spread(key = trans_type, value = amount) %>% 
  rename_all(to_snake_case) %>% 
  mutate(profit = revenue + expense,
         make_loss = ifelse(profit > 0, 1, 0)) %>% 
  ggplot(aes(x = year, y = profit, fill = as.character(make_loss))) +
  geom_bar(stat = 'identity') +
  xlab('Year') +
  ylab('Amount (USD)') +
  ggtitle('CVAST Profit Overtime') +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(breaks = seq(min(cvast_books$year),max(cvast_books$year),by = 1)) +
  geom_text(aes(label = dollar(round(profit, 0))),
            position = position_dodge(width = 0.9),
            size = 3, vjust = -0.3) +
  theme_classic() +
  theme(legend.position = 'none')

# detail table
cvast_books %>% 
  group_by(year, trans_type, category) %>% 
  summarise(total_amt = sum(amount)) %>% 
  spread(key = year, value = total_amt) %>% 
  arrange(desc(trans_type))

#####################################################################################

cvast_books %>% filter(!is.na(trans_type)) %>% 
  ggplot(aes(x = date, y = balance)) +
  geom_line()

uniq_desc <- cvast_books %>% distinct(transaction)



summ_year_mon <- cvast_books %>% 
  filter(year == 2021) %>% 
  group_by(year_month, trans_type, category) %>% 
  summarise(total = sum(amount, na.rm = T))




last_day <- cvast_books %>% 
  group_by(year_month) %>%
  summarise(date = max(date)) %>% 
  ungroup() %>% 
  select(date) %>% 
  mutate(last_day = T)

cvast_books <- cvast_books %>% 
  left_join(last_day, by = 'date') %>% 
  mutate(last_day = ifelse(is.na(last_day), F, T))

wb <- loadWorkbook(file = 'outputs/cvast_finacials.xlsx')
writeData(wb, sheet = 'detail', cvast_books)
saveWorkbook(wb, paste0('outputs/cvast_finacials_', format(Sys.Date(), '%Y-%m'), '.xlsx'), overwrite = T)


