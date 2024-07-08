
# directory ----

# where this file wills download
df_path <- 'inputs/CVAST books.xlsx'

# download books ----

# need to manual download books at this point
# TODO: de-bug authentication process

# drive_auth()
# drive_auth(email = 'alexander.c.brown319@gmail.com')
# drive_find('CVAST books')
# 
# drive_download('CVAST/finacial_management/CVAST books.xlsx',
#                path = df_path,
#                overwrite = T)

# read-in ----

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




