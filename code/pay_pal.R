
# need to manually download file
# TODO: automate signin and download process

# read-in ----

pay_pal <- fread('inputs/paypal_activity.CSV') %>% 
  rename_all(to_snake_case) %>% 
  filter(str_detect(type, 'Withdrawal', negate = T)) %>% 
  mutate_at(vars(gross, net), as.numeric) %>% 
  mutate_all(na_if,'') %>% 
  mutate(date = mdy(date),
         year = year(date),
         year_month = format(date, '%Y-%m'),
         income_type = case_when(gross == 35 | gross == 50 ~ 'membership',
                                T ~ 'donation'),
         expires_date = date + 365,
         member_status = case_when(expires_date < Sys.Date() ~ 'expired',
                                   expires_date <= Sys.Date() + 90 ~ 'expires soon',
                                   T ~ 'current member')) %>% 
  add_count(from_email_address, income_type, name = 'years_a_member')
