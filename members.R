
# read-in
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
         expires_date = date + 365) %>% 
  add_count(from_email_address, income_type, name = 'years_a_member')

# income by year
pay_pal %>% 
  group_by(year) %>% 
  summarise(total_net = sum(net)) %>% 
  ggplot(aes(x = year, y = total_net)) +
  geom_bar(stat = 'identity', fill = '#9ecae1') +
  ggtitle('Total Memberships/Donations by By Year', subtitle = "From PayPal") +
  xlab('Year') +
  ylab('Dollars') +
  scale_y_continuous(labels = dollar) +
  geom_text(aes(label = dollar(round(total_net, digits = 0)),
                vjust = -0.5),
            size = 4.5) +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold'))

# paypal transaction info
pay_pal %>% 
  group_by(year_month) %>% 
  summarise(total_gross = sum(gross),
            total_fee = sum(fee),
            total_net = sum(net)) %>% 
  mutate(total_fee = abs(total_fee)) %>% 
  gather(key = 'type', value = 'total_amt', -year_month) %>% 
  ggplot(aes(x = year_month, y = total_amt, group = type, color = type)) +
  geom_line() +
  geom_point() +
  ggtitle('Total Membership/Donations by By Year', subtitle = "From PayPal") +
  xlab('Year-Month') +
  ylab('Dollars') +
  scale_y_continuous(labels = dollar) +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# unique members and donors
pay_pal %>% 
  distinct(year, income_type, from_email_address) %>% 
  count(year, income_type, name = 'distinct_count') %>% 
  ggplot(aes(x = year, y = distinct_count, fill = income_type)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  ggtitle('Count of Unique Donations and Memberships', subtitle = 'From PayPal, With Email Address Only') +
  xlab('Year') +
  ylab('Count') +
  labs(fill = '') +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold')) +
  geom_text(aes(label = distinct_count,
                vjust = -0.5),
            position = position_dodge(width = 0.9),
            size = 4.5)

# list of current members
current_memb_list <- pay_pal %>% 
  filter(date >= today() - 365, !is.na(from_email_address)) %>% 
  distinct(from_email_address) %>% 
  pull()

current_members <- pay_pal %>% 
  filter(from_email_address %in% current_memb_list) %>% 
  group_by(from_email_address, income_type) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(date,
         name,
         gross,
         fee,
         net,
         from_email_address,
         shipping_address,
         year,
         year_month,
         income_type,
         expires_date,
         years_a_member) %>% 
  mutate(years_a_member = ifelse(years_a_member == 30, 3, years_a_member)) %>% 
  distinct()

non_renewed <- pay_pal %>% 
  filter(date <= today() - 365) %>% 
  filter(!(from_email_address %in% (current_memb_list))) %>%
  filter(!is.na(from_email_address)) %>% 
  group_by(from_email_address, income_type) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(date,
         name,
         gross,
         fee,
         net,
         from_email_address,
         shipping_address,
         year,
         year_month,
         income_type,
         expires_date,
         years_a_member) %>% 
  mutate(years_a_member = ifelse(years_a_member == 30, 3, years_a_member)) %>% 
  distinct()

list_of_datasets <- list('current_members' = current_members,
                         'non_renewed' = non_renewed,
                         'cvast_books' = cvast_books)

write.xlsx(list_of_datasets, file = 'outputs/cvast_info.xlsx')
