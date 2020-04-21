
# library bank ----

library(googledrive)
library(readxl)
library(scales)
library(tidyverse)

# directories ----

df_path <- "inputs/CVAST books.xlsx"

# download books ----

drive_download("CVAST books.xlsx",
               path = df_path,
               overwrite = T)

# read in file ----

cvast_books <- excel_sheets(df_path) %>% 
  map(function(s) {read_excel(path = df_path, sheet = s, range = cell_cols("A:D"))}) %>% 
  bind_rows() %>% 
  rename_all(tolower)

cvast_books <- cvast_books %>%
  mutate(trans_type = ifelse(amount < 0, "Expense", "Revenue"),
         year = as.numeric(format(date, "%Y")))

cvast_books %>% filter(!is.na(trans_type)) %>% 
  group_by(year, trans_type) %>% summarize(amount = sum(amount)) %>% 
  ggplot(aes(x = year, y = abs(amount), fill = trans_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Year") +
  ylab("Amount (USD)") +
  ggtitle("CVAST Revenue vs. Expenses Overtime") +
  scale_y_continuous(labels = dollar) +
  labs(fill = "Tansaction Type") +
  geom_text(aes(label = dollar(round(abs(amount), 0))), size = 3, vjust = -0.3) +
  theme_classic()

cvast_books %>% filter(!is.na(trans_type)) %>% 
  ggplot(aes(x = date, y = balance)) +
  geom_line()

uniq_desc <- cvast_books %>% distinct(transaction)

