
source("make_ValueTable_wide.R")
source("language_level_df.R")

test_data <- read_csv("https://raw.githubusercontent.com/grambank/grambank/master/cldf/values.csv", show_col_types = F) %>% 
  make_ValueTable_wide() 

language_level_df(wide_value_table = test_data, method = "singular_least_missing_data") %>% nrow() == 2428
