test_that("langage_level_df works as expected", {
    library(reshape2)
    library(dplyr)
    library(readr)
    library(tidyr)

    outcome <- read_csv("fixtures/values.csv", show_col_types = F) %>%
        make_ValueTable_wide()  %>%
        language_level_df(method = "singular_least_missing_data", ) %>% nrow()


    expected <- 6
    expect_equal(outcome, expected)
})


