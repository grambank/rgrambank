test_that("langage_level_df works as expected", {
    library(dplyr)
    library(readr)

    outcome <- read_csv("fixtures/values.csv", show_col_types = F) %>%
        language_level_df(method = "singular_least_missing_data", ) %>%        distinct(Language_ID) %>%
        nrow()

    expected <- 6
    expect_equal(outcome, expected)
})


