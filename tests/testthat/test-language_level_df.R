test_that("langage_level_df works as expected", {

    outcome <- read.delim("fixtures/testdata/values.csv", sep = ",") %>%
        language_level_df(method = "singular_least_missing_data") %>%
        nrow()

    expected <- 404
    expect_equal(outcome, expected)
})