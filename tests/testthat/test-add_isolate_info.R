test_that("add_isolate_info works as expected", {

    outcome <-  readr::read_csv("fixtures/testdata/languages.csv", show_col_types = F) %>%
        dplyr::rename(Family_ID = Family_level_ID) %>%
        add_isolate_info(add_isolate_column = TRUE, set_isolates_Family_ID_as = "missing") %>%
        dplyr::filter(Isolate == "yes") %>%
        nrow()

    expectation <- 0

    expect_equal(outcome, expectation)
})