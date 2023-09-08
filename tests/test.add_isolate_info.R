test_that("add_isolate_info works as expected", {

    outcome <-  read_csv("fixtures/testdata/languages.csv") %>%
        dplyr::rename(Family_ID = Family_level_ID) %>%
        add_isolate_info(add_isolate_column = TRUE, set_isolates_family_as = "isolate") %>%
        dplyr::filter(Isolate == "yes") %>%
        nrow()

    expectation <- 0

    expect_equal(outcome, expectation)
})