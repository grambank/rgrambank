#testing that the function binarise behaves as expected.

test_that("raw binarised features are being overwritten when keep_raw_binary is set to FALSE", {
    library(reshape2)
    library(dplyr)
    library(readr)

    outcome <- binarise(ValueTable = read_csv("tests/testthat/fixtures/values_with_raw_fake_binary.csv", show_col_types = FALSE), wide = TRUE,
                        drop_multistate = FALSE,
                        keep_raw_binary = FALSE) %>%
        dplyr::select(Language_ID, GB024, GB024a, GB024b, GB065, GB065a, GB065b) %>%
        filter(Language_ID == "anci1242") %>%
        as.matrix() %>%
        as.vector() %>%
        .[2:7]

    expected <- c("3", "1", "1", "3", "1", "1")
    expect_equal(outcome, expected)
    })


test_that("raw binarised features are not being overwritten when keep_raw_binary is set to TRUE", {
    library(reshape2)
    library(dplyr)
    library(readr)

    outcome <- binarise(ValueTable = read_csv("tests/testthat/fixtures/values_with_raw_fake_binary.csv", show_col_types = FALSE), wide = T,
                        drop_multistate = F,
                        keep_raw_binary = T) %>%
        dplyr::select(Language_ID, GB024, GB024a, GB024b, GB065, GB065a, GB065b) %>%
        filter(Language_ID == "anci1242") %>%
        as.matrix() %>%
        as.vector() %>%
        .[2:7]

    expected <- c("3", "0", "1", "3", "1", "?")
    expect_equal(outcome, expected)
})