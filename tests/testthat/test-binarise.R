#testing that the function binarise behaves as expected.

test_that("binarise_GBXXX_to_GBXXXa_without_zero", {
    expect_equal(binarise_GBXXX_to_GBXXXa_without_zero(c("1", "2", "3", "?", NA)), c("1", "0", "1", "?", NA))
})

test_that("binarise_GBXXX_to_GBXXXb_without_zero", {
    expect_equal(binarise_GBXXX_to_GBXXXb_without_zero(c("1", "2", "3", "?", NA)), c("0", "1", "1", "?", NA))
})

test_that("binarise_GBXXX_to_GBXXXa_with_zero", {
    expect_equal(binarise_GBXXX_to_GBXXXa_with_zero(c("0", "1", "2", "3", "?", NA)), c("0", "1", "0", "1", "?", NA))
})

test_that("binarise_GBXXX_to_GBXXXb_without_zero", {
    expect_equal(binarise_GBXXX_to_GBXXXb_with_zero(c("0", "1", "2", "3", "?", NA)), c("0", "0", "1", "1", "?", NA))
})


test_that("raw binarised features are being overwritten when keep_raw_binary is set to FALSE", {
   outcome <- binarise(
       ValueTable = readr::read_csv("fixtures/values_with_raw_fake_binary.csv",
                                    show_col_types = FALSE),
        drop_multistate = FALSE,
        keep_raw_binary = FALSE) %>%
       dplyr::filter(Language_ID == "anci1242") %>%
       dplyr::filter(Parameter_ID %in% c("GB024", "GB024a", "GB024b", "GB065", "GB065a", "GB065b")) %>%
       dplyr::arrange(Parameter_ID) %>%
       .$Value

   expected <- c("3", "1", "1", "3", "1", "1")
   expect_equal(outcome, expected)
   })


test_that("raw binarised features are not being overwritten when keep_raw_binary is set to TRUE", {
   outcome <- binarise(
       ValueTable = readr::read_csv("fixtures/values_with_raw_fake_binary.csv",
                                    show_col_types = FALSE),
       drop_multistate = FALSE,
       keep_raw_binary = TRUE) %>%
    dplyr::filter(Language_ID == "anci1242") %>%
       dplyr::filter(Parameter_ID %in% c("GB024", "GB024a", "GB024b", "GB065", "GB065a", "GB065b")) %>%
       dplyr::arrange(Parameter_ID) %>%
    .$Value

   expected <- c("3", "0", "1", "3", "1", "?")
   expect_equal(outcome, expected)
})


skip()
test_that("drop_multistate drops multistate variables as required", {
    outcome <- binarise(
        ValueTable = readr::read_csv("fixtures/values_with_raw_fake_binary.csv",
                                     show_col_types = FALSE),
        drop_multistate = FALSE,
        keep_raw_binary = TRUE) %>%
        dplyr::filter(Language_ID == "anci1242") %>%
        dplyr::filter(Parameter_ID %in% c("GB024", "GB024a", "GB024b", "GB065", "GB065a", "GB065b")) %>% View()
    .$Value

    expected <- c("3", "0", "1", "3", "1", "?")

    expect_equal(TRUE, FALSE)  # TODO
})
