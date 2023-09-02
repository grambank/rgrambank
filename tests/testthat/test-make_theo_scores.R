test_that("theo scores work as expected", {

    outcome <- make_theo_scores(ValueTable = read.delim("fixtures/values.csv", sep = ","),
                                ParameterTable = read.delim("fixtures/parameters.csv", sep = ",")) %>%
        dplyr::filter(Language_ID == "anci1242") %>%
        as.matrix() %>%
        as.vector() %>%
        .[2:7]

    expected <- c("0.4545455" , "0", "0.1764706" , "0.4705882" , "0"  ,  "0.01818182")
    expect_equal(outcome, expected)
})