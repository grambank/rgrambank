test_that("theo scores work as expected", {
    library(reshape2)
    library(dplyr)
    library(readr)

    outcome <- make_theo_scores(ValueTable = read_csv("fixtures/values.csv", show_col_types = FALSE), ParameterTable_fn = "fixtures/parameters.csv") %>%
        dplyr::filter(Language_ID == "anci1242") %>%
        as.matrix() %>%
        as.vector() %>%
        .[2:7]

    expected <- c("0.4545455" , "0", "0.1764706" , "0.4705882" , "0"  ,  "0.01818182")
    expect_equal(outcome, expected)
})