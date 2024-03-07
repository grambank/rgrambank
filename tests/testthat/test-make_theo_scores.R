test_that("theo scores work as expected", {
    values <- read.delim("fixtures/values_theoretical.csv", sep = ",")
    params <- read.delim("fixtures/testdata/parameters.csv", sep = ",")
    theoretical <- make_theo_scores(values, params) %>%
        dplyr::filter(Language_ID == "anci1242")

    expect_equal(round(theoretical[['Word_Order']], 4), 0.4545)
    expect_equal(round(theoretical[['Flexivity']], 4), 0.4333)
    expect_equal(round(theoretical[['Gender_or_Noun_Class']], 4), 0.6471)
    expect_equal(round(theoretical[['Locus_of_Marking']], 4), 0.2647)
    expect_equal(round(theoretical[['mean_morph']], 4), 0.4836)
    expect_equal(round(theoretical[['Informativity']], 4), 0.4)
})