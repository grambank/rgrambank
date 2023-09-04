test_that("as.grambank.wide", {
    gb <- rcldf::cldf("fixtures/testdata/StructureDataset-metadata.json")
    values <- as.grambank.wide(gb$tables$ValueTable)

    params <- gb$tables$ValueTable %>% dplyr::filter(Language_ID == 'gamb1251') %>% dplyr::pull("Parameter_ID")

    # overkill perhaps
    for (p in params) {
        expected <- gb$tables$ValueTable %>%
            dplyr::filter(Language_ID == 'gamb1251' & Parameter_ID == p)

        expected <- ifelse(nrow(expected) == 0, NA, expected$Value)

        actual <- values %>% dplyr::filter(Language_ID == 'gamb1251') %>%
            dplyr::pull(p)

        expect_equal(actual, expected)
    }

    # check both methods work
    expect_equal(
        gb$tables$ValueTable %>% as.grambank.wide(),
        as.grambank.wide(gb$tables$ValueTable)
    )

})


test_that("as.grambank.wide fails on incorrect formats", {
    expect_error(as.grambank.wide('not a table'), "'ValueTable' must be a dataframe.")
    expect_error(as.grambank.wide(data.frame()), "Invalid table format")
})
