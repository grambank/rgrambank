#testing that the function as.grambank.wide behaves as expected.

test_that("as.grambank.wide", {
    gb <- rcldf::cldf("fixtures/testdata/StructureDataset-metadata.json")
    values <- as.grambank.wide(gb$tables$ValueTable)

    params <- gb$tables$ParameterTable %>% dplyr::pull("ID")
    # overkill perhaps
    for (p in params) {
        original <- gb$tables$ValueTable %>%
            dplyr::filter(Language_ID == 'gamb1251' & Parameter_ID == p) %>%
            dplyr::pull(Value)
        widened <- values %>% dplyr::filter(Language_ID == 'gamb1251') %>%
            dplyr::pull(p)

        if (original != widened) {
            cat(sprintf("Param %s: %s == %s ---> %s\n", p, original, widened, original == widened))
        }
        expect_equal(original, widened)
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
