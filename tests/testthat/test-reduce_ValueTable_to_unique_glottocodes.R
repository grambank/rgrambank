test_that("langage_level_df works as expected", {
    cldf <- rcldf::cldf("fixtures/testdata/StructureDataset-metadata.json")

    lldf <- reduce_ValueTable_to_unique_glottocodes(
        ValueTable = cldf$tables$ValueTable,
        LanguageTable = cldf$tables$LanguageTable,
        method = "singular_least_missing_data",
        merge_dialects = TRUE
    )

    outcome <- lldf$Language_ID %>% unique() %>% length()
    expected <- 5
        expect_equal(outcome, expected)
})


test_that("langage_level_df works as expected", {
    cldf <- rcldf::cldf("fixtures/testdata/StructureDataset-metadata.json")

    lldf <- reduce_ValueTable_to_unique_glottocodes(
        ValueTable = cldf$tables$ValueTable,
        LanguageTable = cldf$tables$LanguageTable,
        method = "singular_least_missing_data",
        merge_dialects = FALSE
    )

    outcome <- lldf$Language_ID %>% unique() %>% length()
    expected <- 8
    expect_equal(outcome, expected)
})