test_that("langage_level_df works as expected", {
    cldf <- rcldf::cldf("fixtures/testdata/StructureDataset-metadata.json")

    lldf <- language_level_df(
        cldf$tables$ValueTable,
        cldf$tables$LanguageTable,
        method = "singular_least_missing_data"
    )

    for (removed in c('munu1238', 'ngar1285', 'yawi1239')) {
        expect_equal(nrow(subset(lldf, Language_ID == removed)), 0)
    }
})