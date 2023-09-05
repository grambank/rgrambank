test_that("langage_level_df works as expected", {
    cldf <- rcldf::cldf("fixtures/testdata/StructureDataset-metadata.json")

    lldf <- reduce_ValueTable_to_unique_glottocodes(
        ValueTable = cldf$tables$ValueTable,
        LangueageTable = cldf$tables$LanguageTable,
        method = "singular_least_missing_data"
    )

    for (removed in c('munu1238', 'ngar1285', 'yawi1239')) {
        expect_equal(nrow(subset(lldf, Language_ID == removed)), 0)
    }
})