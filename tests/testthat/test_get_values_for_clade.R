
test_that("get_values_for_clade", {
    cldf <- rcldf::cldf("fixtures/testdata/StructureDataset-metadata.json")

    # two taxa worr1237 and yawi1239
    df <- get_values_for_clade(cldf, 'west2435')
    expect_equal(nrow(df[df$Language_ID == 'gamb1251', ]), 0)
    expect_equal(nrow(df[df$Language_ID == 'kwin1241', ]), 0)
    expect_equal(nrow(df[df$Language_ID == 'miwa1242', ]), 0)
    expect_equal(nrow(df[df$Language_ID == 'munu1238', ]), 0)
    expect_equal(nrow(df[df$Language_ID == 'ngar1284', ]), 0)
    expect_equal(nrow(df[df$Language_ID == 'ngar1285', ]), 0)
    expect_equal(nrow(df[df$Language_ID == 'worr1237', ]), 174)
    expect_equal(nrow(df[df$Language_ID == 'yawi1239', ]), 174)

    # everything
    df <- get_values_for_clade(cldf, 'worr1236')
    expect_equal(nrow(df), nrow(cldf$tables$ValueTable))

    # nothing
    df <- get_values_for_clade(cldf, 'simon')
    expect_equal(nrow(df), 0)
})