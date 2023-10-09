test_that("get_shared_features", {
    cldf <- rcldf::cldf("fixtures/testdata/StructureDataset-metadata.json")

    # test identical
    shared <- get_shared_features(cldf, 'kwin1241', 'kwin1241')

    # number of non-empty parameters
    expected <- cldf$tables$ValueTable %>%
        dplyr::filter(Language_ID == 'kwin1241') %>%
        dplyr::filter(Value!="?") %>%
        dplyr::pull(Parameter_ID) %>%
        length()

    expect_equal(shared$compared, expected)
    expect_equal(shared$identical, expected)
    expect_equal(nrow(shared$features), expected)

    # pre-calculated
    shared <- get_shared_features(cldf, 'yawi1239', 'kwin1241')
    expect_equal(shared$compared, 119)
    expect_equal(shared$identical, 109)
    expect_equal(nrow(shared$features), 109)

})