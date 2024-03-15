test_that("add_family_name_column works as expected", {

    LanguageTable <- readr::read_csv("fixtures/testdata/languages.csv", show_col_types = F) %>%
        dplyr::rename(Family_ID = Family_level_ID) %>%
        dplyr::select(-Family_name)
    LanguageTable2 <- readr::read_tsv("fixtures/cldf_wide_df_glottolog_4.8.tsv", show_col_types = F)

    outcome <-  add_family_name_column(LanguageTable = LanguageTable, LanguageTable2 = LanguageTable2)

    expectation <-  c("Worrorran", "Worrorran", "Worrorran", "Worrorran", "Worrorran" ,"Worrorran", "Worrorran", "Worrorran")

    expect_equal(outcome$Family_name, expectation)
})


test_that("add_family_name_column works as expected", {

    LanguageTable <- readr::read_csv("fixtures/testdata/languages.csv", show_col_types = F) %>%
        dplyr::rename(Family_ID = Family_level_ID) %>%
        dplyr::select(-Family_name)

    outcome <-  add_family_name_column(LanguageTable = LanguageTable)

    expectation <-  c(NA, NA, NA, NA, NA ,NA, NA, NA) %>% as.character()

    expect_equal(outcome$Family_name, expectation)
})