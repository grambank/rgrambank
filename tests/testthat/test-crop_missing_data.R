test_that("crop_missing_data works as expected", {
#library(rgrambank)
#library(rcldf)
#library(tidyverse)
#source("../../R/crop_missing_data.R")

    cldf <- rcldf::cldf("fixtures/testdata/StructureDataset-metadata.json")
    ValueTable <- cldf$tables$ValueTable

    ValueTable_cropped <- crop_missing_data(ValueTable = ValueTable)

    n_lgs <- length(unique(ValueTable_cropped$Language_ID))
    n_feats <- length(unique(ValueTable_cropped$Parameter_ID))


        expect_equal(n_lgs, 3)
        expect_equal(n_feats, 87)

}
)