library(reshape2)
library(dplyr)
library(readr)
library(testthat)

source("R/language_level_df.R")
source("R/binarise.R")
source("R/make_binary_parameters_df.R")



test_that("binarised features are not being overwritten", {
    data_with_binarised_features <- ...
    outcome <- binarise(data_with_binarised_features) %>% ...
    expected <- ...
    expect_equal(outcome, expected)
})


#testing

values_fn <- "https://github.com/grambank/grambank/raw/9e0f34194224204fa6a2058a2c12d43923e8715f/cldf/values.csv"

fake_raw_binary_data <- data.frame(
  ID = c("anci1242-GB024a", "anci1242-GB065b"),
  Language_ID = c("anci1242", "anci1242"),
  Parameter_ID = c("GB024a", "GB065b"),
  Value = c("0", "?"),
  Comment = c(NA, NA),
  Code_ID = c("GB024a-0", "GB065b-?"),
  Source = c("s_OaPaul_Gabadi[17]", "s_OaPaul_Gabadi[17]"),
  Source_comment = c("Oa & Paul 2013:17", "Oa & Paul 2013:17"),
  Coders = c("HS", "HS")
)

read_csv(values_fn, show_col_types = F) %>%
  full_join(fake_raw_binary_data, by = join_by(ID, Language_ID, Parameter_ID, Value, Code_ID, Comment, Source, Source_comment, Coders)) %>%
  write_csv("../tests/testthat/fixtures/values_with_raw_fake_binary.csv")

values_fn <- "../tests/testthat/fixtures/values_with_raw_fake_binary.csv"

outcome <- binarise(values_fn = values_fn, wide = T, drop_multistate = F, keep_raw_binary = F) %>%
  dplyr::select(Language_ID, GB024, GB024a, GB024b, GB065, GB065a, GB065b) %>%
  filter(Language_ID == "anci1242") %>%
  as.matrix() %>%
  as.vector() %>%
  .[2:7]

all(outcome == c("3", "1", "1", "3", "1", "1")) == T

#if we keep the fake raw binary data
outcome <- binarise(values_fn = values_fn, wide = T, drop_multistate = F, keep_raw_binary = T) %>%
  dplyr::select(Language_ID, GB024, GB024a, GB024b, GB065, GB065a, GB065b) %>%
  filter(Language_ID == "anci1242") %>%
  as.matrix() %>%
  as.vector() %>%
  .[2:7]

all(outcome == c("3", "0", "1", "3", "1", "?")) == T
