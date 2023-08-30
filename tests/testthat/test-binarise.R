library(reshape2)
library(dplyr)
library(readr)
library(testthat)

source("R/language_level_df.R")
source("R/binarise.R")
source("R/make_binary_parameters_df.R")


#reading in a Grambank value table from the internet and tacking on rows with already binarised features. This allows us to test handling these kinds of features even though they do not occur in Grambank release v1.90

values_fn_full <- "https://github.com/grambank/grambank/raw/9e0f34194224204fa6a2058a2c12d43923e8715f/cldf/values.csv"

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

read_csv(values_fn_full, show_col_types = F) %>%
    filter(Language_ID == "anci1242"|
               Language_ID == "samo1305" ) %>%
    full_join(fake_raw_binary_data, by = join_by(ID, Language_ID, Parameter_ID, Value, Code_ID, Comment, Source, Source_comment, Coders)) %>%
    write_csv("tests/testthat/fixtures/values_with_raw_fake_binary.csv")

values_fn <- "tests/testthat/fixtures/values_with_raw_fake_binary.csv"
ValueTable <- read_csv(values_fn, show_col_types = F)

test_that("binarised features are not being overwritten", {
    outcome <- binarise(ValueTable = ValueTable, wide = T, drop_multistate = F,
                        keep_raw_binary = F) %>%
        dplyr::select(Language_ID, GB024, GB024a, GB024b, GB065, GB065a, GB065b) %>%
        filter(Language_ID == "anci1242") %>%
        as.matrix() %>%
        as.vector() %>%
        .[2:7]

    expected <- c("3", "1", "1", "3", "1", "1")
    expect_equal(outcome, expected)



    outcome <- %>%










    })


#testing



all(outcome ==  == T

#if we keep the fake raw binary data
outcome <- binarise(values_fn = values_fn, wide = T, drop_multistate = F, keep_raw_binary = T) %>%
  dplyr::select(Language_ID, GB024, GB024a, GB024b, GB065, GB065a, GB065b) %>%
  filter(Language_ID == "anci1242") %>%
  as.matrix() %>%
  as.vector() %>%
  .[2:7]

all(outcome == c("3", "0", "1", "3", "1", "?")) == T
