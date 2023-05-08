library(rgrambank)
library(DBI)


test_that("reading tables works", {
  conn <- rgrambank::connect(testthat::test_path("fixtures", "grambank.sqlite"))
  expect_length(DBI::dbListTables(conn), 10)
})


test_that("reading columns works", {
  conn <- rgrambank::connect(testthat::test_path("fixtures", "grambank.sqlite"))
  expect_equal(dim(rgrambank::table(conn, 'LanguageTable'))[[2]], 13)
})
