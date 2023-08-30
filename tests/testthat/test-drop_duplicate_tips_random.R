
test_that("Test if drop_duplicate_tips_at_random works as expected when dialects aren't merged", {
    library(dplyr)
    library(ape)
    library(readr)

outcome <- ape::read.tree("fixtures/example_tree.tree") %>%
    drop_duplicate_tips_random(merge_dialects = F) %>%
    Ntip()

    expected <- 6
    expect_equal(outcome, expected)
})


test_that("Test if drop_duplicate_tips_at_random works as expected when dialects are merged.", {
    library(dplyr)
    library(ape)
    library(readr)

    outcome <- ape::read.tree("fixtures/example_tree.tree") %>%
        drop_duplicate_tips_random(merge_dialects = T) %>%
        Ntip()

    expected <- 4
    expect_equal(outcome, expected)
})

