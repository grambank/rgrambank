
test_that("Test if drop_duplicate_tips_at_random works as expected when dialects aren't merged", {

outcome <- ape::read.tree("fixtures/example_tree.tree") %>%
    drop_duplicate_tips_random(merge_dialects = F) %>%
    ape::Ntip()

    expected <- 6
    expect_equal(outcome, expected)
})


test_that("Test if drop_duplicate_tips_at_random works as expected when dialects are merged.", {

    outcome <- ape::read.tree("fixtures/example_tree.tree") %>%
        drop_duplicate_tips_random(merge_dialects = T) %>%
        ape::Ntip()

    expected <- 4
    expect_equal(outcome, expected)
})

