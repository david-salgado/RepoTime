library(RepoTime)
context("Function Length")

test_that("Length returns the correct value", {
    expect_that(
        Length(newRepoTime('TT12015')), 
        is_identical_to(1L))
    expect_that(
        Length(newRepoTime(c('TT12015', 'MM032014'))), 
        is_identical_to(2L))
})