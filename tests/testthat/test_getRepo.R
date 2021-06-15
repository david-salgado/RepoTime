library(RepoTime)
context("Getter getRepo")

test_that("Getter returns a character vector", {
    expect_true(
        all(sapply(getRepo(newRepoTime('MM022013')), class) == 'character'))
    expect_true(
        all(sapply(getRepo(
            newRepoTime(paste0(paste0('MM0', 1:9), '2013'))), 
            class) == 'character'))
})
