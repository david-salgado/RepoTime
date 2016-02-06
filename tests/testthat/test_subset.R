library(RepoTime)
context("Function [")

test_that("[ returns the correct subsetted RepoTime object", {
    
    Object <- newRepoTime(paste0('MM0', 1:9, '2015'))
    expect_that(Object[1], is_identical_to(newRepoTime('MM012015')))
    
    Object <- newRepoTime(paste0('MM0', 1:9, '2015'))
    expect_that(Object['MM052015'], is_identical_to(newRepoTime('MM052015')))
    
    Object <- newRepoTime(paste0('TT', 1:4, '2011'))
    expect_that(
        Object[c(1, 3, 1)], 
        is_identical_to(newRepoTime(c('TT12011', 'TT32011', 'TT12011')))
    )
    
})