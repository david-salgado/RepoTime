library(RepoTime)
context("Function Intersect")

test_that("Intersect returns the correct RepoTimeInt class object", {
    expect_that(
        Intersect(newRepoTime('TT12015'), newRepoTime('MM022015')), 
        is_identical_to(newRepoTime('MM022015')))
    expect_that(
        Intersect(newRepoTime('TT12015'), newRepoTime('AA2015')), 
        is_identical_to(newRepoTime('TT12015')))
    expect_that(
        Intersect(newRepoTime(c('TT12015', 'AA2015'))), 
        is_identical_to(newRepoTime('TT12015')))
    expect_that(
        Intersect(newRepoTime(c('TT12015', 'TT22015'))), 
        is_identical_to(lubridate::interval(start = NA, end = NA, 
                                            tzone = 'Europe/Madrid')))
    
})

test_that("Intersect reports error when input objects not of class Interval", {
    expect_that(
        Intersect('MM012013', 'AA2015'), throws_error())        
    expect_that(
        Intersect('MM012013', newRepoTime('AA2015')), throws_error())        
})