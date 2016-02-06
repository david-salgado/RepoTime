library(RepoTime)
context("Function Union")

test_that("Union returns the correct union of RepoTime objects", {
    
    RepoPeriod1 <- newRepoTime('QQ1012009')
    RepoPeriod2 <- newRepoTime('QQ2012009')
    expect_that(Union(RepoPeriod1, RepoPeriod2), 
                is_identical_to(newRepoTime('MM012009'))
    )
    
    RepoPeriod1 <- newRepoTime('MM072014')
    RepoPeriod2 <- newRepoTime('MM082014')
    expect_that(Union(RepoPeriod1, RepoPeriod2), 
                is_identical_to(newRepoTime('BB42014'))
    )
    
    RepoPeriod1 <- newRepoTime('MM012014')
    RepoPeriod2 <- newRepoTime('MM032014')
    expect_that(Union(RepoPeriod1, RepoPeriod2), 
                is_identical_to(newRepoTime('TT12014'))
    )
    
    RepoPeriod1 <- Seq(newRepoTime('BB42000'), newRepoTime('BB62000'))
    expect_that(Union(RepoPeriod1), 
                is_identical_to(newRepoTime('SS22000'))
    )
    

})