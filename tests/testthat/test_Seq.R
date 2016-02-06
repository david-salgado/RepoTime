library(RepoTime)
context("Function Seq")

test_that("Seq returns the correct value for simple sequences", {
    RepoPeriod1 <- newRepoTime('MM032014')
    RepoPeriod2 <- newRepoTime('MM062014')
    SeqMan <- newRepoTime(c('MM032014', 'MM042014', 'MM052014', 'MM062014'))
    expect_that(Seq(RepoPeriod1, RepoPeriod2), is_identical_to(SeqMan))
    
    RepoPeriod1 <- newRepoTime('BB32014')
    RepoPeriod2 <- newRepoTime('BB22015')
    SeqMan <- newRepoTime(c('BB32014', 'BB42014', 'BB52014', 'BB62014', 'BB12015', 'BB22015'))
    expect_that(Seq(RepoPeriod1, RepoPeriod2), is_identical_to(SeqMan))
    
    RepoPeriod1 <- newRepoTime('TT32014')
    RepoPeriod2 <- newRepoTime('TT22015')
    SeqMan <- newRepoTime(c('TT32014', 'TT42014', 'TT12015', 'TT22015'))
    expect_that(Seq(RepoPeriod1, RepoPeriod2), is_identical_to(SeqMan))
    
    RepoPeriod1 <- newRepoTime('SS12014')
    RepoPeriod2 <- newRepoTime('SS22015')
    SeqMan <- newRepoTime(c('SS12014', 'SS22014', 'SS12015', 'SS22015'))
    expect_that(Seq(RepoPeriod1, RepoPeriod2), is_identical_to(SeqMan))
    
    RepoPeriod1 <- newRepoTime('AA2014')
    RepoPeriod2 <- newRepoTime('AA2016')
    SeqMan <- newRepoTime(c('AA2014', 'AA2015', 'AA2016'))
    expect_that(Seq(RepoPeriod1, RepoPeriod2), is_identical_to(SeqMan))
})

test_that("Seq returns the correct value for sequences with rotated elements", {
    RepoPeriod1 <- newRepoTime('MM032014')
    RepoPeriod2 <- newRepoTime('MM062014')
    SeqMan <- newRepoTime(c('MM032014', 'MM042014', 'MM052014', 'MM062014', 'MR062014'))
    expect_that(
        Seq(RepoPeriod1, RepoPeriod2, Rot = TRUE, RotPer = c('06', '12')),
        is_identical_to(SeqMan)
    )
    
    RepoPeriod1 <- newRepoTime('BB32014')
    RepoPeriod2 <- newRepoTime('BB32015')
    SeqMan <- newRepoTime(c('BB32014', 'BR32014', 'BB42014', 
                            'BB52014', 'BB62014', 'BB12015', 
                            'BB22015', 'BB32015', 'BR32015'))
    expect_that(
        Seq(RepoPeriod1, RepoPeriod2, Rot = TRUE, RotPer = '3'),
        is_identical_to(SeqMan)
    )
    RepoPeriod1 <- newRepoTime('TT32014')
    RepoPeriod2 <- newRepoTime('TT22015')
    SeqMan <- newRepoTime(c('TT32014', 'TT42014', 'TR42014', 'TT12015', 'TT22015'))
    expect_that(
        Seq(RepoPeriod1, RepoPeriod2, RotPer = '4'),
        is_identical_to(SeqMan)
    )
    RepoPeriod1 <- newRepoTime('SS12014')
    RepoPeriod2 <- newRepoTime('SS22015')
    SeqMan <- newRepoTime(c('SS12014', 'SR12014', 'SS22014', 'SS12015', 'SR12015', 'SS22015'))
    expect_that(
        Seq(RepoPeriod1, RepoPeriod2, RotPer = '1'),
        is_identical_to(SeqMan)
    )
    
})