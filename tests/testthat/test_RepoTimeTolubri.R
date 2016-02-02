library(RepoTime)
context("Function RepoTimeTolubri")

test_that("RepoTimeTolubri reports error when input repo time interval is too short", {
    expect_that(RepoTimeTolubri(character(0)), throws_error())
    expect_that(RepoTimeTolubri(''), throws_error())
    expect_that(RepoTimeTolubri('Q1'), throws_error())
    expect_that(RepoTimeTolubri('BB2'), throws_error())
    expect_that(RepoTimeTolubri('m126'), throws_error())
    expect_that(RepoTimeTolubri('A2016'), throws_error())
    expect_that(RepoTimeTolubri('12016'), throws_error())
})

test_that("RepoTimeTolubri reports error when input repo time interval does not have correct two initial characters", {
    expect_that(RepoTimeTolubri('FF2015'), throws_error())
    expect_that(RepoTimeTolubri('aa2015'), throws_error())
    expect_that(RepoTimeTolubri('Aa2015'), throws_error())
    expect_that(RepoTimeTolubri('2015'), throws_error())
})

test_that("RepoTimeTolubri reports error when input repo time interval beginning with AA or AR does have valid syntax", {
    expect_that(RepoTimeTolubri('AA1'), throws_error())
    expect_that(RepoTimeTolubri('AA01'), throws_error())
    expect_that(RepoTimeTolubri('AR92'), throws_error())
})

test_that("RepoTimeTolubri reports error when input repo time interval beginning with SS or SR does have valid syntax", {
    
})

test_that("RepoTimeTolubri reports error when input repo time interval beginning with TT or TR does have valid syntax", {
    
})

test_that("RepoTimeTolubri reports error when input repo time interval beginning with MM or MR does have valid syntax", {
    
})

test_that("RepoTimeTolubri reports error when input repo time interval beginning with QQ or QR does have valid syntax", {
    
})

test_that("RepoTimeTolubri reports error when input repo time interval is too long", {
    expect_that(RepoTimeTolubri('QQ012015'), throws_error())
    expect_that(RepoTimeTolubri('MM1102016'), throws_error())
    expect_that(RepoTimeTolubri('TT032015'), throws_error())
    expect_that(RepoTimeTolubri('SS011900'), throws_error())
    expect_that(RepoTimeTolubri('BB011994'), throws_error())
    expect_that(RepoTimeTolubri('AA12016'), throws_error())
})

test_that("RepoTimeTolubri reports error when input repo time interval is a longer vector", {
    expect_that(RepoTimeTolubri(c('QQ012015', 'MM1102016', 'TT032015',
                                  'SS011900', 'BB011994','AA12016')), 
                throws_error())
})

