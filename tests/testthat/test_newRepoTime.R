library(RepoTime)
context("Constructor newRepoTime")

test_that("newRepoTime reports error when input repo time interval is too short", {
    expect_that(newRepoTime(character(0)), throws_error())
    expect_that(newRepoTime(''), throws_error())
    expect_that(newRepoTime('Q1'), throws_error())
    expect_that(newRepoTime('BB2'), throws_error())
    expect_that(newRepoTime('m126'), throws_error())
    expect_that(newRepoTime('A2016'), throws_error())
    expect_that(newRepoTime('12016'), throws_error())
})

test_that("newRepoTime reports error when input repo time interval does not have correct two initial characters", {
    expect_that(newRepoTime('FF2015'), throws_error())
    expect_that(newRepoTime('aa2015'), throws_error())
    expect_that(newRepoTime('Aa2015'), throws_error())
    expect_that(newRepoTime('2015'), throws_error())
})

test_that("newRepoTime reports error when input repo time interval beginning with AA or AR does have valid syntax", {
    expect_that(newRepoTime('AA1'), throws_error())
    expect_that(newRepoTime('AA01'), throws_error())
    expect_that(newRepoTime('AR92'), throws_error())
})

test_that("newRepoTime reports error when input repo time interval beginning with SS or SR does have valid syntax", {
    expect_that(newRepoTime('SS1'), throws_error())
    expect_that(newRepoTime('SS01'), throws_error())
    expect_that(newRepoTime('SR192'), throws_error())
})

test_that("newRepoTime reports error when input repo time interval beginning with TT or TR does have valid syntax", {
    expect_that(newRepoTime('TT3'), throws_error())
    expect_that(newRepoTime('TT03'), throws_error())
    expect_that(newRepoTime('TR392'), throws_error())
})

test_that("newRepoTime reports error when input repo time interval beginning with MM or MR does have valid syntax", {
    expect_that(newRepoTime('MM1'), throws_error())
    expect_that(newRepoTime('MM01'), throws_error())
    expect_that(newRepoTime('MR192'), throws_error())
})

test_that("newRepoTime reports error when input repo time interval beginning with QQ or QR does have valid syntax", {
    expect_that(newRepoTime('QQ1'), throws_error())
    expect_that(newRepoTime('QQ01'), throws_error())
    expect_that(newRepoTime('QR101'), throws_error())
    expect_that(newRepoTime('QR10114'), throws_error())
})

test_that("newRepoTime reports error when input repo time interval is too long", {
    expect_that(newRepoTime('QQ012015'), throws_error())
    expect_that(newRepoTime('MM1102016'), throws_error())
    expect_that(newRepoTime('TT032015'), throws_error())
    expect_that(newRepoTime('SS011900'), throws_error())
    expect_that(newRepoTime('BB011994'), throws_error())
    expect_that(newRepoTime('AA12016'), throws_error())
})

test_that("newRepoTime reports error when input repo time interval is a longer vector", {
    expect_that(newRepoTime(c('QQ012015', 'MM1102016', 'TT032015',
                                  'SS011900', 'BB011994','AA12016')), 
                throws_error())
})