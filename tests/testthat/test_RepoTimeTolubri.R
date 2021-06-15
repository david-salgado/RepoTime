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

test_that("RepoTimeTolubri reports error when input repo time interval does not have the correct two initial characters", {
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
    expect_that(RepoTimeTolubri('SS201601'), throws_error())
    expect_that(RepoTimeTolubri('SS20161'), gives_warning())
    expect_that(RepoTimeTolubri('SR02014'), throws_error())
    expect_that(RepoTimeTolubri('SR104'), throws_error())
    expect_that(RepoTimeTolubri('SR32012'), throws_error())
    expect_that(RepoTimeTolubri('SSa2012'), throws_error())
})

test_that("RepoTimeTolubri reports error when input repo time interval beginning with TT or TR does have valid syntax", {
    expect_that(RepoTimeTolubri('TT201601'), throws_error())
    expect_that(RepoTimeTolubri('TT20164'), gives_warning())
    expect_that(RepoTimeTolubri('TR02014'), throws_error())
    expect_that(RepoTimeTolubri('TR104'), throws_error())
    expect_that(RepoTimeTolubri('TR52012'), throws_error())
    expect_that(RepoTimeTolubri('TTt2012'), throws_error())
})

test_that("RepoTimeTolubri reports error when input repo time interval beginning with MM or MR does have valid syntax", {
    expect_that(RepoTimeTolubri('MM201601'), throws_error())
    expect_that(RepoTimeTolubri('MM12016'), throws_error())
    expect_that(RepoTimeTolubri('MR152014'), throws_error())
    expect_that(RepoTimeTolubri('MR104'), throws_error())
    expect_that(RepoTimeTolubri('MR52012'), throws_error())
    expect_that(RepoTimeTolubri('MMt2011'), throws_error())
})

test_that("RepoTimeTolubri reports error when input repo time interval beginning with QQ or QR does have valid syntax", {
    expect_that(RepoTimeTolubri('QQ20161201'), throws_error())
    expect_that(RepoTimeTolubri('QQ2016121'), gives_warning())
    expect_that(RepoTimeTolubri('QR3022016'), throws_error())
    expect_that(RepoTimeTolubri('QR11304'), throws_error())
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

test_that("RepoTimeTolubri makes the correct transformations", {
    expect_that(RepoTimeTolubri('QQ1012016'), 
                is_identical_to(list(QQ1012016 = 
                                       lubridate::interval(start = '2016-01-01',
                                                  end = '2016-01-15',
                                                  tzone = 'Europe/Madrid'))))
    expect_that(RepoTimeTolubri('QQ2022015'), 
                is_identical_to(list(QQ2022015 = 
                                       lubridate::interval(start = '2015-02-16',
                                                  end = '2015-02-28',
                                                  tzone = 'Europe/Madrid'))))
    expect_that(RepoTimeTolubri('MM022016'), 
                is_identical_to(list(MM022016 = 
                                       lubridate::interval(start = '2016-02-01',
                                         end = '2016-02-29',
                                         tzone = 'Europe/Madrid'))))
    expect_that(RepoTimeTolubri('BB52014'), 
                is_identical_to(list(BB52014 = 
                                       lubridate::interval(start = '2014-09-01',
                                                  end = '2014-10-31',
                                                  tzone = 'Europe/Madrid'))))
    expect_that(RepoTimeTolubri('TT22013'), 
                is_identical_to(list(TT22013 = 
                                       lubridate::interval(start = '2013-04-01',
                                                  end = '2013-06-30',
                                                  tzone = 'Europe/Madrid'))))
    expect_that(RepoTimeTolubri('SS12011'), 
                is_identical_to(list(SS12011 = 
                                       lubridate::interval(start = '2011-01-01',
                                                  end = '2011-06-30',
                                                  tzone = 'Europe/Madrid'))))
    expect_that(RepoTimeTolubri('AA2009'), 
                is_identical_to(list(AA2009 = 
                                       lubridate::interval(start = '2009-01-01',
                                                  end = '2009-12-31',
                                                  tzone = 'Europe/Madrid'))))
    
})