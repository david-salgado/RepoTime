library(RepoTime)
library(lubridate)
context("Function lubriToRepoTime")

test_that("lubriToRepoTime reports error when input interval does not begin and/or end with the valid days of the month.", {
    expect_that(
            lubriToRepoTime(interval(start = '2016-01-02', 
                                     end = '2016-01-31', 
                                     tzone = 'Europe/Madrid')
            ), 
            throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2016-01-01', 
                                 end = '2016-01-30', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2016-02-02', 
                                 end = '2016-01-28', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2016-02-01', 
                                 end = '2016-02-28', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-03-02', 
                                 end = '2012-01-31', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-03-01', 
                                 end = '2012-01-30', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-04-02', 
                                 end = '2012-01-30', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-04-01', 
                                 end = '2012-04-31', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-05-02', 
                                 end = '2012-05-31', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-05-01', 
                                 end = '2012-05-30', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-06-02', 
                                 end = '2012-06-30', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-06-01', 
                                 end = '2012-06-31', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-07-02', 
                                 end = '2012-07-31', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-07-01', 
                                 end = '2012-07-30', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-08-02', 
                                 end = '2012-08-15', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-08-01', 
                                 end = '2012-08-16', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-09-02', 
                                 end = '2012-09-30', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-09-01', 
                                 end = '2012-09-31', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-10-02', 
                                 end = '2012-10-15', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-10-16', 
                                 end = '2012-10-30', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-11-02', 
                                 end = '2012-11-15', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-11-16', 
                                 end = '2012-11-31', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-12-02', 
                                 end = '2012-12-15', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    expect_that(
        lubriToRepoTime(interval(start = '2012-12-16', 
                                 end = '2012-12-30', 
                                 tzone = 'Europe/Madrid')
        ), 
        throws_error())
    
})
test_that("lubriToRepoTime makes the correct transformations", {
    expect_that(lubriToRepoTime(interval(start = '2016-01-01',
                                         end = '2016-01-15',
                                         tzone = 'Europe/Madrid')), 
                is_identical_to('QQ1012016'))
    expect_that(lubriToRepoTime(interval(start = '2015-02-16',
                                         end = '2015-02-28',
                                         tzone = 'Europe/Madrid')), 
                is_identical_to('QQ2022015'))
    expect_that(lubriToRepoTime(interval(start = '2016-02-01',
                                         end = '2016-02-29',
                                         tzone = 'Europe/Madrid')), 
                is_identical_to('MM022016'))
    expect_that(lubriToRepoTime(interval(start = '2014-09-01',
                                         end = '2014-10-31',
                                         tzone = 'Europe/Madrid')), 
                is_identical_to('BB52014'))
    expect_that(lubriToRepoTime(interval(start = '2013-04-01',
                                         end = '2013-06-30',
                                         tzone = 'Europe/Madrid')), 
                is_identical_to('TT22013'))
    expect_that(lubriToRepoTime(interval(start = '2011-01-01',
                                         end = '2011-06-30',
                                         tzone = 'Europe/Madrid')), 
                is_identical_to('SS12011'))
    expect_that(lubriToRepoTime(interval(start = '2009-01-01',
                                         end = '2009-12-31',
                                         tzone = 'Europe/Madrid')), 
                is_identical_to('AA2009'))
})