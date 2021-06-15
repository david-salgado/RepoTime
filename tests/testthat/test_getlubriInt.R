library(RepoTime)
context("Getter getlubriInt")

test_that("Getter returns a list of Interval class objects", {
    expect_true(
        all(sapply(getlubriInt(newRepoTime('MM022013')), class) == 'Interval'))
    expect_true(
        all(sapply(getlubriInt(newRepoTime(c('MM022013', 
                                             'TT32015',
                                             'QQ1042017',
                                             'AA1992'))), class) == 'Interval'))
})

test_that("Getter returns a list of the correct Interval class objects", {
    interv <- list(MM022013 = lubridate::interval(start = '2013-02-01',
                                       end = '2013-02-28',
                                       tzone = 'Europe/Madrid'))
    expect_true(
        getlubriInt(newRepoTime('MM022013'))[[1]] == interv[[1]])
    
    interv <- list(MM022013 = lubridate::interval(start = '2013-02-01',
                                       end = '2013-02-28',
                                       tzone = 'Europe/Madrid'),
                   TT32015 = lubridate::interval(start = '2015-07-01',
                                      end = '2015-09-30',
                                      tzone = 'Europe/Madrid'),
                   QQ1042017 = lubridate::interval(start = '2017-04-01',
                                        end = '2017-04-15',
                                        tzone = 'Europe/Madrid'),
                   AA1992 = lubridate::interval(start = '1992-01-01',
                                     end = '1992-12-31',
                                     tzone = 'Europe/Madrid'))
    lubriList <- getlubriInt(newRepoTime(c('MM022013', 
                                           'TT32015',
                                           'QQ1042017',
                                           'AA1992')))
    CompVec <- c()
    for (i in seq(along = interv)){
        
        CompVec <- c(CompVec, interv[[i]] == lubriList[[i]])
        
    }
    expect_true(all(CompVec))
})