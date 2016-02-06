library(RepoTime)
context("Function +")

test_that("+ returns the correct value", {
    expect_that(
        newRepoTime('QQ1012015') + days(14),
        is_identical_to(newRepoTime('QQ1012015'))
    )
    expect_that(
        newRepoTime('QQ1012015') + days(15),
        is_identical_to(newRepoTime('QQ2012015'))
    )
    expect_that(
        newRepoTime('QQ1012015') + months(1),
        is_identical_to(newRepoTime('QQ1022015'))
    )
    expect_that(
        newRepoTime('QQ1012015') + years(1),
        is_identical_to(newRepoTime('QQ1012016'))
    )
    expect_that(
        newRepoTime('MM012015') + days(30),
        is_identical_to(newRepoTime('MM012015'))
    )
    expect_that(
        newRepoTime('MM012015') + days(31),
        is_identical_to(newRepoTime('MM022015'))
    )
    expect_that(
        newRepoTime('MM012015') + months(2),
        is_identical_to(newRepoTime('MM032015'))
    )
    expect_that(
        newRepoTime('MM012015') + years(3),
        is_identical_to(newRepoTime('MM012018'))
    )
    expect_that(
        newRepoTime('BB12015') + months(1),
        is_identical_to(newRepoTime('BB12015'))
    )
    expect_that(
        newRepoTime('BB12015') + months(2),
        is_identical_to(newRepoTime('BB22015'))
    )
    expect_that(
        newRepoTime('BB12015') + years(2),
        is_identical_to(newRepoTime('BB12017'))
    )
    expect_that(
        newRepoTime('TT12015') + months(2),
        is_identical_to(newRepoTime('TT12015'))
    )
    expect_that(
        newRepoTime('TT12015') + months(3),
        is_identical_to(newRepoTime('TT22015'))
    )
    expect_that(
        newRepoTime('TT12015') + years(2),
        is_identical_to(newRepoTime('TT12017'))
    )
    expect_that(
        newRepoTime('SS12015') + months(5),
        is_identical_to(newRepoTime('SS12015'))
    )
    expect_that(
        newRepoTime('SS12015') + months(6),
        is_identical_to(newRepoTime('SS22015'))
    )
    expect_that(
        newRepoTime('SS12015') + years(1),
        is_identical_to(newRepoTime('SS12016'))
    )
    expect_that(
        newRepoTime('AA2015') + months(5),
        is_identical_to(newRepoTime('AA2015'))
    )
    expect_that(
        newRepoTime('AA2015') + months(12),
        is_identical_to(newRepoTime('AA2016'))
    )
    expect_that(
        newRepoTime('AA2015') + years(1),
        is_identical_to(newRepoTime('AA2016'))
    )
})

test_that("+ reports error when input objects are not of class RepoTime + duration", {
    expect_that(
        newRepoTime('QQ1012015') + newRepoTime('QQ2012015'),
        throws_error()
    )
    
})