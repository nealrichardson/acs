context("acs.lookup")

test_that("acs.lookup finds variables in census", {
    vars <- acs.lookup(dataset="sf3", keyword="SCHOOL/ELEM OR HS", endyear=1990,
        case.sensitive=FALSE)
    expect_identical(nrow(results(vars)), 2L)
})

test_that("acs.lookup finds variables in acs", {
    vars <- acs.lookup(dataset="acs", keyword="private school", endyear=2015,
        case.sensitive=FALSE)
    expect_identical(nrow(results(vars)), 32L)
})

test_that("acs.lookup doesn't find vars", {
    expect_warning(vars <- acs.lookup(dataset="sf3", keyword="school/elem",
        endyear=1990, case.sensitive=TRUE))
    skip("The function just returns 'NA'")
    expect_identical(nrow(vars@results), 0L)
})

test_that("lookup doesn't leave ragged data.frame", {
    skip("Error: arguments imply differing number of rows: 478, 475")
    vars <- acs.lookup(dataset="sf3", keyword="school", endyear=2000,
        case.sensitive=FALSE)
})
