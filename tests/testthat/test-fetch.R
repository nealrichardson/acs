context("acs.fetch")

test_that("Fetch with valid query", {
    skip("This fails.")
    vars <- acs.fetch(dataset="sf3", keyword="SCHOOL/ELEM OR HS", endyear=1990,
        case.sensitive=FALSE, geo=geo.make(state="CA"))
    print(str(vars))
})

test_that("Fetch with valid query", {
    d <- acs.fetch(dataset="sf3", variable="P147H010", endyear=2000,
        geo=geo.make(state="CA"))
    expect_equal(estimate(d)[1,1], 93774)
    expect_output(print(d), "California 93774 +/- 0", fixed=TRUE)
})