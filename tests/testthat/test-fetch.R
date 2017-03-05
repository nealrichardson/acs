context("acs.fetch")

test_that("1990 census doesn't take 'NAME'", {
    expect_error(.acsGET("http://api.census.gov/data/1990/sf3?key=&get=P0540003,P0540004,NAME&for=state:6"),
        "error: error: unknown variable 'NAME'")
    expect_is(acs.fetch(dataset="sf3",
            keyword="SCHOOL/ELEM OR HS", endyear=1990,
            case.sensitive=FALSE, geo=geo.make(state="CA")),
        "acs")
})

with_mock_API({
    test_that("Fetch from census with valid query", {
        d <- acs.fetch(dataset="sf3", variable="P147H010", endyear=2000,
            geo=geo.make(state="CA"))
        expect_equal(estimate(d)[1,1], 93774)
        expect_output(print(d), "California 93774 +/- 0", fixed=TRUE)
    })

    test_that("Fetch from ACS with valid query", {
        d <- acs.fetch(dataset="acs", variable="B14003_043", endyear=2015,
            geo=geo.make(state="CA"))
        expect_equal(estimate(d)[1,1], 107747)
        expect_output(print(d), "California 107747 +/- 2255", fixed=TRUE)
    })

    test_that("Fetch from ACS with lookup", {
        d <- acs.fetch(keyword="private school", endyear=2014,
            geo=geo.make(state="CA"))
        expect_equal(estimate(d)[1,1], 713682)
        expect_output(print(d), "B14003_012      B14003_013      B14003_014")
    })

    test_that("Fetch with lots of variables (recursion)", {
        z <- acs.fetch(dataset="acs", table.name="SEX BY SCHOOL ENROLLMENT BY TYPE OF SCHOOL BY AGE FOR THE POPULATION 3 YEARS AND OVER", endyear=2015, case.sensitive=FALSE, geo=geo.make(state="CA"))
        expect_identical(dim(z), c(1L, 57L))
    })
})
## TODO: get more working examples with different data shapes from multiple years
## then swap out the data fetching service so we can get the actual 400 error messages for when the queries fail.
