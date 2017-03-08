context("geo.make")

test_that("geo.make should error if it can't complete", {
    expect_error(geo.make(state.legislative.district.upper="*"),
        "state required")
})

test_that("Can make geo for county=*", {
    expect_is(geo.make(county="*"), "geo.set")
})
