context("API keys")

test_that("API key can be 'installed'", {
    expect_false(file.exists(keyfile))
    api.key.install("foo")
    expect_true(file.exists(keyfile))
    load(keyfile)
    expect_identical(key, "foo")
})

test_that("API key can be read from options", {
    options(census.api.key="bar")
    expect_identical(api.key.load(), "bar")
})

test_that("if not set in options, loads the key.rda", {
    options(census.api.key=NULL)
    expect_identical(api.key.load(), "foo")
    expect_identical(getOption("census.api.key"), "foo")
})

test_that("if neither are set, key is empty string", {
    options(census.api.key=NULL)
    file.remove(keyfile)
    expect_identical(api.key.load(), "")
})
