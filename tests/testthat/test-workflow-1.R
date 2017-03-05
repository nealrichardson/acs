context("A working example")

## TODO: mock this request
with_mock_API({
    z <- acs.fetch(dataset="acs", table.name="SEX BY SCHOOL ENROLLMENT BY TYPE OF SCHOOL BY AGE FOR THE POPULATION 3 YEARS AND OVER", endyear=2015, case.sensitive=FALSE, geo=geo.make(state="CA", county="*"))
})


test_that("Fetch with lots of variables (recursion)", {
    expect_identical(dim(z), c(58L, 57L))
    ## TODO: This should be a "names" method
    expect_identical(acs.colnames(z)[1:3],
        c("B14003_001", "B14003_002", "B14003_003"))
})

n <- acs.lookup(dataset="acs", table.name="SEX BY SCHOOL ENROLLMENT BY TYPE OF SCHOOL BY AGE FOR THE POPULATION 3 YEARS AND OVER", endyear=2015, case.sensitive=FALSE)
acs.colnames(z) <- results(n)$variable.name[match(acs.colnames(z), results(n)$variable.code)]

test_that("Lookup the names and assign them", {
    expect_identical(acs.colnames(z)[1:3],
        c("Total:", "Male:", "Male: Enrolled in public school:"))
})

z2 <- z[,2:29] + z[,30:57]
test_that("Subsetting and adding 'acs' objects", {
    expect_identical(dim(z2), c(58L, 28L))
})

pub <- apply(z2[,4:7], 2, sum)
priv <- apply(z2[,13:16], 2, sum)
test_that("apply", {
    ## TODO: this is backwards right?
    expect_identical(dim(pub), c(58L, 1L))
})

test_that("division", {
    expect_warning(100 * priv / (pub + priv),
        "using the more conservative formula")
    expect_warning(out <- divide.acs(priv, pub + priv, method="proportion"),
        "assumes that numerator is a SUBSET")
    expect_output(print(100 * out[order(estimate(out), decreasing=TRUE),]),
        "San Francisco County, California   28.43")
})
