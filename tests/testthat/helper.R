Sys.setlocale("LC_COLLATE", "C") ## What CRAN does; affects sort order
set.seed(999) ## To ensure that tests that involve randomness are reproducible

keyfile <- file.path(system.file(package="acs"), "extdata", "key.rda")
if (file.exists(keyfile)) {
    load(keyfile)
    oldkey <- key
}
old <- options(census.api.key="")

bye <- new.env()
reg.finalizer(bye, function (x) {
    ## Restore any key options set in the tests
    do.call(options, old)
    if (exists("oldkey")) {
        api.key.install(oldkey)
    }
})
