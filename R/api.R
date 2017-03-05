# an internal function to generate urls variables look like
# c('B21003_001E','B21003_001M') geo.call should have 'for' and 'in' 'for' is
# pairlist, like pairlist(county=05) or c('block+group'='*') 'in' is pairlist
# including none or more of: state, county, tract, each either number or '*'
api.url.maker <- function (endyear, span, key, variables, dataset, geo.call) {
    ## 1990 census calls "NAME" "ANPSADPI"
    variables <- c(variables, ifelse(endyear == 1990, "ANPSADPI", "NAME"))

    variables <- paste0(variables, collapse = ",")
    if (span == 0) {
        span <- ""
    }
    api.for <- paste0("for=", paste(names(api.for(geo.call)), api.for(geo.call),
        sep = ":"))
    api.in <- paste0(paste(names(api.in(geo.call)), api.in(geo.call), sep = ":"),
        collapse = "+")
    if (!identical(api.in, ""))
        api.in <- paste0("&in=", api.in)
    api.url <- paste0("http://api.census.gov/data/", endyear, "/", dataset, span,
        "?key=", key, "&get=", variables, "&", api.for, api.in)
    api.url
}

# a function to install a users key for use in this and future sessions writes
# to package extdata directory
## N.B. You can also just put it in `options` in your .Rprofile
api.key.install <- function (key) {
    extdata.path <- file.path(system.file(package = "acs"), "extdata")
    if (!dir.exists(extdata.path)) {
        dir.create(extdata.path)
    }
    save(key, file=file.path(extdata.path, "key.rda"))
}

api.key.load <- function () {
    key <- getOption("census.api.key")
    if (is.null(key)) {
        ## It's not set. Try key.rda
        keyfile <- file.path(system.file(package="acs"), "extdata", "key.rda")
        if (file.exists(keyfile)) {
            load(keyfile)
            ## Set it in options for next time
            options(census.api.key=key)
        } else {
            ## Empty key works sometimes!
            key <- ""
        }
    }
    return(key)
}

# a function to migrate a previously installed api.key after package update
# writes to package extdata directory

api.key.migrate <- function() {
    key.path <- system.file("../key-holder.rda", package = "acs")
    if (file.exists(key.path)) {
        dir.create(paste(system.file(package = "acs"), "extdata", sep = "/"), showWarnings = FALSE)
        file.copy(from = key.path, to = paste(system.file("extdata", package = "acs"),
            "key.rda", sep = "/"), overwrite = TRUE)
    } else {
        warning("No archived key found;\n  try api.key.install with new key.")
    }
}
