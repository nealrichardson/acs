# an internal function to generate urls variables look like
# c('B21003_001E','B21003_001M') geo.call should have 'for' and 'in' 'for' is
# pairlist, like pairlist(county=05) or c('block+group'='*') 'in' is pairlist
# including none or more of: state, county, tract, each either number or '*'
api.url.maker <- function(endyear, span, key, variables, dataset, geo.call) {
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
        "?key=", key, "&get=", variables, ",NAME&", api.for, api.in)
    api.url
}

# a function to install a users key for use in this and future sessions writes
# to package extdata directory
## N.B. You can also just put it in `options` in your .Rprofile


#' Installs an API key from the US Census to use with calls to
#' \code{acs.fetch}.
#' 
#' The \code{acs.fetch} function requires an "API key" to use when downloading
#' data from the US Census API.  Rather than pass this rather long string to
#' the function each time, users can save the key as part of the package
#' installation, using the \code{api.key.install} function. Once installed, an
#' api key is saved on the system and available for use in future sessions.
#' (To replace a key, simply call the function again with the new key.)
#' 
#' 
#' The requirement for a key seems to be laxly enforced by the Census API, but
#' is nonetheless coded into the acs package.  Users without a key may find
#' success by simply installing a blank key (i.e., key="") via
#' \code{api.key.install(key="")}; similarly, calls to \code{acs.fetch} and
#' \code{geo.make(..., check=TRUE)} may success with a \code{key=""} argument.
#' Note that while this may work today, it may fail in the future if the API
#' decides to start enforcing the requirement.
#' 
#' 
#' @aliases api.key.install api.key.load
#' @param key The API key provided to the user upon registering with the US
#' Census Developer's page.  A string.
#' @return Saves the key and exits silently, unless an error is encountered.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso \code{\link{acs.fetch}}
#' @references To request an API key, see
#' \url{http://www.census.gov/developers/}
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



#' After updating the acs package, installs an archived API key from a
#' previous installation.
#' 
#' The \code{acs.fetch} function requires an "API key" to use when downloading
#' data from the US Census API.  Rather than pass this rather long string to
#' the function each time, users can save the key as part of the package
#' installation, using the \code{api.key.install} function. Once installed, an
#' api key is saved on the system and available for use in future sessions.
#' (To replace a key, simply call the function again with the new key.)
#' 
#' During the update process, this key may be lost or left in the wrong
#' location.  A call to \code{api.key.migrate()} can help restore an archived
#' key, if found.
#' 
#' 
#' @return Migrates the key (if found) and exits silently; return an error if
#' no archived key is found.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso \code{\link{acs.fetch}} \code{\link{api.key.install}}
#' @references To request an API key, see
#' \url{http://www.census.gov/developers/}
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
