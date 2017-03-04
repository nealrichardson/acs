#' Download, Manipulate, and Present American Community Survey and Decennial
#' Data from the US Census
#'
#'
#' Provides a general toolkit for downloading, managing, analyzing, and
#' presenting data from the U.S. Census, including SF1 (Decennial
#' "short-form"), SF3 (Decennial "long-form"), and the American Community
#' Survey (ACS).  Confidence intervals provided with ACS data are converted to
#' standard errors to be bundled with estimates in complex acs objects.
#' Package provides new methods to conduct standard operations on acs objects
#' and present/plot data in statistically appropriate ways. Current version is
#' 2.0 +/- .033.
#'
#' \tabular{ll}{ Package: \tab acs\cr Type: \tab Package\cr Version: \tab
#' 2.0\cr Date: \tab 2016-03-08\cr License: \tab GPL-3\cr Depends: \tab
#' stringr, methods, plyr, XML\cr }
#'
#' The package defines a new "acs" class (containing estimates, standard
#' errors, geography, and metadata for tables from the U.S. Census American
#' Community Survey), with methods to deal appropriately with common tasks,
#' such as combining subgroups or geographies, mathematical operations on
#' estimates, tests of significance, and computing (and plotting) confidence
#' intervals.
#'
#' @name acs-package
#' @aliases acs-package acs
#' @docType package
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @references \enumerate{
#'
#' \item{"A Compass for Understanding and Using American Community Survey Data:
#' What State and Local Governments Need to Know." Washington, DC: U.S. Census
#' Bureau. 2009.
#' \url{http://www.census.gov/library/publications/2009/acs/state-and-local.html}.}
#'
#' \item{"acs.R: An R Package for Neighborhood-Level Data from the U.S.
#' Census." Ezra Haber Glenn, Department of Urban Studies and Planning,
#' Massachusetts Institute of Technology.  Presented at the Computers in Urban
#' Planning and Urban Management Conference, July 6, 2011.
#' \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2171390}.}
#'
#' \item{"Working with acs.R (June 2013)", Ezra Haber Glenn.
#' \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2552524}}
#'
#' \item{CityState webpage: \url{http://eglenn.scripts.mit.edu/citystate/}}
#'
#' \item{User Group Mailing List:
#' \url{http://mailman.mit.edu/mailman/listinfo/acs-r} }}
#' @keywords package manip
NULL

#' Class \code{"acs"}
#'
#'
#' The acs class provides a convenient wrapper for demographic data from the
#' U.S. Census, especially the American Community Survey.  Estimates and
#' standard errors are kept together, along with geographic information and
#' metadata necessary to manipulate and analyze data in this form.
#'
#'
#' @name acs-class
#' @aliases acs-class acs.colnames,acs-method acs.units,acs-method
#' -,acs,acs-method -,acs,numeric-method -,numeric,acs-method /,acs,acs-method
#' /,acs,numeric-method /,numeric,acs-method [<-,acs-method [,acs-method
#' *,acs,acs-method *,acs,numeric-method *,numeric,acs-method +,acs,acs-method
#' +,acs,numeric-method +,numeric,acs-method estimate,acs-method
#' modified,acs-method show,acs-method span,acs-method
#' standard.error,acs-method summary,acs-method estimate modified span
#' standard.error acs.colnames acs.units dim.acs length.acs acs.colnames<-
#' acs.units<- is.acs span<- apply acs.colnames<-,acs-method
#' acs.units<-,acs-method is.acs span<-,acs-method apply,acs-method
#' @docType class
#' @section Objects from the Class:
#'
#' acs objects can be created by calls of the form \code{new("acs", ...)}, or
#' through helper functions provided by the package (currently \code{read.acs}
#' and \code{acs.fetch}), or from the output of subsetting or other calls on
#' existing acs objects.  Once created, acs objects can be manipulated through
#' new methods to deal appropriately with common analytical tasks such as
#' combining subgroups or geographies, mathematical operations on estimates,
#' and computing (and plotting) confidence intervals.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @keywords classes
#' @examples
#'
#' showClass("acs")
#' # load some data from the ACS
#' data(kansas09)
#' str(kansas09)
#'
#' # access slots
#' endyear(kansas09)
#' span(kansas09)
#' estimate(kansas09)[1:5,1:5]
#' standard.error(kansas09[1:5,1:5])
#'
#'
#' # subset
#' kansas09[1:4,6:9]
#'
#' # more complicated subsets
#' kansas09[c("Linn County, Kansas", "Wilson County, Kansas") ,
#'    grep(pattern="21.years", x=acs.colnames(kansas09))]
#'
#' # addition on estimates and errors
#' kansas09[1:4,25]+kansas09[1:4,49]
#'
#' # can even multiply and divide
#' # males per female, by county
#' kansas09[1:4,2]/kansas09[1:4,26]
#'
#' # (males<5 plus females<5) * 12
#' (kansas09[7,3]+kansas09[7,27]) * 12
#'
#' # some replacement: males<5 as a percentage of all males
#' kansas09[,3]=kansas09[,3]/kansas09[,2]
#'
NULL


# .acs.unit.levels includes all valid types of units for acs estimates
.acs.unit.levels <- c("count", "dollars", "proportion", "ratio", "other")

globalVariables(c("fips.state", "fips.school", "fips.county.subdivision",
    "fips.american.indian.area", "fips.county", "fips.place"))

# .acs.dimnames() ensures that a returned acs object includes proper row
# (geography) and column (col.names) labels

.acs.dimnames <- function(acs.obj) {
    dimnames(acs.obj@estimate) <- list(acs.obj@geography[[1]], acs.obj@acs.colnames)
    dimnames(acs.obj@standard.error) <- list(acs.obj@geography[[1]], acs.obj@acs.colnames)
    acs.obj
}

# .acs.combine.headers() create metadata and row/column names for operations that
# merge two acs units into one

.acs.combine.headers <- function(e1, e2, operator) {
    if (endyear(e1) == endyear(e2))
        ENDYEAR <- endyear(e1) else ENDYEAR <- NA_integer_
    if (currency.year(e1) == currency.year(e2))
        CURRENCY.YEAR <- currency.year(e1) else CURRENCY.YEAR <- NA_integer_
    if (span(e1) == span(e2))
        SPAN <- span(e1) else SPAN <- NA_integer_
    if (identical(geography(e1), geography(e2)))
        GEOGRAPHY <- geography(e1) else {
        GEOGRAPHY <- geography(e1)
        for (i in 1:length(geography(e1))) {
            if (!identical(geography(e1)[[i]], geography(e2)[[i]]))
                GEOGRAPHY[[i]] <- paste("(", GEOGRAPHY[[i]], operator, geography(e2)[[i]],
                  ")", sep = " ")
        }
    }
    if (identical(acs.colnames(e1), acs.colnames(e2))) {
        ACS.COLNAMES <- acs.colnames(e1)
        ACS.UNITS <- acs.units(e1)
    } else {
        ACS.COLNAMES <- paste("(", acs.colnames(e1), operator, acs.colnames(e2),
            ")", sep = " ")
        ACS.UNITS <- factor("other", levels = .acs.unit.levels)
    }
    header <- list(endyear = ENDYEAR, span = SPAN,
        currency.year = CURRENCY.YEAR,
        geography = GEOGRAPHY, acs.colnames = ACS.COLNAMES,
        acs.units = ACS.UNITS)
    header
}


# .acs.identify.units() used to set units in acs object; initially assumes all
# units are 'counts', then changes some to 'dollars' is the word 'dollars'
# matches in colnames.

.acs.identify.units <- function(acs.colnames) {
    acs.units <- rep("count", length(acs.colnames))
    dollar.index <- grep(pattern = "dollars", x = acs.colnames, fixed = TRUE)
    acs.units[dollar.index] <- "dollars"
    factor(acs.units, levels = .acs.unit.levels)
}


# .acs.make.constant.object() use this to create an acs object with some constant
# value in the estimate and 0 for all the standard errors -- helpful, for
# example, to add a certain number to every value.

.acs.make.constant.object <- function(value, template) {
    acs.obj <- template
    # if given a vector, replaces by row, not by column
    acs.obj@estimate[, ] <- value
    acs.obj@standard.error[, ] <- 0
    acs.obj@acs.colnames[1:length(acs.colnames(acs.obj))] <- as.character(value)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
}




# a function to download XML variable lookup files, to speed up future acs.lookup
# calls.



#' Downloads and stores XML tables used to lookup variable codes, table names,
#' and other metadata associated with acs package.
#'
#' To obtain variable codes and other metadata needed to access the Census
#' API, both \code{acs.fetch} and \code{acs.lookup} must consult various XML
#' lookup files, which are provided by the Census with each data release.  To
#' keep the acs package-size small, as of version 2.0 these files are accessed
#' online at run-time for each query.  As an alternative, users may use
#' \code{acs.tables.install} to download and archive all current tables
#' (approximately 10MB, as of version 2.0 release).
#'
#' Use of this function is completely optional and the package should work
#' fine without it (assuming the computer is online and is able to access the
#' lookup tables), but running it once may result in faster searches and
#' quicker downloads for all subsequent sessions.  (The results are saved and
#' archived, so once a user has run the function, it is unnecessary to run
#' again, unless the acs package is re-installed or updated.)
#'
#'
#' @return Downloads the files and saves them to the package's "extdata"
#' directory; return an error if no files found.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso \code{\link{acs.fetch}} \code{\link{acs.lookup}}
#' @references \url{http://www.census.gov/data/developers/data-sets.html}
acs.tables.install <- function() {
    filelist <- readLines("http://web.mit.edu/eglenn/www/acs/acs-variables/filelist.txt")
    dir.create(paste(system.file(package = "acs"), "extdata", sep = "/"), showWarnings = FALSE)
    for (i in filelist) {
        download.file(url = paste("http://web.mit.edu/eglenn/www/acs/acs-variables/",
            i, sep = ""), destfile = paste(system.file("extdata/", package = "acs"),
            i, sep = ""))
    }
}

setClass(Class = "acs", representation = representation(endyear = "integer", span = "integer",
    geography = "data.frame", acs.colnames = "character", modified = "logical", acs.units = "factor",
    currency.year = "integer", estimate = "matrix", standard.error = "matrix"), prototype(endyear = NA_integer_,
    span = NA_integer_, acs.units = factor(levels = .acs.unit.levels), currency.year = NA_integer_,
    modified = FALSE))

is.acs <- function (object) class(object) == "acs"

#' Return or replace endyear value from the metadata of an acs object.
#'
#' \code{endyear()} will return the (integer) value of the latest year of the
#' object (for example, for the 2005-2009 ACS survey, \code{endyear} = 2009.)
#' When used for assignment, \code{endyear<-} will change the value of the
#' endyear slot in an acs object, warning the user that this is an unusual
#' thing to do.
#'
#' Normal operations on acs objects should not involve altering the value of
#' endyear (although users may wish to change the value of currency.year for
#' comparisons with other objects).  Sometimes endyear may be set incorrectly
#' when data is imported, in which case \code{endyear<-} may be necessary.
#'
#' @name endyear
#' @aliases endyear<- endyear endyear,acs-method endyear<-,acs-method
#' @param object an acs object
#' @param value an integer to use as the new endyear
#' @return Returns (or replaces) an integer value from the \code{endyear} slot
#' of an object.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso
#'
#' \code{currency.year}, which is often what users will be intending to modify
#'
#' \code{\link{acs-class}}
NULL

#' @rdname endyear
setMethod("endyear", "acs", function(object) object@endyear)

setMethod("span", "acs", function(object) object@span)

#' Return or replace geography metadata of an acs object.
#'
#' \code{geography()} will return the geography of an acs object, as a
#' dataframe.  Depending on the format of the data at import (and possibly the
#' values of \code{geocols=}, if the object was created with \code{read.acs}),
#' this may have multiple columns, but the number of geographic rows should be
#' the same as the number of rows of the acs estimates and standard errors.
#'
#' When used for assignment, \code{geography<-} will change the values
#' contained in the metadata, replacing the existing dataframe with a new one.
#' To replace a single value or a limited subset, call with subsetting (e.g.,
#' \code{geography(object)[i,j]<-value} or
#' \code{geography(object)[[i]]<-value}; note that the brackets should occur
#' \emph{outside} the call -- you are subsetting the dataframe, not the
#' object).
#'
#' To help with replacement operations, the package provides a new
#' \code{prompt} method, which can be used to interactively set new values for
#' geography (as well as other metadata); see \code{prompt.acs}.
#'
#'
#' @name geography
#' @aliases geography<- geography geography,acs-method geography<-,acs-method
#' @param object an acs object
#' @param value a dataframe containing geographic metadata; must contain the
#' same number of rows as the object
#' @return Returns (or replaces) a dataframe containing the \code{geography}
#' slot of an object.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso
#'
#' \code{\link{prompt.acs}}, a helper function to interactively generate a new
#' geography dataframe to be used for replacement.
#'
#' \code{\link{acs-class}}
#'
#' @examples
#'
#' data(lawrence10)
#' geography(lawrence10)
#' str(geography(lawrence10))
NULL

#' @rdname geography
setMethod("geography", "acs", function(object) object@geography)
setMethod("acs.colnames", "acs", function(object) object@acs.colnames)

#' Return (or change) currency.year value from the metadata of an acs object.
#'
#' Standard accessor/replacement method for metadata contained within S4
#' acs-class objects.
#'
#' \code{currency.year} will return the (integer) value of the dollar-year of
#' object.
#'
#' Assigning a new value to currency.year (through
#' \code{currency.year(object)<-value} or \code{currency.year(object)=value})
#' will change the value of \code{currency.year} in the object's metadata and
#' also modify all dollar values of the object (as determined by
#' \code{acs.units(object)=="dollars"}) to be in the dollars of the desired
#' new year.
#'
#' A related function, \code{currency.convert} provides a helper function to
#' create a new copy of an acs-class object with a modified currency.year and
#' converted dollar values without altering the original object.  When
#' \code{rate="auto"} (the default), \code{currency.convert} will look up
#' values from the \code{cpi} database to use in conversion.  When a numeric
#' rate is provided through this option, actual \code{cpi} values are ignored.
#' When \code{verbose=TRUE}, currency.convert will provide additional
#' information about the rates of conversion and the acs.colnames converted.
#'
#' As of version 2.0 the package includes CPI data from 1913 through 2015,
#' allowing conversion of dollar values for any years in this range.
#'
#' @name currency.year
#' @aliases currency.year currency.year<- currency.year,acs-method
#' currency.year<-,acs-method
#' @param object an acs object
#' @param value an integer value to be used in replacement
#' @return Returns (or replaces) an integer value from the "currency.year"
#' slot of an object.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso
#'
#' \code{\link{cpi}}
#'
#' \code{\link{currency.convert}}
#'
#' \code{\link{acs-class}}
NULL

#' @rdname currency.year
setMethod("currency.year", "acs", function(object) object@currency.year)
setMethod("modified", "acs", function(object) object@modified)
setMethod("acs.units", "acs", function(object) object@acs.units)
setMethod("estimate", "acs", function(object) object@estimate)
setMethod("standard.error", "acs", function(object) object@standard.error)

setMethod("[", "acs", function (x, i, j, ..., drop = FALSE) {
    if (missing(i))
        i <- 1:dim(x@estimate)[1]
    if (missing(j))
        j <- 1:dim(x@estimate)[2]
    new(Class = "acs", endyear = endyear(x), span = span(x),
        geography = geography(x)[i, ], acs.colnames = acs.colnames(x)[j],
        modified = modified(x), acs.units = acs.units(x)[j],
        currency.year = currency.year(x), estimate = estimate(x)[i, j, drop = F],
        standard.error = standard.error(x)[i, j, drop = F])
})

setReplaceMethod("[", "acs", function (x, i, j, value) {
    if (missing(i))
        i <- 1:dim(x)[1]
    if (missing(j))
        j <- 1:dim(x)[2]
    # is value acs object? ## still need to check for metadata being the same
    if (is.acs(value) && all(dim(value) == c(length(i), length(j)))) {
        if (endyear(x) != endyear(value)) {
            warning("original and replacement do not have same endyear;\nkeeping original value",
                call. = FALSE)
        }
        if (span(x) != span(value)) {
            warning("original and replacement do not have same span;\nkeeping original value",
                call. = FALSE)
        }
        if (currency.year(x) != currency.year(value)) {
            warning("original and replacement do not have same currency.year;\nkeeping original value",
                call. = FALSE)
        }
        x@estimate[i, j] <- value@estimate
        x@standard.error[i, j] <- value@standard.error
        # check for mismatch geo when not all cols changed if not identical geogs if
        # changing all cols or more
        if (!all(geography(x[i, j]) == geography(value))) {
            if (dim(x)[2] <= length(j)) {
                x@geography[i, ] <- geography(value)  # change all geo
                warning("geographies do not match but all columns changed;\nusing new geographies",
                  call. = FALSE)
            } else {
                warning("geographies do not match but some columns retained;\nkeeping original geography values",
                  call. = FALSE)
            }
        }
        if (!all(acs.colnames(x[i, j]) == acs.colnames(value))) {
            # if not identical colnames if not changing all rows or more
            if (dim(x)[1] <= length(i)) {
                x@acs.colnames[j] <- acs.colnames(value)
                warning("acs.colnames do not match but all rows changes;\nusing new acs.colnames",
                  call. = FALSE)
            } else {
                warning("acs.colnames do not match but some rows retained;\nkeeping original acs.colnames",
                  call. = FALSE)
            }
        }
        # is value two item list?
    } else if (is.list(value) && length(value) == 2) {
        if (is.null(value$estimate))
            x@estimate[i, j] <- value[[1]] else x@estimate[i, j] <- value$estimate
        if (is.null(value$standard.error)) {
            if (is.null(value$error))
                x@standard.error[i, j] <- value[[2]] else x@standard.error[i, j] <- value$error
        } else x@standard.error[i, j] <- value$standard.error
    } else if (is.numeric(value)) {
        x@estimate[i, j] <- value
        x@standard.error[i, j] <- 0
    } else {
        stop("incompatible objects or dimensions;\nunable to parse for replacement",
            call. = FALSE)
    }
    x@modified <- TRUE
    x <- .acs.dimnames(x)  # in case geography or acs.colnames changed
    validObject(x)
    return(x)
})

cbind.acs <- function(e1, e2) {
    if (e1@endyear != e2@endyear | e1@span != e2@span) {
        warning("** acs objects x and y must have same endyear and span;\nreturning NA **")
        return(NA)
    }
    if (identical(geography(e1), geography(e2)))
        GEOGRAPHY <- geography(e1) else {
        warning("geographies do not appear to match; using first geography")
        GEOGRAPHY <- geography(e1)
    }
    NEW.ESTIMATE <- cbind(estimate(e1), estimate(e2))
    NEW.ERROR <- cbind(standard.error(e1), standard.error(e2))
    acs.obj <- new(Class = "acs", endyear = endyear(e1), span = span(e1), modified = TRUE,
        geography = GEOGRAPHY, acs.units = factor(c(acs.units(e1), acs.units(e2)),
            levels = .acs.unit.levels), currency.year = currency.year(e1), acs.colnames = c(acs.colnames(e1),
            acs.colnames(e2)), estimate = NEW.ESTIMATE, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
}



#' Combine acs Objects by Rows or Columns
#'
#' Take a pair of acs objects and combine by _c_olumns or _r_ows,
#' respectively.
#'
#' When passed two acs-class objects, rbind (and cbind) will first check to
#' confirm whether the objects contain compatible data: same endyear and span;
#' same column names (for rbind) or geography (for cbind).  If not, it will
#' issue a warning, but will still proceed.
#'
#' After this check, the function will return a new acs object that has
#' resulted from combining the two arguments row-wise or column-wise.  The
#' effect is essentially the same as rbind (or cbind) on the underlying
#' estimate and standard.error matrices, with all the additional acs metadata
#' tended to.
#'
#' @aliases rbind.acs rbind cbind.acs cbind
#' @param e1,e2 two acs-class objects
#' @return Returns a single new acs object with all of the data contained in
#' the two arguments.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
rbind.acs <- function(e1, e2) {
    if (e1@endyear != e2@endyear | e1@span != e2@span) {
        warning("** acs objects x and y must have same endyear and span;\nreturning NA **")
        return(NA)
    }
    if (identical(acs.colnames(e1), acs.colnames(e2)))
        ACS.COLNAMES <- acs.colnames(e1) else {
        warning("columns do not appear to match; using first colnames")
        ACS.COLNAMES <- acs.colnames(e1)
    }
    GEOGRAPHY <- rbind.fill(geography(e1), geography(e2))
    NEW.ESTIMATE <- rbind(estimate(e1), estimate(e2))
    NEW.ERROR <- rbind(standard.error(e1), standard.error(e2))
    acs.obj <- new(Class = "acs", endyear = endyear(e1), span = span(e1), modified = TRUE,
        geography = GEOGRAPHY, acs.units = acs.units(e1), currency.year = currency.year(e1),
        acs.colnames = acs.colnames(e1), estimate = NEW.ESTIMATE, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
}

setMethod("+", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
    header <- .acs.combine.headers(e1, e2, "+")
    NEW.ESTIMATE <- estimate(e1) + estimate(e2)
    NEW.ERROR <- sqrt(standard.error(e1)^2 + standard.error(e2)^2)
    acs.obj <- new(Class = "acs", endyear = header$endyear, span = header$span, modified = TRUE,
        geography = header$geography, acs.units = header$acs.units, currency.year = header$currency.year,
        acs.colnames = header$acs.colnames, estimate = NEW.ESTIMATE, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
})

setMethod("-", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
    header <- .acs.combine.headers(e1, e2, "-")
    NEW.ESTIMATE <- estimate(e1) - estimate(e2)
    NEW.ERROR <- sqrt(standard.error(e1)^2 + standard.error(e2)^2)
    acs.obj <- new(Class = "acs", endyear = header$endyear, span = header$span, modified = TRUE,
        geography = header$geography, acs.units = header$acs.units, currency.year = header$currency.year,
        acs.colnames = header$acs.colnames, estimate = NEW.ESTIMATE, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
})

.acs.divider <- function(num, den, proportion, verbose=FALSE, output="result") {
    header <- .acs.combine.headers(num, den, ifelse(proportion, "/", ":"))
    p <- estimate(num)/estimate(den)
    # start with proportion-style
    if (isTRUE(proportion)) {
        header$acs.units <- rep(factor("proportion", levels = .acs.unit.levels),
            length(header$acs.units))
        if (verbose) {
            warning("** using formula for PROPORTIONS, which assumes that numerator is a SUBSET of denominator **")
        }
        NEW.ERROR <- suppressWarnings(sqrt(standard.error(num)^2 - (p^2 * standard.error(den)^2))/estimate(den))
        # change all that are should be ratio-stye index for ratio corrections
        proportion.numerators <- suppressWarnings(sqrt(standard.error(num)^2 - (p^2 *
            standard.error(den)^2)))
        ratio.correct.index <- is.na(proportion.numerators)
        # if any fail the test
        if (any(ratio.correct.index)) {
            # use ratio-style correction
            ratio.errors <- sqrt(standard.error(num)^2 + (p^2 * standard.error(den)^2))/estimate(den)
            NEW.ERROR[ratio.correct.index] <- ratio.errors[ratio.correct.index]
            if (verbose) {
                warning(paste("**Note: due to the nature of the errors in some cells,\n  they were divided using the more conservative formula for RATIOS\n  which assumes that numerator is not a subset of denominator**:\n  in total, ",
                  sum(ratio.correct.index), " ratio-style divisions substituted;\n  see ?divide.acs and/or use output=\"result\" for more.",
                  sep = ""))
            }
            if (output == "both" | output == "div.method") {
                ratio.report <- p  # just to get a matrix with the same dims
                colnames(ratio.report) <- header$acs.colnames
                rownames(ratio.report) <- header$geography[[1]]
                ratio.report[ratio.correct.index] <- "ratio"
                ratio.report[!ratio.correct.index] <- "proportion"
            }
        }
    } else {
        # ratio style
        header$acs.units <- rep(factor("ratio", levels = .acs.unit.levels), length(header$acs.units))
        NEW.ERROR <- sqrt(standard.error(num)^2 + (p^2 * standard.error(den)^2))/estimate(den)
        ratio.report <- p  # just to get a matrix with the same dims
        colnames(ratio.report) <- header$acs.colnames
        rownames(ratio.report) <- header$geography[[1]]
        ratio.report[, ] <- "ratio"
    }
    acs.obj <- new(Class = "acs", endyear = header$endyear, span = header$span, modified = TRUE,
        geography = header$geography, acs.units = header$acs.units, currency.year = header$currency.year,
        acs.colnames = header$acs.colnames, estimate = p, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    if (output == "both") {
        list(result = acs.obj, div.method = ratio.report)
    } else if (output == "div.method") {
        ratio.report
    } else {
        acs.obj
    }
}



setMethod("/", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
    # by default, use more conservative 'ratio-style' dividing
    warning("** using the more conservative formula for ratio-type
    dividing, which does not assume that numerator is a subset of
    denominator; for more precise results when seeking a proportion
    and not a ratio, use divide.acs(..., method=\"proportion\") **")
    .acs.divider(num = e1, den = e2, proportion = FALSE, verbose = FALSE)
})



#' Divide one acs object or variable by another.
#'
#' The \code{acs} package provides a new S4 method for standard division
#' operations using "/" notation.  However, due to the nature of estimates and
#' errors, there are actually two types of division, with slightly different
#' meanings: depending on which variables are being divided, the process may
#' be either a "proportion"-type division (in which the numerator is a subset
#' of the denominator) or a "ratio"-type division (in which this is not the
#' case).  When dividing with standard "a/b" notation, the package will always
#' use the more conservative ratio-type procedure.
#'
#' When appropriate, "proportion"-type division may be desirable, as it
#' results in lower standarard errors.  To allow users to specify which type
#' of division to use for acs objects, the package includes a new
#' \code{"divide.acs"} function.  (See details.)
#'
#' In certain cases, "proportion-style" division will fail, due to the
#' creation of a negative number under a square root when calculating new
#' standard errors.  To address this problem and prevent unnecessary NaN
#' values in the standard.errors, the package implements the recommended
#' Census practice of simply using "ratio-style" division in those cases.
#'
#' If method="proportion" (not the default) and verbose=TRUE(the default),
#' \code{division.acs} will provide a warning to indicate when "ratio-style"
#' division has been used, including the number of standard error cells so
#' affected.  Users wishing to examine a detailed, cell-by-cell report may run
#' \code{divide.acs} with the output="div.method" of output="both" to get
#' additional diagnostic information.
#'
#' See "A Compass for Understanding and Using American Community Survey Data"
#' below for details on when this substitution is recommended.
#'
#' @param numerator an acs object to divide
#' @param denominator an acs object to divide by
#' @param method either "ratio" (the default) or "proportion", to indicate
#' what kind of division is desired
#' @param verbose whether to provide additional warnings or just shut up
#' @param output either "result" (the default), "div.method", or "both"
#' @return Returns a new acs object with the results of the division (the
#' default), or (when result="div.method") a martix with diagnostic
#' information, or (when result="both"), a list with both of these objects
#' (the first name $result and the second $div.method).
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso \code{\link{acs-class}}
#' @references \enumerate{
#'
#' \item{"A Compass for Understanding and Using American Community Survey Data:
#' What State and Local Governments Need to Know." Washington, DC: U.S. Census
#' Bureau. 2009.
#' \url{http://www.census.gov/library/publications/2009/acs/state-and-local.html}.}
#' }
divide.acs <- function(numerator, denominator, method = "ratio", verbose = TRUE, output = "result") {
    if (identical(method, "ratio")) {
        .acs.divider(num = numerator, den = denominator, proportion = FALSE, verbose = verbose,
            output = output)
    } else if (identical(method, "proportion")) {
        .acs.divider(num = numerator, den = denominator, proportion = TRUE, verbose = verbose,
            output = output)
    } else {
        warning("Error: must set method to \"ratio\" or \"proportion\"")
        NA
    }
}

setMethod("*", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
    header <- .acs.combine.headers(e1, e2, "*")
    NEW.ESTIMATE <- estimate(e1) * estimate(e2)
    NEW.ERROR <- sqrt((estimate(e1)^2 * standard.error(e2)^2) + (estimate(e2)^2 *
        standard.error(e1)^2))
    acs.obj <- new(Class = "acs", endyear = header$endyear, span = header$span, modified = TRUE,
        geography = header$geography, acs.units = header$acs.units, currency.year = header$currency.year,
        acs.colnames = header$acs.colnames, estimate = NEW.ESTIMATE, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
})

setMethod("+", signature(e1 = "acs", e2 = "numeric"), function(e1, e2) {
    e2 <- .acs.make.constant.object(value = e2, template = e1)
    e1 + e2
})

# and reverse classes...

setMethod("+", signature(e1 = "numeric", e2 = "acs"), function(e1, e2) {
    e1 <- .acs.make.constant.object(value = e1, template = e2)
    e1 + e2
})

setMethod("-", signature(e1 = "acs", e2 = "numeric"), function(e1, e2) {
    e2 <- .acs.make.constant.object(value = e2, template = e1)
    e1 - e2
})

# ditto...

setMethod("-", signature(e1 = "numeric", e2 = "acs"), function(e1, e2) {
    e1 <- .acs.make.constant.object(value = e1, template = e2)
    e1 - e2
})

setMethod("/", signature(e1 = "acs", e2 = "numeric"), function(e1, e2) {
    e2 <- .acs.make.constant.object(value = e2, template = e1)
    e1/e2
})

# ditto...

setMethod("/", signature(e1 = "numeric", e2 = "acs"), function(e1, e2) {
    e1 <- .acs.make.constant.object(value = e1, template = e2)
    e1/e2
})

setMethod("*", signature(e1 = "acs", e2 = "numeric"), function(e1, e2) {
    e2 <- .acs.make.constant.object(value = e2, template = e1)
    e1 * e2
})

# ditto...

setMethod("*", signature(e1 = "numeric", e2 = "acs"), function(e1, e2) {
    e1 <- .acs.make.constant.object(value = e1, template = e2)
    e1 * e2
})

setMethod("show", signature(object = "acs"), function(object) {
    if (is.na(span(object)) | is.na(endyear(object)))
        years <- "NO MEANINGFUL YEAR" else if (span(object) == 0 | span(object) == 1)
        years <- endyear(object) else years <- paste(endyear(object) - span(object) + 1, "--", endyear(object))
    if (span(object) == 0) {
        dataset <- "Decennial Census (SF1/SF3)"
    } else {
        dataset <- "ACS"
    }
    cat(dataset, "DATA: \n", years, ";\n")
    cat("  Estimates w/90% confidence intervals;\n  for different intervals, see confint()\n")
    est <- estimate(object)
    err <- standard.error(object)
    output <- matrix(paste(est, "+/-", 1.645 * err), nrow = nrow(est))
    dimnames(output) <- dimnames(est)
    print(output, quote = FALSE)
})

setMethod("summary", signature(object = "acs"), function(object) {
    if (span(object) == 1)
        years <- endyear(object) else years <- paste(endyear(object) - span(object) + 1, "--", endyear(object))
    cat("ACS DATA: \n", years, "\n")
    cat("----------\nESTIMATES:\n")
    print(summary(estimate(object)))
    cat("----------\n90% MARGINS OF ERROR:\n")
    print(summary(1.645 * standard.error(object)))
})

dim.acs <- function(x) {
    dim(estimate(x))
}

length.acs <- function(x) {
    length(estimate(x))
}



#' Return upper and lower bounds of given confidence intervals for acs
#' objects.
#'
#' When passed an acs object, \code{confint} will return a list of two-column
#' dataframes (one dataframe for each variable specified in \code{parm})
#' including lower and upper bounds for given confidence intervals.  Intervals
#' can be one- or two-sided.
#'
#'
#' @aliases confint.acs confint
#' @param object a acs object (or subset).
#' @param parm which variables/columns to return confidence intervals for;
#' defaults to "all", which computes confidence intervals for all estimates in
#' the acs object.
#' @param level the confidence level required -- e.g., .95 = 95\% confidence.
#' @param alternative whether the interval should be one-sided (i.e.,
#' one-tailed -- "greater" or "less" -- extending to Inf (or -Inf) on one
#' side) or "two-sided".
#' @param ... additional argument(s) for methods.
#' @return Returns a list of dataframes (one for each variable specified in
#' \code{parm}) of the lower and upper bounds of the confidence interval for
#' each row of the data.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}.
#' @examples
#'
#'
#' # load ACS data
#' data(kansas09)
#'
#' # confidence intervals for select columns
#' confint(kansas09[20:25,], parm=c(4,5,10))
#'
#' # another way to accomplish this
#' confint(kansas09[20:25,c(4,5,10)])
#'
#' # store data and extract at will
#' my.conf <- confint(kansas09)
#' str(my.conf)
#' my.conf[32]
#' my.conf$Universe...TOTAL.POPULATION.IN.THE.UNITED.STATES..U.S..citizen.by.naturalization
#'
#' # try a different value for level
#' confint(kansas09[1:10,6], level=.75)
#'
#' # ... or a one-sided confidence interval
#' confint(kansas09[1:10,6], level=.75, alternative="greater")
#' confint(kansas09[1:10,29], level=.75, alternative="less")
#'
#'
confint.acs <- function(object, parm = "all", level = 0.95, alternative = "two.sided",
    ...) {
    if (parm[1] == "all")
        parm <- 1:dim(object)[2]
    z.upper <- switch(alternative, two.sided = qnorm((1 + level)/2), greater = Inf,
        less = qnorm(level))
    z.lower <- switch(alternative, two.sided = qnorm((1 + level)/2), greater = qnorm(level),
        less = Inf)
    labels <- switch(alternative, two.sided = c((1 - level)/2, 1 - (1 - level)/2),
        less = c(0, level), greater = c(1 - level, 1))
    labels <- paste(100 * labels, "%", sep = " ")
    RESULT <- list()
    for (i in parm) {
        conf.int.lower <- estimate(object[, i]) - standard.error(object[, i]) * z.lower
        conf.int.upper <- estimate(object[, i]) + standard.error(object[, i]) * z.upper
        RESULT[[acs.colnames(object)[i]]] <- data.frame(conf.int.lower, conf.int.upper,
            row.names = geography(object)[[1]])
        names(RESULT[[acs.colnames(object)[i]]]) <- labels
    }
    RESULT
}

setMethod("sum", "acs", function(x, agg.term = c("aggregate", "aggregate"),
    one.zero = FALSE, ..., na.rm = FALSE) {
    if (length(agg.term) < 2) {
        agg.term[2] <- agg.term[1]
    }
    est <- estimate(x)
    err <- standard.error(x)
    if (one.zero && any(est == 0)) {
        max.zero.error <- max(err[est == 0])
        err[est == 0] <- c(max.zero.error, rep(0, sum(est == 0) - 1))
    }
    if (dim(est)[1] == 1) {
        # single row
        geography <- geography(x)
    } else {
        geography <- geography(x[1, 1])
        for (i in 1:length(geography)) {
            geography[, i] <- agg.term[1]
        }
    }
    if (dim(est)[2] == 1) {
        acs.units <- acs.units(x)  # single column
        acs.colnames <- acs.colnames(x)
    } else {
        acs.units <- factor(levels = .acs.unit.levels)
        acs.colnames <- agg.term[2]
    }
    ESTIMATE <- as.matrix(sum(est))
    ERROR <- as.matrix(sqrt(sum(err^2)))
    acs.obj <- new(Class = "acs", endyear = endyear(x), span = span(x), modified = TRUE,
        geography = geography, acs.units = acs.units, currency.year = currency.year(x),
        acs.colnames = acs.colnames, estimate = ESTIMATE, standard.error = ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
})

## TODO: add names, sort methods

.apply.acs <- function(X, MARGIN, FUN, ...) {
    ## TODO: these are backwards I think.
    FUN <- match.fun(FUN)
    if (identical(MARGIN, 1)) {
        # apply row-wise
        acs.obj <- FUN(X[, 1], ...)
        if (dim(X)[2] > 1) {
            for (i in 2:dim(X)[2]) {
                acs.obj <- cbind(acs.obj, FUN(X[, i], ...))
            }
        }
    }
    if (identical(MARGIN, 2)) {
        # apply col-wise
        acs.obj <- FUN(X[1, ], ...)
        if (dim(X)[1] > 1) {
            for (i in 2:dim(X)[1]) {
                acs.obj <- rbind(acs.obj, FUN(X[i, ], ...))
            }
        }
    }
    # I think the next part works, except it fails because the stuff above doesn't
    # like single rows or single columns...  if (identical (MARGIN, c(1,2))){
    # acs.obj=apply(apply(X, MARGIN=1, FUN=FUN), MARGIN=2, FUN=FUN)}

    # or maybe this...?
    if (all(MARGIN == c(1, 2))) {
        acs.obj <- FUN(apply(X, MARGIN = 2, FUN = FUN, ...), ...)
    }
    acs.obj
}

setMethod("apply", signature = "acs", def = function(X, MARGIN, FUN, ...) {
    .apply.acs(X, MARGIN, FUN, ...)
})

setReplaceMethod(f = "acs.colnames", signature = "acs", definition = function(x,
    value) {
    x@acs.colnames <- value
    x <- .acs.dimnames(x)
    x@modified <- TRUE
    validObject(x)
    return(x)
})

#' @rdname geography
setReplaceMethod(f = "geography", signature = "acs", definition = function(object,
    value) {
    object@geography <- value
    object <- .acs.dimnames(object)
    object@modified <- TRUE
    validObject(object)
    return(object)
})

#' @rdname endyear
setReplaceMethod(f = "endyear", signature = "acs", definition = function(object,
    value) {
    warning(paste("Changing value of endyear from ", endyear(object), " to ", value,
        ".\nThis is an unusual thing to do, unless the original value was incorrect.\nAlso changing value of currency.year to",
        value, ", without converting currency values.\nPlease see ?endyear and ?currency.year for more information",
        sep = ""), call. = FALSE)
    object@endyear <- as.integer(value)
    object@currency.year <- as.integer(value)
    object@modified <- TRUE
    validObject(object)
    return(object)
})

setReplaceMethod(f = "span", signature = "acs", definition = function(x, value) {
    warning(paste("Changing value of span from ", span(x), " to ", value, ".\nThis is an unusual
                         thing to do, unless the original value was
                         incorrect.\nSee ?acs for more information",
        sep = ""), call. = FALSE)
    x@span <- as.integer(value)
    x@modified <- TRUE
    validObject(x)
    return(x)
})

setReplaceMethod(f = "acs.units", signature = "acs", definition = function(x, value) {
    x@acs.units <- factor(value, levels = .acs.unit.levels)
    x@modified <- TRUE
    validObject(x)
    return(x)
})

#' @rdname currency.year
setReplaceMethod(f = "currency.year", signature = "acs", definition = function(object,
    value) {
    currency.convert(object, rate = "auto", newyear = value)
})



#' Convert dollar values of acs object to a new base year.
#'
#' \code{currency.convert} provides a helper function to create a new copy of
#' an acs-class object with a modified currency.year and converted dollar
#' values without altering the original object.
#'
#'
#' \code{currency.convert} provides a helper function to create a new copy of
#' an acs-class object with a modified currency.year and converted dollar
#' values without altering the original object.  When \code{rate="auto"} (the
#' default), \code{currency.convert} will look up values from the \code{cpi}
#' database to use in conversion.  When a numeric rate is provided through
#' this option, actual \code{cpi} values are ignored.  When
#' \code{verbose=TRUE}, currency.convert will provide additional information
#' about the rates of conversion and the acs.colnames converted.
#'
#' As of version 2.0 the package includes CPI data from 1913 through 2015,
#' allowing conversion of dollar values for any years in this range.
#'
#' @aliases currency.convert currency.convert,acs-method
#' @param object an acs object
#' @param rate an optional rate to apply; "auto" (the default) will look up
#' values from the cpi dataset.
#' @param newyear an integer specifying the new value of currency.year to
#' convert into
#' @param verbose whether to print additional information about the conversion
#' @return Returns a new acs object with updated dollar values and
#' \code{currency.year} metadata.
#'
#' Unlike \code{currency.year<-}, \code{currency.convert} does not alter the
#' original object.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso
#'
#' \code{\link{currency.year}}
#'
#' \code{\link{cpi}}
#' @examples
#'
#' lawrence10                                   # median income data, endyear = 2010
#' currency.convert(lawrence10, newyear=2014)   # convert $$ to 2014 dollars
#' currency.convert(lawrence10, newyear=1929)   # convert $$ to 1929 dollars
#'
currency.convert <- function(object, rate = "auto", newyear = NA_integer_, verbose = FALSE) {
    if (rate == "auto") {
        .env <- environment()
        data("cpi", envir = .env)
        new.rate <- "cpi[as.character(newyear)]"
        new.rate <- eval(parse(text = new.rate))
        curr.rate <- "cpi[as.character(currency.year(object))]"
        curr.rate <- eval(parse(text = curr.rate))
        rate <- new.rate/curr.rate
    }
    dollar.cols <- which(acs.units(object) == "dollars")
    if (verbose) {
        if (!missing(newyear)) {
            output <- c(paste("CPI (base 1982-84) for ", currency.year(object), " = ",
                curr.rate, sep = ""), "\n", paste("CPI (base 1982-84) for ", newyear,
                " = ", new.rate, sep = ""), "\n", paste("$1.00 in ", currency.year(object),
                " dollars = $", round(rate, 2), " in ", newyear, " dollars.", sep = ""),
                "\n")
        } else {
            output <- c(paste("$1.00 in ", currency.year(object), " dollars = $",
                round(rate, 2), " in converted dollars.", sep = ""), "\n")
        }
        output <- c(output, "Converting the following columns:", "\n", paste(acs.colnames(object)[dollar.cols],
            "\n", sep = ""))
        warning(output, call. = FALSE)
    }
    for (i in dollar.cols) {
        object@estimate[, i] <- object@estimate[, i] * rate
        object@standard.error[, i] <- object@standard.error[, i] * rate
    }
    object@currency.year <- as.integer(newyear)
    object@modified <- TRUE
    validObject(object)
    return(object)
}

# helper function for replacing geography or acs.colnames



#' Prompt for new values for metadata in an acs object.
#'
#' Helper function to interactively set new values for row- and/or
#' column-names in an acs object.
#'
#' The acs package provides this S3 \code{prompt} method for acs-class
#' objects, primarily as a "helper" function to use in calls to
#' \code{geography(object)<-}, \code{acs.units(object)<-}, or
#' \code{acs.colnames(object)<-}.  \code{prompt} provides an interactive
#' interface, \code{prompt}ing the user for new metadata values based on the
#' existing ones.
#'
#' When \code{what="geography"} and \code{geocols} is not "all", \code{prompt}
#' will only prompt for replacements of the values of geocols, but will still
#' return values for all geography columns, suitable for passing to
#' \code{geography(object)<-}.
#'
#' Anytime during the interactive \code{prompt()} session, a user may enter a
#' blank line to terminate, returning only the changed values up to that point
#' (along with the unchanged values for remaining entries.)
#'
#' @aliases prompt.acs prompt
#' @param object an acs object
#' @param filename not used; provided for S3 generic/method consistency
#' @param name not used; provided for S3 generic/method consistency
#' @param what which acs-class metadata slot to prompt for; either
#' "acs.colnames" (the default), "acs.units", or "geography"
#' @param geocols a vector, or "all", specifying which columns from the
#' geography metadata to prompt for (optional; defaults to "all"; ignored when
#' \code{what="acs.colnames"})
#' @param ... not used; provided for S3 generic/method consistency
#' @return Returns a value of the same class and dimensions as the current
#' geography, acs.units, or acs.colnames of object, but with new names,
#' suitable for passing to one of the replacement methods
#' (\code{acs.colnames<-}, (\code{acs.units<-}, or \code{geography<-}).
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso
#'
#' \code{geography<-}
#'
#' \code{acs.colnames<-}
#'
#' \code{acs.units<-}
#' @examples
#'
#'
#' data(kansas07)
#'
#' acs.colnames(kansas07)=prompt(kansas07, what="acs.colnames")
#'
#' geography(kansas07)=prompt.acs(kansas07, what="geography")
#'
#'
#'
prompt.acs <- function(object, filename = NA, name = NA, what = "acs.colnames", geocols = "all",
    ...) {
    print("To end session, enter a blank line.")
    if (what == "geography") {
        if (geocols == "all")
            geocols <- 1:dim(geography(object))[2]
        value <- geography(object)
        for (j in geocols) {
            for (i in 1:dim(geography(object))[1]) {
                line.input <- readline(prompt = paste("Change ", value[i, j], " to: \n",
                  sep = ""))
                if (line.input == "") {
                  break
                } else {
                  value[i, j] <- line.input
                }
            }
        }
    } else if (what == "acs.colnames") {
        value <- acs.colnames(object)
        for (i in 1:length(acs.colnames(object))) {
            line.input <- readline(prompt = paste("Change ", value[i], " to: \n",
                sep = ""))
            if (line.input == "") {
                break
            } else {
                value[i] <- line.input
            }
        }
    } else if (what == "acs.units") {
        value <- acs.units(object)
        input <- rep("", length(value))
        print("Type [c]ount, [d]ollars, [p]roportion, [r]atio, or [o]ther.")
        for (i in 1:length(value)) {
            line.input <- readline(prompt = paste(acs.colnames(object)[i], " is currently in these units: ",
                value[i], ".  Change to what units?: (c,d,p,r,o)\n", sep = ""))
            if (line.input == "") {
                break
            } else {
                input[i] <- line.input
            }
        }
        for (i in .acs.unit.levels) {
            value[input == substr(start = 1, stop = 1, i)] <- i
        }
    } else {
        value <- NA
        warning(paste("prompt can only prompt for \"geography\", \"acs.units\", or \"acs.colnames\", not \"",
            what, "\"", sep = ""))
    }
    value
}
