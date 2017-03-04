#' Class \code{"acs.lookup"}
#'
#' A new class to hold the results of calls to \code{acs.lookup}, typically
#' for possible modification and then passing to calls to \code{acs.fetch}
#' (using the "variable=" argument).
#'
#'
#' @name acs.lookup-class
#' @aliases acs.lookup-class is.acs.lookup results
#' +,acs.lookup,acs.lookup-method c,acs.lookup-method
#' endyear,acs.lookup-method show,acs.lookup-method [,acs.lookup-method
#' results,acs.lookup-method span,acs.lookup-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the
#' form \code{new("acs.lookup", ...)}, but more typically will be created as
#' output from calls to \code{acs.lookup}.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso \code{\link{acs.lookup}}
#' @keywords classes
#' @examples
#'
#' showClass("acs.lookup")
#'
NULL

setClass(Class = "acs.lookup", representation = representation(endyear = "numeric",
    span = "numeric", args = "list", results = "data.frame"))

setMethod("endyear", "acs.lookup", function(object) object@endyear)
setMethod("span", "acs.lookup", function(object) object@span)
setMethod("results", "acs.lookup", function(object) object@results)
setMethod("show", "acs.lookup", function(object) {
    cat("An object of class \"acs.lookup\"\n")
    cat("endyear=", endyear(object), " ; span=", span(object), "\n\n")
    cat("results:\n")
    print(results(object))
    cat("\n")
})

is.acs.lookup <- function (object) class(object) == "acs.lookup"

setMethod("+", signature(e1 = "acs.lookup", e2 = "acs.lookup"), function(e1, e2) {
    e3 <- rbind(e1@results, e2@results)
    new(Class = "acs.lookup", endyear = e1@endyear, args = list(e1@args, e2@args),
        span = e1@span, results = e3)
})

setMethod("c", signature(x = "acs.lookup"), function(x, y, ..., recursive = FALSE) {
    if (missing(y))
        x else x + c(y, ...)
})

setMethod(f = "[", signature = "acs.lookup", definition = function(x, i, j, ...,
    drop = FALSE) {
    if (missing(i))
        i <- j
    if (missing(j))
        j <- i
    new(Class = "acs.lookup", endyear = x@endyear, args = x@args, span = x@span,
        results = x@results[i, ])
})



#' Search for relevant demographic variables and tables from the US Census.
#'
#' The \code{acs.fetch} function is used to download data from the US Census
#' American Community Survey.  The \code{acs.lookup} function provides a
#' convenience function to use in advance to locate tables and variables that
#' may be of interest.
#'
#' \code{acs.lookup} takes arguments similar to \code{acs.fetch} --- in
#' particular, "table.number", "table.name", and "keyword", as well as
#' "endyear","span", and "dataset" --- and searches for matches in the
#' meta-data of the Census tables.  When multiple search terms are passed to a
#' given argument (e.g., \code{keyword=c("Female", "GED")}), the tool returns
#' matches where ALL of the terms are found; similarly, when more than one
#' lookup argument is used (e.g., \code{table.number="B01001",
#' keyword="Female"}), the tool searches for matches that include all of the
#' terms (i.e., terms are combined with a logical "AND", not a logical "OR").
#'
#' Results from acs.lookup --- which are acs.lookup class objects --- can then
#' be inspected, subsetted (with [square brackets]), and combined (with
#' \code{c} or \code{+}) to create custom acs.lookup objects to store and
#' later pass to \code{acs.fetch}.
#'
#'
#' In many cases, \code{acs.lookup} is called internally by \code{acs.fetch},
#' to determine the variable codes to use for a given table.name or
#' table.number.  Since each lookup involves a search of static XML tables
#' (provided by the census for each endyear/span combination, and included by
#' the acs package in /extdata), searches involving more recent years (e.g.,
#' for version 2.0, endyears > 2014) may fail.  In such situations, users may
#' wish to call \code{acs.fetch} with the "variable=" option, perhaps reusing
#' variables from a saved \code{acs.lookup} search for a previous year.
#'
#' For example, once the 2011-2015 5-year ACS data is available via the API,
#' users can attempt the following to access Table B01003, even before the new
#' version of the package is installed with the correct variable lookup
#' tables: \code{acs.fetch(endyear=2015, span=5,
#' variable=acs.lookup(endyear=2014, span=5, table.number="B01003"))}.
#'
#' @param endyear an integer indicating the latest year of the data in the
#' survey (e.g., for data from the 2007-2011 5-year ACS data, endyear would be
#' 2011; limited by acceptable values currently provided by the Census API)
#' @param span an integer indicating the span (in years) of the desired ACS
#' data (should be 1, 3, or 5); defaults to 5.  Ignored and reset to 0 if
#' dataset="sf1" or "sf3".
#' @param dataset either "acs" (the default), "sf1", or "sf3", indicating
#' whether to look for tables and variables in the American Community Survey,
#' the SF1 dataset (decennial/"short-form"), or the SF3 dataset
#' (decennial/"long-form").
#' @param keyword a string or vector of strings giving the search term(s) to
#' find in the name of the ACS census variable (for example, "Male" or
#' "Haiti"); accepts multiple words, which must all be found in the returned
#' variable names.
#' @param table.name a string giving the search term(s) to find in the name of
#' the ACS census table (for example, "Sex" or "Age" or "Age by Sex"); accepts
#' multiple words, which must all be found in the returned table names.
#' @param table.number a string (not a number) indicating the desired table
#' from the Census to fetch; examples: "B01003" or "B23013"; always
#' case-sensitive. Used to identify all variables for a given table number.
#' @param case.sensitive a logical flag indicating whether searching is
#' case-sensitive (the default) or not.  Note that the Census is not entirely
#' consistent in capitalization in table and variable names, so setting
#' \code{case.sensitive=FALSE} may be useful in finding all possible matches.
#' @param ... additional arguments, ignored
#' @return Returns an acs.lookup class object with the results of the query.
#' acs.lookup objects can be subsetted and combined, and passed to the
#' "variable" argument of \code{acs.fetch}.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso \code{\link{acs.lookup-class}}
#' @examples
#'
#' acs.lookup(endyear=2014, span=5, table.number="B01001")
#' acs.lookup(endyear=2012, span=1, table.number="B01001", keyword="Female")
#' acs.lookup(endyear=2012, span=1, keyword=c("Female", "GED"))
#' acs.lookup(endyear=2000, dataset="sf3", table.number="P56")
#' acs.lookup(endyear=1990, dataset="sf3", table.number="H058")
#' age.by.sex=acs.lookup(endyear=2014, span=5, table.name="Age by Sex")
#' age.by.sex
#' workers.age.by.sex=age.by.sex[4:6]
#' workers.age.by.sex
#'
acs.lookup <- function (endyear, span = 5, dataset = "acs", keyword,
                        table.name, table.number, case.sensitive = TRUE, ...) {

    arglist <- as.list(environment())
    if (!missing(table.number)) {
        if (!missing(table.name))
            warning("Cannot specify both table.name and table.number; using table.number")
        # in future?: consider changing next line to table.name='', and let table.number
        # drive the train
        if (endyear != 1990) {
            table.name <- paste(table.number, ".", sep = "")
        } else {
            table.name <- table.number
        }
    }
    if (missing(table.name) && missing(keyword)) {
        warning("No search terms provided; returning NA")
        return(NA)
    }
    if (!missing(keyword) && sum(unlist(lapply(X = keyword, FUN = grepl, "Margin Of Error For",
        ignore.case = TRUE))) > 0) {
        warning("'keyword' marching string 'Margin Of Error For' not permitted\n  Returning NA")
        return(NA)
    }
    if (!case.sensitive) {
        if (!missing(table.name))
            table.name <- tolower(table.name)
        if (!missing(keyword))
            keyword <- tolower(keyword)
    } else {
        insensitive <- FALSE
    }
    # find correct XML variables new way / updated for v2.0 doc.string is xml file
    # name when saved locally or on eglenn archive doc.url is path to census file for
    # XML variables
    if (dataset == "acs") {
        doc.string <- paste(dataset, "_", span, "yr_", endyear, "_var.xml.gz", sep = "")
        doc.url <- paste("http://api.census.gov/data/", endyear, "/acs", span, "/variables.xml",
            sep = "")
    }
    if (dataset == "sf1" | dataset == "sf3") {
        doc.string <- paste(dataset, "_", endyear, ".xml.gz", sep = "")
        doc.url <- paste("http://api.census.gov/data/", endyear, "/", dataset, "/variables.xml",
            sep = "")
        span <- 0
    }
    # first look for XML table internally
    if (file.exists(system.file(paste("extdata/", doc.string, sep = ""), package = "acs"))) {
        doc <- xmlInternalTreeParse(system.file(paste("extdata/", doc.string, sep = ""),
            package = "acs"))
    } else if (url.exists(doc.url)) {
        # next check online at census site

        doc <- xmlInternalTreeParse(doc.url)
        # finally, check personal eglenn archive

    } else if (url.exists(paste("http://web.mit.edu/eglenn/www/acs/acs-variables/",
        doc.string, sep = ""))) {
        # since only here is issues, give some advice
        warning(paste("XML variable lookup tables for this request\n  seem to be missing from '",
            doc.url, "';\n  temporarily downloading and using archived copies instead;\n  since this is *much* slower, recommend running\n  acs.tables.install()"),
            sep = "")
        doc.download <- tempfile()
        download.file(url = paste("http://web.mit.edu/eglenn/www/acs/acs-variables/",
            doc.string, sep = ""), destfile = doc.download)
        doc <- xmlInternalTreeParse(doc.download)
        unlink(doc.download)
    } else {
        # if found nowhere, issue warning and return NA

        warning("As of the date of this version of the acs package\n  no variable lookup tables were available\n  for this dataset/endyear/span combination;\n  perhaps try a different combination...?\n  Returning NA;")
        return(NA)
    }

    if (!missing(keyword)) {
        if (!case.sensitive) {
            str.a <- "contains(translate(@label, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'),'"
        } else {
            str.a <- "contains(@label, '"
        }
        str.b <- paste(str.a, keyword, "')", sep = "")
        str.c <- paste(str.b, collapse = " and ")
        keyword <- str.c
    } else {
        keyword <- ""
    }
    # add in stanza using table number
    if (!missing(table.name)) {
        if (!case.sensitive) {
            str.a <- "contains(translate(@concept, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'),'"
        } else {
            str.a <- "contains(@concept, '"
        }
        str.b <- paste(str.a, table.name, "')", sep = "")
        str.c <- paste(str.b, collapse = " and ")
        table.name <- str.c
        if (endyear == 1990) {
            table.name <- gsub(table.name, pattern = "@concept", replacement = "@xml:id")
        }
    } else {
        table.name <- ""
    }

    if (identical(table.name, "")) {
        STRING <- paste0("//ns:var[", keyword, "]")
    } else if (identical(keyword, "")) {
        STRING <- paste0("//ns:var[", table.name, "]")
    } else {
        STRING <- paste0("//ns:var[", paste(table.name, keyword, sep = " and "),
            "]")
    }
    # get variable codegs ('id')
    names <- suppressWarnings(xpathSApply(doc, STRING, namespaces = "ns", xmlGetAttr,
        "xml:id"))
    names <- gsub("!!!!", " ", names)
    names <- gsub("!!", " ", names)
    my.index <- order(names)  # added for 2012, since data not sorted
    names <- names[my.index]  # added for 2012, since data not sorted
    names <- gsub("E$", "", names)  # remove 'E' from variable name
    if (dataset == "acs" && length(names) > 0)
        {
            names <- names[seq(1, length(names), 2)]
        }  # only want every other

    # get table names
    table.names <- suppressWarnings(xpathSApply(doc, STRING, namespaces = "ns", xmlGetAttr,
        "concept"))
    table.names <- gsub("!!!!", " ", table.names)
    table.names <- gsub("!!", " ", table.names)
    table.numbers <- regmatches(table.names, m = regexpr(table.names, pattern = "^.*\\."))  # find table numbers
    if (endyear == 1990) {
        table.numbers <- substr(names, 1, 5)
        table.numbers <- gsub(x = table.numbers, pattern = "0$", replacement = "")
        table.numbers <- gsub(x = table.numbers, pattern = "$", replacement = ".")
    }
    table.names <- gsub(x = table.names, pattern = "^.*\\.  ", replacement = "")  # remove table numbers from names
    table.names <- gsub(x = table.names, pattern = "* \\[.*\\]$", replacement = "")  # remove bracketed variable counts from SF1/SF3 tables
    table.names <- table.names[my.index]  # added for 2012, since data not sorted
    if (dataset == "acs" && length(table.names) > 0)
        {
            table.names <- table.names[seq(1, length(table.names), 2)]
        }  # only want every other

    # get table numbers
    table.numbers <- substr(table.numbers, 1, unlist(lapply(table.numbers, nchar)) -
        1)  # remove trailing period
    if (dataset == "acs")
        {
            # sf1/sf3 table.numbers have already been reordered!
            table.numbers <- table.numbers[my.index]
        }  # added for 2012, since data not sorted
    if (dataset == "acs" && length(table.numbers) > 0)
        {
            table.numbers <- table.numbers[seq(1, length(table.numbers), 2)]
        }  # only want every other

    # get variable names
    values <- suppressWarnings(xpathSApply(doc, STRING, namespaces = "ns", xmlGetAttr,
        "label"))
    values <- gsub("!!!!", " ", values)
    values <- gsub("!!", " ", values)
    values <- gsub(x = values, pattern = "*\\[.*\\]", replacement = ":")  # remove bracketed variable counts from SF1/SF3 tables
    values <- values[my.index]  # added for 2012, since data not sorted
    if (dataset == "acs" && length(values) > 0)
        {
            values <- values[seq(1, length(values), 2)]
        }  # only want every other

    if (length(names) == 0) {
        warning("Sorry, no tables/keyword meets your search.\n  Suggestions:\n    try with 'case.sensitive=F',\n    remove search terms,\n    change 'keyword' to 'table.name' in search (or vice-versa)")
        return(NA)
    }
    free(doc)
    rm(doc)
    gc()
    new(Class = "acs.lookup", endyear = endyear, span = span, args = arglist, results = data.frame(variable.code = names,
        table.number = table.numbers, table.name = table.names, variable.name = values,
        stringsAsFactors = FALSE))
}
