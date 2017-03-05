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
        stop("No search terms provided", call.=FALSE)
    }
    if (!missing(keyword) && sum(unlist(lapply(X = keyword, FUN = grepl, "Margin Of Error For",
        ignore.case = TRUE))) > 0) {
        stop("'keyword' marching string 'Margin Of Error For' not permitted",
            call.=FALSE)
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
        doc.string <- paste0(dataset, "_", span, "yr_", endyear, "_var.xml.gz")
        doc.url <- paste0("http://api.census.gov/data/", endyear, "/acs", span, "/variables.xml")
    }
    if (dataset == "sf1" | dataset == "sf3") {
        doc.string <- paste0(dataset, "_", endyear, ".xml.gz")
        doc.url <- paste0("http://api.census.gov/data/", endyear, "/", dataset, "/variables.xml")
        span <- 0
    }

    # first look for XML table internally
    local.table <- system.file(paste("extdata/", doc.string, sep = ""),
        package = "acs")
    if (file.exists(local.table)) {
        doc <- xmlInternalTreeParse(local.table)
    } else {
        doc <- try(xmlInternalTreeParse(doc.url), silent=TRUE)
        if (inherits(doc, "try-error")) {
            # finally, check personal eglenn archive
            warning(paste("XML variable lookup tables for this request\n  seem to be missing from '",
                doc.url, "';\n  temporarily downloading and using archived copies instead;\n  since this is *much* slower, recommend running\n  acs.tables.install()"),
                sep = "")
            eglenn.url <- paste0("http://web.mit.edu/eglenn/www/acs/acs-variables/",
                doc.string)
            tryCatch(doc <- xmlInternalTreeParse(eglenn.url),
                error=function (e) {
                    stop("As of the date of this version of the acs package\n  no variable lookup tables were available\n  for this dataset/endyear/span combination;\n  perhaps try a different combination...?", call.=FALSE)
                })
        }
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
    if (dataset == "acs") {
        # sf1/sf3 table.numbers have already been reordered!
        table.numbers <- table.numbers[my.index]
    }  # added for 2012, since data not sorted
    if (dataset == "acs" && length(table.numbers) > 0) {
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
