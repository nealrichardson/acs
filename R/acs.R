
# library(stringr) library(plyr) library(XML)

# fips.state=read.csv('./data/FIPS_state_file.txt', sep='|', stringsAsFactors=F)
# fips.county=read.csv('./data/FIPS_county_file.txt', sep=',',
# stringsAsFactors=F)

## this file needs extra help, due to commas in some of the subdiv names, which
## goofs up the read.csv import
## fips.county.subdivision=read.csv('./data/FIPS_countysubdivision_file.txt',
## sep=',', stringsAsFactors=F) correct some problem with commas in names
## index=nchar(fips.county.subdivision$FUNCSTAT)>1
## fips.county.subdivision$COUSUBNAME[index]=paste(fips.county.subdivision$COUSUBNAME[index],
## fips.county.subdivision$FUNCSTAT[index], sep=':')
## fips.county.subdivision$FUNCSTAT[index]='N'
## fips.county.subdivision=fips.county.subdivision[!is.na(fips.county.subdivision$STATEFP),]

# fips.place=read.csv('./data/FIPS_place_file.txt', sep='|', stringsAsFactors=F)

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
    header <- list(endyear = ENDYEAR, span = SPAN, currency.year = CURRENCY.YEAR,
        geography = GEOGRAPHY, acs.colnames = ACS.COLNAMES, acs.units = ACS.UNITS)
    header
}


# .acs.identify.units() used to set units in acs object; initially assumes all
# units are 'counts', then changes some to 'dollars' is the word 'dollars'
# matches in colnames.

.acs.identify.units <- function(acs.colnames) {
    acs.units <- rep("count", length(acs.colnames))
    dollar.index <- grep(pattern = "dollars", x = acs.colnames, fixed = T)
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

acs.tables.install <- function() {
    filelist <- readLines("http://web.mit.edu/eglenn/www/acs/acs-variables/filelist.txt")
    dir.create(paste(system.file(package = "acs"), "extdata", sep = "/"), showWarnings = F)
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
    modified = F))

is.acs <- function (object) class(object) == "acs"

setMethod("endyear", "acs", function(object) object@endyear)
setMethod("span", "acs", function(object) object@span)
setMethod("geography", "acs", function(object) object@geography)
setMethod("acs.colnames", "acs", function(object) object@acs.colnames)
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
                call. = F)
        }
        if (span(x) != span(value)) {
            warning("original and replacement do not have same span;\nkeeping original value",
                call. = F)
        }
        if (currency.year(x) != currency.year(value)) {
            warning("original and replacement do not have same currency.year;\nkeeping original value",
                call. = F)
        }
        x@estimate[i, j] <- value@estimate
        x@standard.error[i, j] <- value@standard.error
        # check for mismatch geo when not all cols changed if not identical geogs if
        # changing all cols or more
        if (!all(geography(x[i, j]) == geography(value))) {
            if (dim(x)[2] <= length(j)) {
                x@geography[i, ] <- geography(value)  # change all geo
                warning("geographies do not match but all columns changed;\nusing new geographies",
                  call. = F)
            } else {
                warning("geographies do not match but some columns retained;\nkeeping original geography values",
                  call. = F)
            }
        }
        if (!all(acs.colnames(x[i, j]) == acs.colnames(value))) {
            # if not identical colnames if not changing all rows or more
            if (dim(x)[1] <= length(i)) {
                x@acs.colnames[j] <- acs.colnames(value)
                warning("acs.colnames do not match but all rows changes;\nusing new acs.colnames",
                  call. = F)
            } else {
                warning("acs.colnames do not match but some rows retained;\nkeeping original acs.colnames",
                  call. = F)
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
            call. = F)
    }
    x@modified <- T
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
    acs.obj <- new(Class = "acs", endyear = endyear(e1), span = span(e1), modified = T,
        geography = GEOGRAPHY, acs.units = factor(c(acs.units(e1), acs.units(e2)),
            levels = .acs.unit.levels), currency.year = currency.year(e1), acs.colnames = c(acs.colnames(e1),
            acs.colnames(e2)), estimate = NEW.ESTIMATE, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
}

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
    acs.obj <- new(Class = "acs", endyear = endyear(e1), span = span(e1), modified = T,
        geography = GEOGRAPHY, acs.units = acs.units(e1), currency.year = currency.year(e1),
        acs.colnames = acs.colnames(e1), estimate = NEW.ESTIMATE, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
}

setMethod("+", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
    header <- .acs.combine.headers(e1, e2, "+")
    NEW.ESTIMATE <- estimate(e1) + estimate(e2)
    NEW.ERROR <- sqrt(standard.error(e1)^2 + standard.error(e2)^2)
    acs.obj <- new(Class = "acs", endyear = header$endyear, span = header$span, modified = T,
        geography = header$geography, acs.units = header$acs.units, currency.year = header$currency.year,
        acs.colnames = header$acs.colnames, estimate = NEW.ESTIMATE, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
})

setMethod("-", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
    header <- .acs.combine.headers(e1, e2, "-")
    NEW.ESTIMATE <- estimate(e1) - estimate(e2)
    NEW.ERROR <- sqrt(standard.error(e1)^2 + standard.error(e2)^2)
    acs.obj <- new(Class = "acs", endyear = header$endyear, span = header$span, modified = T,
        geography = header$geography, acs.units = header$acs.units, currency.year = header$currency.year,
        acs.colnames = header$acs.colnames, estimate = NEW.ESTIMATE, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
})

.acs.divider <- function(num, den, proportion, verbose = F, output = "result") {
    if (proportion == T)
        header <- .acs.combine.headers(num, den, "/") else header <- .acs.combine.headers(num, den, ":")
    p <- estimate(num)/estimate(den)
    # start with proportion-style
    if (proportion == T) {
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
    acs.obj <- new(Class = "acs", endyear = header$endyear, span = header$span, modified = T,
        geography = header$geography, acs.units = header$acs.units, currency.year = header$currency.year,
        acs.colnames = header$acs.colnames, estimate = p, standard.error = NEW.ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    if (output == "both") {
        list(result = acs.obj, div.method = ratio.report)
    } else if (output == "div.method") {
        ratio.report
    } else acs.obj
}



setMethod("/", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
    # by default, use more conservative 'ratio-style' dividing
    warning("** using the more conservative formula for ratio-type
    dividing, which does not assume that numerator is a subset of
    denominator; for more precise results when seeking a proportion
    and not a ratio, use divide.acs(..., method=\"proportion\") **")
    .acs.divider(num = e1, den = e2, proportion = F, verbose = F)
})

divide.acs <- function(numerator, denominator, method = "ratio", verbose = T, output = "result") {
    if (identical(method, "ratio")) {
        .acs.divider(num = numerator, den = denominator, proportion = F, verbose = verbose,
            output = output)
    } else if (identical(method, "proportion")) {
        .acs.divider(num = numerator, den = denominator, proportion = T, verbose = verbose,
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
    acs.obj <- new(Class = "acs", endyear = header$endyear, span = header$span, modified = T,
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

setMethod("sum", signature(x = "acs"), function(x, agg.term = c("aggregate", "aggregate"),
    one.zero = FALSE, ..., na.rm = FALSE) {
    if (length(agg.term) < 2) {
        agg.term[2] <- agg.term[1]
    }
    est <- estimate(x)
    err <- standard.error(x)
    if (one.zero == T && any(est == 0)) {
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
    acs.obj <- new(Class = "acs", endyear = endyear(x), span = span(x), modified = T,
        geography = geography, acs.units = acs.units, currency.year = currency.year(x),
        acs.colnames = acs.colnames, estimate = ESTIMATE, standard.error = ERROR)
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
})

.apply.acs <- function(X, MARGIN, FUN, ...) {
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
    x@modified <- T
    validObject(x)
    return(x)
})

setReplaceMethod(f = "geography", signature = "acs", definition = function(object,
    value) {
    object@geography <- value
    object <- .acs.dimnames(object)
    object@modified <- T
    validObject(object)
    return(object)
})

setReplaceMethod(f = "endyear", signature = "acs", definition = function(object,
    value) {
    warning(paste("Changing value of endyear from ", endyear(object), " to ", value,
        ".\nThis is an unusual thing to do, unless the original value was incorrect.\nAlso changing value of currency.year to",
        value, ", without converting currency values.\nPlease see ?endyear and ?currency.year for more information",
        sep = ""), call. = F)
    object@endyear <- as.integer(value)
    object@currency.year <- as.integer(value)
    object@modified <- T
    validObject(object)
    return(object)
})

setReplaceMethod(f = "span", signature = "acs", definition = function(x, value) {
    warning(paste("Changing value of span from ", span(x), " to ", value, ".\nThis is an unusual
                         thing to do, unless the original value was
                         incorrect.\nSee ?acs for more information",
        sep = ""), call. = F)
    x@span <- as.integer(value)
    x@modified <- T
    validObject(x)
    return(x)
})

setReplaceMethod(f = "acs.units", signature = "acs", definition = function(x, value) {
    x@acs.units <- factor(value, levels = .acs.unit.levels)
    x@modified <- T
    validObject(x)
    return(x)
})

setReplaceMethod(f = "currency.year", signature = "acs", definition = function(object,
    value) {
    currency.convert(object, rate = "auto", newyear = value)
})

currency.convert <- function(object, rate = "auto", newyear = NA_integer_, verbose = F) {
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
        warning(output, call. = F)
    }
    for (i in dollar.cols) {
        object@estimate[, i] <- object@estimate[, i] * rate
        object@standard.error[, i] <- object@standard.error[, i] * rate
    }
    object@currency.year <- as.integer(newyear)
    object@modified <- T
    validObject(object)
    return(object)
}

# helper function for replacing geography or acs.colnames

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
