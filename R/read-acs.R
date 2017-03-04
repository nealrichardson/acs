#' Reads a comma-delimited file from the American Community Survey and creates
#' an acs object with estimates, standard errors, and associated metadata.
#' 
#' When passed a comma-delimited file from the U.S. Census American Community
#' Survey (typically downloaded via the FactFinder website and unzipped),
#' read.acs returns an acs object with estimates, standard errors, and
#' associated metadata.
#' 
#' Most users will prefer to start with \code{\link{acs.fetch}} to import
#' data; \code{read.acs} is maintained as a "legacy" function, primarily for
#' use in situations where data is not available via the Census API.
#' 
#' 
#' After executing a query on the U.S. Census American FactFinder site
#' (\url{http://factfinder2.census.gov}), users can download their results as
#' a zip file containing data in comma-delimited file format (for example,
#' "ACS_10_5YR_B19013_with_ann.csv").  \code{read.acs} simplifies the creation
#' of new acs objects from these files.  The function uses some rudimentary
#' algorithms to guess intelligently about values for metadata (such as
#' \code{endyear} and \code{geography}), based on current file-format used by
#' the Census "AmericanFactFinder 2" download site.
#' 
#' The specified \code{filename} can be an actual \code{.csv} file, or can be
#' the name of a \code{.zip} file downloaded from the FactFinder site.  If the
#' latter, \code{read.acs} will extract the necessary data and leave the
#' compressed zipfile in place.
#' 
#' As a default, \code{read.acs} assumes the first three columns will contain
#' geographic header information, which seems to be the standard for the new
#' Census American Factfinder download site. Users can also set different
#' values for the \code{geocols=} to specify other columns for this geographic
#' information.  The function will use the first of these columns for
#' geographic rownames to label estimates.  (By default, then, this would be
#' the third column of the actual file, since \code{geocols=3:1}.  For files
#' downloaded via the Census "legacy" version of FactFinder prior to 2012,
#' users will probably want to specify \code{geocols=4:1}.
#' 
#' As for column names, by default \code{read.acs} will scan the file to
#' determine how many of the initial rows contain "header" information, and
#' will generate new \code{acs.colnames} by concatenating information found in
#' these rows.  Note that this can result in \emph{very long} variable names,
#' and users may want to modify the contents of \code{acs.colnames} after
#' creation.
#' 
#' Alternatively, users can inspect downloaded csv files prior to import and
#' specify the \code{skip=} option explicitly, as with \code{read.csv} and
#' other \code{read.XXX} functions (i.e., the value of skip is equal to the
#' number of rows prior to the last header row).  Regardless of whether
#' \code{skip=} is set or "auto", however, the column names will be created
#' using all of the rows at the top of the file, \emph{even the "skipped"
#' ones}.
#' 
#' Finally, these new \code{acs.colnames} are used to guess intelligently
#' about values for \code{acs.units}, but currently all this includes is a
#' check for the word "dollars" in the names; if this is not found, the
#' columns are assumed to be "counts".
#' 
#' When no other values are provided, \code{read.acs} will attempt to
#' determine \code{endyear} and \code{span} from the filename.
#' 
#' @param filename the name of the \code{.csv}, \code{.zip}, or \code{.txt}
#' file to be input
#' @param endyear an integer (or "auto") indicating the latest year of the
#' data in the survey (e.g., for data from the 2005-2009 5-year ACS data,
#' endyear would be 2009)
#' @param span an integer (should be 1, 3, or 5), or "auto" to have read.acs
#' guess the span from the filename (e.g., for data from the 2005-2009 5-year
#' ACS data, span would be 5)
#' @param col.names a vector of column names to be used as \code{acs.colnames}
#' for the object; defaults to "auto", which will result in auto-generated
#' names from the headers lines of the input file
#' @param acs.units a vector of factors indicating what sort of data is
#' contained within each column of data ("count","dollars","proportion",
#' "ratio", "other")
#' @param geocols a vector of integers indicating which columns contain the
#' geographic header information; defaults to "auto", which is the same as
#' 3:1, which seems to be the standard for FactFinder-2 downloads
#' @param skip an integer indicating how many rows to skip before processing
#' the csv file; defaults to "auto", which will try to guess the proper value
#' @return Returns a new acs-class object with estimates, standard errors
#' (derived from the census 90\% margins of error), and metadata associated
#' with the survey,
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
read.acs <- function(filename, endyear = "auto", span = "auto", col.names = "auto",
    acs.units = "auto", geocols = "auto", skip = "auto") {
    # Attempt to automatically determine endyear from filename if necessary.

    if (endyear == "auto") {
        # try to guess end year from filename
        endyear <- 2000 + as.integer(str_extract(str_extract(filename, "ACS_[0-9][0-9]_"),
            "[0-9][0-9]"))
        if (is.na(endyear))
            endyear <- as.integer(str_extract(filename, "200[0-9]"))
        if (is.na(endyear) | endyear < 2000 | endyear > 2012) {
            warning("Can't determine endyear from filename;\nplease set manually before proceeding.\nSetting endyear to default of 2010.\nOperations with this acs object may not be reliable...")
            endyear <- 2010
        } else {
            warning("Guessing endyear of ", endyear, " based on filename...", call. = FALSE)
        }
    }
    # Attempt to automatically determine span from filename if necessary.

    if (span == "auto") {
        span <- as.integer(str_extract(str_extract(filename, "_[0-9]YR"), "[0-9]"))
        if (is.na(span))
            span <- as.integer(substr(str_extract(filename, "[0-9]yr"), 1, 1))
        if (is.na(span) | span > 5) {
            warning("Can't determine span from filename;\nplease set manually before proceeding.\nSetting span to default of 1.\nOperations with this acs object may not be reliable...")
            span <- 1
        } else {
            warning("Guessing span of ", span, " based on filename...", call. = FALSE)
        }
    }
    span <- as.integer(span)
    endyear <- as.integer(endyear)
    if (span > 5 | span < 1) {
        warning("Span out of range; returning NA")
        return(NA)
    }
    # set geocols
    if (identical(geocols, "auto")) {
        geocols <- 3:1
        warning("Using first three columns as geographic headers.", call. = FALSE)
    }

    if (identical(str_sub(filename, start = -4), ".zip")) {
        zip.filename <- filename
        contents <- unzip(zip.filename, list = TRUE)
        acs.filename <- grep(pattern = "with_ann.csv", x = contents[[1]], value = TRUE)
        if (length(acs.filename) == 0)
            acs.filename <- grep(pattern = "[0-9].csv", x = contents[[1]], value = TRUE)
        make.con <- function() {
            unz(description = zip.filename, filename = acs.filename)
        }
    } else {
        make.con <- function() {
            file(description = filename)
        }
    }
    # figure out how many rows to skip -- only call if skip='auto'
    figure.skip <- function() {
        con <- make.con()
        open(con)
        i <- 0
        while (str_sub(scan(con, what = "", nlines = 1, quiet = TRUE), end = 1)[1] ==
            ",") {
            i <- i + 1
        }
        close(con)
        i + 1  # need to add one more to skip extra column header; most ACS files have two rows for headers
    }
    if (identical(skip, "auto")) {
        skip <- figure.skip()
    }

    # figure out column names based on headers
    get.colnames <- function(geocols, skip) {
        con <- make.con()
        open(con)
        headers <- read.csv(con, nrows = skip + 1, header = FALSE)  # add one to include 'header' row
        headers <- headers[, -geocols]
        headers <- apply(headers, FUN = "paste", MARGIN = 2, collapse = ".")
        headers <- headers[seq(1, length(headers), 2)]
        close(con)
        headers
    }
    if (identical(col.names, "auto")) {
        col.names <- get.colnames(geocols, skip)
    }
    # helper function to read and clean data
    get.acs <- function(geocols, skip) {
        con <- make.con()
        open(con)
        in.data <- read.csv(con, skip = skip, na.strings = c("-", "**", "***", "(X)",
            "N"), stringsAsFactors = FALSE)
        # trick to get geocols to be first columns
        datacols <- 1:length(in.data)
        datacols <- datacols[-geocols]
        in.data <- in.data[, c(geocols, datacols)]
        in.data[in.data == "*****"] <- 0
        for (i in (length(geocols) + 1):length(in.data)) {
            in.data[[i]] <- gsub(",", "", in.data[[i]])
            in.data[[i]] <- as.numeric(in.data[[i]])
        }
        colnames(in.data)[-geocols] <- col.names
        close(con)
        in.data
    }

    in.data <- get.acs(geocols = geocols, skip = skip)
    acs.colnames <- unname(col.names)
    # create new acs object
    if (acs.units == "auto") {
        # try to guess variable types from filename
        acs.units <- .acs.identify.units(acs.colnames)
    }
    acs.units <- factor(acs.units, levels = .acs.unit.levels)
    acs.obj <- new(Class = "acs", endyear = endyear, span = span, geography = as.data.frame(in.data[,
        1:length(geocols)]), acs.colnames = acs.colnames, acs.units = acs.units,
        currency.year = endyear, standard.error = as.matrix(in.data[, seq((length(geocols) +
            2), length(in.data), 2)]), modified = FALSE, estimate = as.matrix(in.data[,
            seq((length(geocols) + 1), length(in.data), 2)]))

    # convert 90% MOE into standard error, correct for 2005 flaw
    if (endyear(acs.obj) <= 2005)
        acs.obj@standard.error <- acs.obj@standard.error/1.65 else acs.obj@standard.error <- acs.obj@standard.error/1.645
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
}
