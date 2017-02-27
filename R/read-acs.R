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
            warning("Guessing endyear of ", endyear, " based on filename...", call. = F)
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
            warning("Guessing span of ", span, " based on filename...", call. = F)
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
        warning("Using first three columns as geographic headers.", call. = F)
    }

    if (identical(str_sub(filename, start = -4), ".zip")) {
        zip.filename <- filename
        contents <- unzip(zip.filename, list = T)
        acs.filename <- grep(pattern = "with_ann.csv", x = contents[[1]], value = T)
        if (length(acs.filename) == 0)
            acs.filename <- grep(pattern = "[0-9].csv", x = contents[[1]], value = T)
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
        while (str_sub(scan(con, what = "", nlines = 1, quiet = T), end = 1)[1] ==
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
        headers <- read.csv(con, nrows = skip + 1, header = F)  # add one to include 'header' row
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
            "N"), stringsAsFactors = F)
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
            2), length(in.data), 2)]), modified = F, estimate = as.matrix(in.data[,
            seq((length(geocols) + 1), length(in.data), 2)]))

    # convert 90% MOE into standard error, correct for 2005 flaw
    if (endyear(acs.obj) <= 2005)
        acs.obj@standard.error <- acs.obj@standard.error/1.65 else acs.obj@standard.error <- acs.obj@standard.error/1.645
    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
}
