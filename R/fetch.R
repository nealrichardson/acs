acs.fetch <- function (endyear, span = 5, geography, table.name, table.number,
    variable, keyword, dataset = "acs", key, col.names = "auto", ...) {
        
    var.max <- 40  # most var for acs call; keep even;
    # check some basic stuff about arguments
    if (missing(key)) {
        if (file_test("-f", system.file("extdata/key.rda", package = "acs"))) {
            load(system.file("extdata/key.rda", package = "acs"))
        } else if (!is.null(getOption("census.api.key"))) {
            key <- getOption("census.api.key")
        } else {
            warning("'key' required to access Census API site for download;\n  See http://www.census.gov/developers/ to request a key\n  and/or use 'key=' (or run 'api.key.install()') to avoid this error.")
            return(NA)
        }
    }
    if (missing(endyear)) {
        warning("No endyear provided\n  As of version 2.0, endyear must be explicit.\n  Returning NA.")
        return(NA)
    }
    endyear <- as.integer(endyear)
    span <- as.integer(span)
    if (span < 0 | span > 5) {
        warning("span must be 1, 3, or 5 (or 0, for decennial census)")
    }
    if (dataset == "sf1" | dataset == "sf3") {
        span <- as.integer(0)
    }
    if (missing(keyword) && missing(table.name) && missing(table.number) && missing(variable)) {
        warning("No search terms provided; returning NA")
        return(NA)
    }
    if (!missing(variable)) {
        if (!missing(keyword) || !missing(table.name) || !missing(table.number)) {
            warning("Cannot specify both 'variable' and 'keyword/table.name/table.number'.\n  Using 'variable' and ignoring others.")
        }
    }
    # when variable is NOT provided
    if (missing(variable)) {
        arglist <- as.list(environment())
        missing.args <- unlist(lapply(arglist, is.symbol))
        arglist <- arglist[!missing.args]
        arglist <- arglist[names(arglist) != "variable"]
        arglist <- arglist[names(arglist) != "geography"]
        arglist <- arglist[names(arglist) != "key"]
        arglist <- arglist[names(arglist) != "col.names"]
        arglist <- arglist[names(arglist) != "var.max"]
        variable <- do.call(acs.lookup, arglist)
        if (!isS4(variable) && is.na(variable)) {
            return(NA)
        }
    }
    # when variable is provided
    if (is.acs.lookup(variable)) {
        variables.xml <- results(variable)
        variables <- variables.xml$variable.code
        # next 'if' added to allow for sf1/sf3 fetching
        if (dataset == "acs") {
            variables <- paste(rep(variables, each = 2), c("E", "M"), sep = "")
        }
    }
    if (!is.acs.lookup(variable)) {
        if (variable[1] == "recur") {
            variables <- variable[2:length(variable)]
        } else {
            if (dataset == "acs") {
                variables <- paste(rep(variable, each = 2), c("E", "M"), sep = "")
            }
            if (dataset == "sf1" | dataset == "sf3") {
                variables <- variable
            }
        }
        if (variable[1] == "") {
            variables.xml <- acs.lookup(keyword = keyword, table.name = table.name,
                endyear = endyear, dataset = dataset, ...)
            if (!identical(NA, variables.xml)) {
                variables.xml <- results(variables.xml)
                variables <- variables.xml$variable.code
            } else {
                warning("No results found;\n  perhaps try acs.lookup()...?")
                return(NA)
            }
        }
    }
    if (length(variables) == 1 && is.na(variable))
        return(NA)
    # pretty option to pull descriptive names from XML lookup results; only take
    # every other, since these are the headers for estimates, not MOEs
    if (identical(col.names, "pretty")) {
        if (!is.acs.lookup(variable)) {
            warning("\"pretty\" col.names not available when variable codes provided.\n  Using standard variable code names for columns.")
            col.names <- "auto"
        } else {
            col.names <- paste(variables.xml$table.name, variables.xml$variable.name,
                sep = ": ")
        }
    }
    if (identical(col.names, "auto") & dataset == "acs")
        col.names <- rep("auto", (length(variables)/2))
    if (identical(col.names, "auto") & (dataset == "sf1" | dataset == "sf3"))
        col.names <- rep("auto", length(variables))
    # deal with too many variables -- API currently limits to < 50 note two versions:
    # acs datasets have doubled variables
    if (length(variables) > var.max & dataset == "acs") {
        acs.obj <- cbind(acs.fetch(endyear = endyear, span = span, geography = geography,
            variable = c("recur", variables[1:(var.max - 2)]), key = key, dataset = dataset,
            col.names = col.names[1:((var.max - 2)/2)]), acs.fetch(endyear = endyear,
            span = span, geography = geography, variable = c("recur", variables[(var.max -
                1):length(variables)]), col.names = col.names[(1 + ((var.max - 2)/2)):length(col.names)],
            key = key, dataset = dataset))
        return(acs.obj)
    }
    # note two versions:sf1/sf3 datasets have single variables, since no E and M
    # types
    if (length(variables) > var.max & (dataset == "sf1" | dataset == "sf3")) {
        acs.obj <- cbind(acs.fetch(endyear = endyear, span = span, geography = geography,
            variable = c("recur", variables[1:(var.max - 2)]), key = key, dataset = dataset,
            col.names = col.names[1:(var.max - 2)]), acs.fetch(endyear = endyear,
            span = span, geography = geography, variable = c("recur", variables[(var.max -
                1):length(variables)]), col.names = col.names[(var.max - 1):length(col.names)],
            key = key, dataset = dataset))
        return(acs.obj)
    }

    # next deal with complex geographies
    if (is.geo.set(geography) && length(geography) > 1) {
        acs.obj <- rbind(acs.fetch(endyear = endyear, span = span, geography = geography[1],
            variable = c("recur", variables), key = key, dataset = dataset, col.names = col.names),
            acs.fetch(endyear = endyear, span = span, geography = geography[2:length(geography)],
                variable = c("recur", variables), dataset = dataset, key = key, col.names = col.names))
        if (combine(geography)) {
            acs.obj <- apply(acs.obj, FUN = sum, MARGIN = 1, agg.term = combine.term(geography),
                ...)
            acs.obj@acs.units <- .acs.identify.units(acs.colnames(acs.obj))
        }
        return(acs.obj)
    }
    if (is.geo.set(geography) && length(geography) == 1) {
        acs.obj <- acs.fetch(endyear = endyear, span = span, geography = geography[[1]],
            variable = c("recur", variables), key = key, dataset = dataset, col.names = col.names)
        if (combine(geography)) {
            acs.obj <- apply(acs.obj, FUN = sum, MARGIN = 1, agg.term = combine.term(geography),
                ...)
            acs.obj@acs.units <- .acs.identify.units(acs.colnames(acs.obj))
        }
        return(acs.obj)
    }
    # after this point, geography should be always be a single geo
    api.url <- api.url.maker(endyear = endyear, span = span, key = key, variables = variables,
        dataset = dataset, geo.call = geography)
    geo.length <- length(api.in(geography)) + 2
    # adding check to stop bad url / maybe do this later
    url.test <- url.exists(api.url, .header = TRUE)
    if (url.test["statusMessage"] != "OK") {
        warning(call. = FALSE, paste("No data found at:\n  ", api.url, sep = ""))
    }
    in.data <- suppressWarnings(read.csv(api.url, na.strings = c("-", "**", "***",
        "(X)", "N", "null"), stringsAsFactors = FALSE))
    in.data <- in.data[, -length(in.data)]  # remove junk NA columns
    # set geocols
    geocols <- (length(in.data) - geo.length + 1):length(in.data)
    if (identical(col.names[1], "auto")) {
        # check this!
        if (dataset == "acs") {
            col.names <- names(in.data)[seq(1, (length(in.data) - geo.length), 2)]
        } else if (dataset == "sf1" | dataset == "sf3") {
            col.names <- names(in.data)[1:(length(in.data) - geo.length)]
        }
        col.names[1] <- gsub("X..", "", col.names[1])
        col.names <- gsub(pattern = "E$", x = col.names, replacement = "")

    }
    datacols <- 1:(length(in.data) - geo.length)
    in.data[in.data == "*****"] <- 0
    in.data[[1]] <- gsub("[", "", in.data[[1]], fixed = TRUE)
    in.data[[length(in.data)]] <- gsub("]", "", in.data[[length(in.data)]], fixed = TRUE)
    # clean brackets
    for (i in 1:length(datacols)) {
        in.data[[i]] <- gsub(",", "", in.data[[i]])
        in.data[[i]] <- as.numeric(in.data[[i]])
    }
    GEOGRAPHY <- as.data.frame(in.data[, geocols])
    names(GEOGRAPHY) <- gsub(".", "", names(GEOGRAPHY), fixed = TRUE)  # remove strange trailing period
    if (dataset == "acs") {
        acs.obj <- new(Class = "acs", endyear = endyear, span = span, geography = GEOGRAPHY,
            acs.colnames = col.names, acs.units = .acs.identify.units(col.names),
            currency.year = endyear, standard.error = as.matrix(in.data[, seq(2,
                (length(in.data) - geo.length), 2)]), modified = FALSE, estimate = as.matrix(in.data[,
                seq(1, (length(in.data) - geo.length), 2)]))
    }
    if (dataset == "sf1" | dataset == "sf3") {
        acs.obj <- new(Class = "acs", endyear = endyear, span = span, geography = GEOGRAPHY,
            acs.colnames = col.names, acs.units = .acs.identify.units(col.names),
            currency.year = endyear, standard.error = as.matrix(0 * (in.data[1:(length(in.data) -
                geo.length)])), modified = FALSE, estimate = as.matrix(in.data[1:(length(in.data) -
                geo.length)]))
    }
    # convert 90% MOE into standard error, correct for 2005 flaw
    if (endyear(acs.obj) <= 2005)
        acs.obj@standard.error <- acs.obj@standard.error/1.65 else acs.obj@standard.error <- acs.obj@standard.error/1.645
    acs.obj <- .acs.dimnames(acs.obj)
    # if (obj.combine) {acs.obj=apply(acs.obj, FUN=sum, MARGIN=1)}
    acs.obj
}
