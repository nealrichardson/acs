acs.fetch <- function (endyear, span=5, geography, table.name, table.number,
    variable, keyword, dataset=c("acs", "sf1", "sf3"), key=api.key.load(),
    col.names="auto", ...) {

    dataset <- match.arg(dataset)
    census <- dataset != "acs"

    var.max <- 40  # most var for acs call; keep even;
    # check some basic stuff about arguments
    if (!is.character(key)) {
        ## Leaving this message here, though you won't hit it normally.
        ## TODO: move to an error handler on an API request that actually fails
        ## because there was no key supplied.
        ## TODO: move to a "warn once" per session in api.key.load.
        stop("'key' required to access Census API site for download;\n  See http://www.census.gov/developers/ to request a key\n  and/or use 'key=' (or run 'api.key.install()') to avoid this error.")
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
    if (census) {
        span <- as.integer(0)
    }
    if (missing(keyword) && missing(table.name) && missing(table.number) && missing(variable)) {
        warning("No search terms provided; returning NA")
        return(NA)
    }

    # when variable is NOT provided
    if (missing(variable)) {
        Call <- match.call()
        Call[[1]] <- as.name("acs.lookup")
        variable <- eval(Call)
        if (!isS4(variable) && is.na(variable)) {
            return(NA)
        }
    } else {
        if (!missing(keyword) || !missing(table.name) || !missing(table.number)) {
            warning("Cannot specify both 'variable' and 'keyword/table.name/table.number'.\n  Using 'variable' and ignoring others.")
        }
    }

    # when variable is provided
    if (is.acs.lookup(variable)) {
        variables.xml <- results(variable)
        variables <- variables.xml$variable.code
        # next 'if' added to allow for sf1/sf3 fetching
        if (!census) {
            variables <- paste(rep(variables, each=2), c("E", "M"), sep="")
        }
    } else {
        if (variable[1] == "recur") {
            variables <- variable[2:length(variable)]
        } else {
            if (census) {
                variables <- variable
            } else {
                variables <- paste(rep(variable, each=2), c("E", "M"), sep="")
            }
        }
        if (variable[1] == "") {
            variables.xml <- acs.lookup(keyword=keyword, table.name=table.name,
                endyear=endyear, dataset=dataset, ...)
            if (!identical(NA, variables.xml)) {
                variables.xml <- results(variables.xml)
                variables <- variables.xml$variable.code
            } else {
                warning("No results found;\n  perhaps try acs.lookup()...?")
                return(NA)
            }
        }
    }
    if (length(variables) == 1 && is.na(variable)) {
        return(NA)
    }
    # pretty option to pull descriptive names from XML lookup results; only take
    # every other, since these are the headers for estimates, not MOEs
    if (identical(col.names, "pretty")) {
        if (!is.acs.lookup(variable)) {
            warning("\"pretty\" col.names not available when variable codes provided.\n  Using standard variable code names for columns.")
            col.names <- "auto"
        } else {
            col.names <- paste(variables.xml$table.name, variables.xml$variable.name,
                sep=": ")
        }
    }
    if (identical(col.names, "auto")) {
        col.names <- rep("auto",
            ifelse(census, length(variables), length(variables)/2))
    }

    if (length(variables) > var.max) {
        # deal with too many variables -- API currently limits to < 50
        # note two versions:
        # acs datasets have doubled variables
        # sf1/sf3 datasets have single variables, since no E and M types
        col.break <- ifelse(census, var.max - 2, (var.max - 2)/2)
        return(cbind(
            acs.fetch(endyear=endyear,
                span=span,
                geography=geography,
                variable=c("recur", variables[1:(var.max - 2)]),
                key=key,
                dataset=dataset,
                col.names=col.names[1:col.break]),
            acs.fetch(endyear=endyear,
                span=span,
                geography=geography,
                variable=c("recur", variables[(var.max - 1):length(variables)]),
                col.names=col.names[(1 + col.break):length(col.names)],
                key=key, dataset=dataset)
        ))
    }

    # next deal with complex geographies
    if (is.geo.set(geography) && length(geography) > 1) {
        acs.obj <- rbind(acs.fetch(endyear=endyear, span=span, geography=geography[1],
            variable=c("recur", variables), key=key, dataset=dataset, col.names=col.names),
            acs.fetch(endyear=endyear, span=span, geography=geography[2:length(geography)],
                variable=c("recur", variables), dataset=dataset, key=key, col.names=col.names))
        if (combine(geography)) {
            acs.obj <- apply(acs.obj, FUN=sum, MARGIN=1, agg.term=combine.term(geography),
                ...)
            acs.obj@acs.units <- .acs.identify.units(acs.colnames(acs.obj))
        }
        return(acs.obj)
    }
    if (is.geo.set(geography) && length(geography) == 1) {
        acs.obj <- acs.fetch(endyear=endyear, span=span,
            geography=geography[[1]],
            variable=c("recur", variables), key=key, dataset=dataset, col.names=col.names)
        if (combine(geography)) {
            acs.obj <- apply(acs.obj, FUN=sum, MARGIN=1, agg.term=combine.term(geography),
                ...)
            acs.obj@acs.units <- .acs.identify.units(acs.colnames(acs.obj))
        }
        return(acs.obj)
    }
    # after this point, geography should be always be a single geo

    ## Get the data from the Census API
    api.url <- api.url.maker(endyear=endyear, span=span, key=key,
        variables=variables, dataset=dataset, geo.call=geography)
    # adding check to stop bad url / maybe do this later
    req <- GET(api.url)
    stop_for_status(req)

    in.data <- fromJSON(content(req, "text", encoding="UTF-8"))
    ## Names are in the first row. Pop them off.
    nm <- in.data[1,]
    in.data <- in.data[-1,,drop=FALSE]
    ## Geography columns are at the end. Pop them off too.
    geo.length <- length(api.in(geography)) + 2
    ncols <- ncol(in.data) - geo.length
    datacols <- 1:ncols
    geocols <- (ncols + 1):ncol(in.data)
    GEOGRAPHY <- as.data.frame(in.data[, geocols, drop=FALSE])
    names(GEOGRAPHY) <- gsub(".", "", nm[geocols], fixed=TRUE)  # remove strange trailing period

    in.data <- matrix(as.numeric(in.data[, datacols]), ncol=ncols)
    colnames(in.data) <- nm[datacols]

    if (identical(col.names[1], "auto")) {
        # check this!
        if (census) {
            col.names <- nm[datacols]
        } else {
            col.names <- nm[seq(1, ncols, 2)]
        }
        col.names <- gsub(pattern="E$", x=col.names, replacement="")
    }
    if (census) {
        est <- in.data
        se <- matrix(0, nrow=nrow(est), ncol=ncol(est))
    } else {
        est <- in.data[, seq(1, ncols, 2), drop=FALSE]
        se <- in.data[, seq(2, ncols, 2), drop=FALSE]
        # convert 90% MOE into standard error, correct for 2005 flaw
        se <- se/ifelse(endyear <= 2005, 1.65, 1.645)
    }
    acs.obj <- new(Class="acs",
        endyear=endyear,
        span=span,
        geography=GEOGRAPHY,
        acs.colnames=col.names,
        acs.units=.acs.identify.units(col.names),
        currency.year=endyear,
        standard.error=se,
        modified=FALSE,
        estimate=est
    )

    acs.obj <- .acs.dimnames(acs.obj)
    acs.obj
}
