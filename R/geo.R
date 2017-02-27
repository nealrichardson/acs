setClass(Class = "geo", representation = representation(api.for = "list", api.in = "list",
    name = "character", sumlev = "numeric"), prototype())

is.geo <- function (object) class(object) == "geo"

if (!isGeneric("api.for")) {
    setGeneric("api.for", def = function(object) {
        standardGeneric("api.for")
    })
} else {
}
setMethod("api.for", "geo", function(object) object@api.for)

if (!isGeneric("api.in")) {
    setGeneric("api.in", def = function(object) {
        standardGeneric("api.in")
    })
} else {
}
setMethod("api.in", "geo", function(object) object@api.in)

if (!isGeneric("name")) {
    setGeneric("name", def = function(object) {
        standardGeneric("name")
    })
} else {
}
setMethod("name", "geo", function(object) object@name)

if (!isGeneric("sumlev")) {
    setGeneric("sumlev", def = function(object) {
        standardGeneric("sumlev")
    })
} else {
}
setMethod("sumlev", "geo", function(object) object@sumlev)

setClass(Class = "geo.set", representation = representation(geo.list = "list", combine = "logical",
    combine.term = "character"), prototype(combine = TRUE, combine.term = "aggregate"))
is.geo.set <- function(object) {
    if (class(object) == "geo.set") {
        TRUE
    } else {
        FALSE
    }
}
if (!isGeneric("combine")) {
    setGeneric("combine", def = function(object) {
        standardGeneric("combine")
    })
} else {
}
setMethod("combine", "geo.set", function(object) object@combine)
if (!isGeneric("combine.term")) {
    setGeneric("combine.term", def = function(object) {
        standardGeneric("combine.term")
    })
} else {
}
setMethod("combine.term", "geo.set", function(object) object@combine.term)
if (!isGeneric("geo.list")) {
    setGeneric("geo.list", def = function(object) {
        standardGeneric("geo.list")
    })
} else {
}

setMethod("geo.list", "geo.set", function(object) {
    if (length(object@geo.list) == 1)
        object@geo.list[[1]] else object@geo.list
})

setMethod("show", signature(object = "geo"), function(object) {
    cat("\"geo\" object: ")
    print(object@name)
})


# method to combine geos and geo.sets
setMethod("+", signature(e1 = "geo", e2 = "geo"), function(e1, e2) {
    geo.set.obj <- new(Class = "geo.set", combine = FALSE, geo.list = c(e1, e2))
    geo.set.obj
})
setMethod("+", signature(e1 = "geo.set", e2 = "geo"), function(e1, e2) {
    geo.set.obj <- new(Class = "geo.set", combine = combine(e1), geo.list = c(geo.list(e1),
        e2))
    geo.set.obj
})
setMethod("+", signature(e1 = "geo", e2 = "geo.set"), function(e1, e2) {
    geo.set.obj <- new(Class = "geo.set", combine = combine(e2), geo.list = c(e1,
        geo.list(e2)))
    geo.set.obj
})

# Note on '+' if geo.list(e1), geo.list(e2) are geo.sets, then take the geolists
# of them, recusively; should always yield flattend set.

setMethod("+", signature(e1 = "geo.set", e2 = "geo.set"), function(e1, e2) {
    if (is.geo(geo.list(e2)))
        combine <- combine(e1) else if (is.geo(geo.list(e1)))
        combine <- combine(e2) else combine <- FALSE
    geo.set.obj <- flatten.geo.set(c(e1, e2))
    combine(geo.set.obj) <- combine
    combine.term(geo.set.obj) <- paste(combine.term(e1), " + ", combine.term(e2))
    geo.set.obj
})

flatten.geo.set <- function(x) {
    if (!is.geo.set(x))
        return(NA)
    if (length(x) == 1) {
        if (is.geo(geo.list(x))) {
            return(x)
        }
        if (is.geo.set(geo.list(x))) {
            return(flatten.geo.set(geo.list(x[1])))
        }
    }
    if (length(x) > 1) {
        a <- flatten.geo.set(x[1])
        b <- flatten.geo.set(x[2:length(x)])
        new(Class = "geo.set", combine = combine(x), combine.term = combine.term(x),
            geo.list = c(geo.list(a), geo.list(b)))
    }
}

setMethod("c", signature(x = "geo.set"), function(x, y, ..., combine = FALSE, combine.term = "aggregate",
    recursive = FALSE) {
    if (recursive) {
        if (missing(y))
            geo.set.obj <- x else geo.set.obj <- x + c(y, ..., recursive = TRUE)
        geo.set.obj@combine <- combine
        geo.set.obj@combine.term <- combine.term
    } else {
        if (missing(y)) {
            geo.set.obj <- x
        } else {
            if (length(y) == 1) {
                geo.set.obj <- c((x + geo.list(y)), ...)
                geo.set.obj@combine <- combine
                geo.set.obj@combine.term <- combine.term
            } else {
                geo.set.obj <- new(Class = "geo.set", combine = combine, combine.term = combine.term,
                  geo.list = list(x, y, ...))
            }
        }
    }
    geo.set.obj
})

# NOTE: changed this to prevent extra nesting when only one geo.set is subsetted

setMethod(f = "[", signature = "geo.set", definition = function(x, i, j, ..., drop = FALSE) {
    if (missing(i))
        i <- j
    if (missing(j))
        j <- i
    if (length(i) == 1 && is.geo.set(x@geo.list[[i]]))
        return(x@geo.list[[i]]) else new(Class = "geo.set", geo.list = x@geo.list[i], combine = combine(x), combine.term = paste(combine.term(x),
        "(partial)", sep = " "))
})

setMethod(f = "[[", signature = "geo.set", definition = function(x, i, j, ..., drop = FALSE) {
    if (missing(i))
        i <- j
    if (missing(j))
        j <- i
    x@geo.list[[i]]
})

# need to work on to allow to change combine values -- seem to not like when you
# replace more than one
setReplaceMethod(f = "[", signature = "geo.set", definition = function(x, i, j, value) {
    if (missing(i))
        i <- j
    if (missing(j))
        j <- i
    if (length(i) == 1) {
        x@geo.list[i] <- value
        validObject(x)
        return(x)
    } else {
        for (a in i) {
            x@geo.list[i] <- value@geo.list[a]
        }
        validObject(x)
        return(x)
    }
})

setReplaceMethod(f = "[[", signature = "geo.set", definition = function(x, i, j,
    value) {
    if (missing(i))
        i <- j
    if (missing(j))
        j <- i
    x@geo.list[i] <- value
    validObject(x)
    return(x)
})


## for some reason, can't declare new generic for 'length' if
## (!isGeneric('length')) { setGeneric('length',
## def=function(x){standardGeneric('length')})}else{}

setMethod("length", "geo.set", function(x) length(x@geo.list))

if (!isGeneric("combine<-")) {
    setGeneric("combine<-", def = function(object, value) {
        standardGeneric("combine<-")
    })
} else {
}

setReplaceMethod(f = "combine", signature = "geo.set", definition = function(object,
    value) {
    object@combine <- value
    validObject(object)
    return(object)
})

if (!isGeneric("combine.term<-")) {
    setGeneric("combine.term<-", def = function(object, value) {
        standardGeneric("combine.term<-")
    })
} else {
}

setReplaceMethod(f = "combine.term", signature = "geo.set", definition = function(object,
    value) {
    object@combine.term <- value
    validObject(object)
    return(object)
})
