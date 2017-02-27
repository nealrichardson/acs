setGeneric("endyear", function (object) standardGeneric("endyear"))
setGeneric("span", function (object) standardGeneric("span"))
setGeneric("geography", function (object) standardGeneric("geography"))
setGeneric("acs.colnames", function (object) standardGeneric("acs.colnames"))
setGeneric("acs.units", function (object) standardGeneric("acs.units"))
setGeneric("currency.year", function (object) standardGeneric("currency.year"))
setGeneric("modified", function (object) standardGeneric("modified"))
setGeneric("results", function (object) standardGeneric("results"))
setGeneric("estimate", function (object) standardGeneric("estimate"))
setGeneric("standard.error", function (object) standardGeneric("standard.error"))

setGeneric("geography<-", function(object, value) standardGeneric("geography<-"))
setGeneric("endyear<-", function (object, value) standardGeneric("endyear<-"))
setGeneric("span<-", function (x, value) standardGeneric("span<-"))
setGeneric("currency.year<-",
    function(object, value) standardGeneric("currency.year<-"))
setGeneric("acs.units<-", function (x, value) standardGeneric("acs.units<-"))
setGeneric("acs.colnames<-",
    function (x, value) standardGeneric("acs.colnames<-"))

setGeneric("apply")
