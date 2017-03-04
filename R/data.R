#' Consumer Price Index data (1913-2015).
#'
#' Contains data on the Consumer Price Index for All Urban Consumers (CPI-U)
#' for the years 1913-2015 from the U.S. Bureau of Labor Statistics.  Used by
#' the acs package for currency conversion functions.  Scaled for base (100)
#' to be 1982-84.
#'
#'
#' @name cpi
#' @docType data
#' @format A named vector containing 103 observations, one for each year from
#' 1913 through 2015.
#' @seealso \code{\link{currency.year}}
#'
#' \code{\link{currency.convert}}
#' @source \url{http://www.bls.gov/cpi/}
#' @keywords datasets
NULL





#' FIPS codes and geographic names for use in searching and creating
#' geographies in the acs package.
#'
#' FIPS codes and geographic names for use in searching and creating
#' geographies in the acs package.  (Used internally.)
#'
#'
#' @name fips.state
#' @aliases fips.state fips.county fips.county.subdivision fips.place
#' fips.school fips.american.indian.area
#' @docType data
#' @format Each table is a dataframe containing FIPS codes and names from the
#' US Census.
#' @source
#'
#' State: http://www.census.gov/geo/reference/ansi_statetables.html
#'
#' County: http://www.census.gov/geo/www/codes/county/download.html
#'
#' County Subdivision:
#' http://www.census.gov/geo/www/codes/cousub/download.html
#'
#' Place: http://www.census.gov/geo/www/codes/place/download.html
#'
#' School: http://www.census.gov/geo/www/codes/sd/
#'
#' American Indian Area: http://www.census.gov/geo/www/codes/aia/
#' @keywords datasets
NULL





#' Class \code{"geo.set"}
#'
#' The geo.set class provides a convenient wrapper for user-defined
#' geographies, used for downloading data from the U.S. Census American
#' Community Survey.  A geo.set may hold the designation of a single geography
#' (say, a census tract, a county, or a state), or may bundle together
#' multiple geographies of various levels, which may or may not be "combined"
#' when downloaded.  Note that geo.sets may even contain nested geo.sets.
#'
#' Note: even a single geographic unit --- one specific tract or county ---
#' must be wrapped up as a geo.set.  Technically, each individual element in
#' the set is known as a "geo", but users will rarely (if ever) interact will
#' individual elements such as this; wrapping all groups of geographies ---
#' even groups consisting of just one element --- in geo.sets like this will
#' help make them easier to deal with as the geographies get more complex.
#'
#' geo.set objects may be combined with the simple addition operator
#' (\code{+}).  By default, this will always return "flat" geo.sets with all
#' the geographies in a single list.  The combination operator (\code{c}), on
#' the other hand, will generally return nested hierarchies, embedding sets
#' within sets.  When working with nested sets like this, the "combine" flag
#' can be set at each level to aggregate subsets within the structure
#' (although be careful --- if a higher level of set includes "combine=TRUE"
#' you'll never actually see the unaggregated subsets deeper down).
#'
#' Using these different techniques, users are able to create whatever sort of
#' new geographies they need --- aggregating some geographies, keeping others
#' distinct (but still bundled as a set for convenience), mixing and matching
#' different levels of Census geography, and so on.
#'
#'
#' @name geo.set-class
#' @aliases geo-class geo.set-class api.for api.in sumlev is.geo geo.list
#' is.geo.set [,geo.set-method [[,geo.set-method [<-,geo.set-method
#' [[<-,geo.set-method +,geo,geo.set-method +,geo.set,geo-method
#' +,geo,geo-method +,geo.set,geo.set-method c,geo.set-method combine
#' combine.term combine<-,geo.set-method combine,geo.set-method
#' combine.term<-,geo.set-method combine.term,geo.set-method
#' geo.list,geo.set-method length,geo.set-method show,geo-method
#' name,geo-method sumlev,geo-method api.for,geo-method api.in,geo-method
#' combine<- combine.term<- name
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the
#' form \code{new("geo.set", ...)}, or more frequently through the
#' \code{geo.make()} helper function.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso \code{\link{geo.make}}
#' @references http://eglenn.scripts.mit.edu/citystate/category/code/
#' @keywords classes
#' @examples
#'
#' showClass("geo.set")
#'
NULL





#' County-level data from the 2007 American Community Survey for Kansas for
#' use in examples of acs package.
#'
#' County-level data from the 2007 American Community Survey for Kansas.
#' Contains demographic data on sex, age, and citizenship.  Used for examples
#' in acs package.  \code{kansas07} and the corresponding five-year survey
#' data in \code{kansas09} provide acs objects to test and demonstrate various
#' functions in the package.
#'
#'
#' @name kansas07
#' @docType data
#' @format An acs-class object with 7 rows/geographies and 55 demographic
#' variables, representing county-level ACS data for the state of Kansas.
#' Also includes geographic and other metadata.
#'
#' Note that in comparison to \code{kansas09}, \code{kansas07} has far fewer
#' rows, which illustrates the fact that the Census only provides ACS one-year
#' data for the largest counties (over 65,000 population).
#' @source U.S. Census American Community Survey, 2007;
#' \url{http://www.census.gov/}
#' @keywords datasets
#' @examples
#'
#' data(kansas07)
#' str(kansas07)
#' class(kansas07)
#'
#' geography(kansas07)
#'
#' # subsetting
#' kansas07[1:3,2:4]
#'
#' # row-wise addition
#' kansas07[1,6]+kansas07[2,6]
#'
#' # column-wise addition
#' kansas07[1:4,3]+kansas07[1:4,27]
#'
#'
NULL





#' County-level data from the 2005-2009 American Community Survey for Kansas
#' for use in examples of acs package.
#'
#' County-level data from the 2005-2009 American Community Survey for Kansas.
#' Contains demographic data on sex, age, and citizenship.  Used for examples
#' in acs package.  \code{kansas09}, and the corresponding one-year survey
#' data in \code{kansas07}, provide acs objects to test and demonstrate
#' various functions in the package.
#'
#'
#' @name kansas09
#' @docType data
#' @format An acs-class object with 105 rows/geographies and 55 demographic
#' variables, representing county-level ACS data for the state of Kansas.
#' Also includes geographic and other metadata.
#' @source U.S. Census American Community Survey, 2009;
#' \url{http://www.census.gov/}
#' @keywords datasets
#' @examples
#'
#' data(kansas09)
#' str(kansas09)
#' class(kansas09)
#'
#' geography(kansas09)
#'
#' # subsetting
#' kansas09[1:3,2:4]
#'
#' # row-wise addition
#' kansas09[1,6]+kansas09[2,6]
#'
#' # column-wise addition
#' kansas09[1:4,3]+kansas09[1:4,27]
#'
#'
NULL





#' Tract-level data from the 2006-2010 American Community Survey for Lawrence,
#' MA for use in examples of acs package.
#'
#' Tract-level data from the 2006-2010 American Community Survey for Lawrence,
#' MA.  Contains median household income.  Used for examples in acs package.
#'
#'
#' @name lawrence10
#' @docType data
#' @format An acs-class object with 18 rows/geographies and 1 variable,
#' representing tract-level ACS data for the city of Lawrence, MA from
#' 2006-2010.  Also includes geographic and other metadata.
#' @source U.S. Census American Community Survey, 2010;
#' \url{http://www.census.gov/}
#' @keywords datasets
#' @examples
#'
#' data(lawrence10)
#' str(lawrence10)
#' class(lawrence10)
#'
#' # subsetting
#' lawrence10[1:3,1]
#'
#' # row-wise subtraction
#' lawrence10[1,1]+lawrence10[2,1]
#'
#'
NULL





#' acs Methods for Function \code{plot}
#'
#' Plot acs objects, with both estimates and confidence intervals.
#'
#'
#' @name plot-methods
#' @aliases plot plot-methods plot,acs-method plot,acs,acs-method
#' @docType methods
#' @param x the acs object to be plotted
#' @param conf.level the desired confidence interval to use for error bars;
#' numeric between 0<1
#' @param err.col the color to use for the error bars; analogous to graphic
#' parameter \code{col}
#' @param err.lwd the line weight to use for the error bars; analogous to
#' graphic parameter \code{lwd}
#' @param err.pch the point character to use for the error bars; analogous to
#' graphic parameter \code{pch}
#' @param err.cex the scaling factor to use for the error bars; analogous to
#' graphic parameter \code{cex}
#' @param err.lty the line type to use for the error bars; analogous to
#' graphic parameter \code{lty}
#' @param x.res when plot called with a single acs value (see below), x.res
#' determines the resolution of the resulting density plot; integer (defaults
#' to 300, i.e., the curve is drawn with 300 points)
#' @param labels the labels to use for the x axis; defaults to either
#' geography names or acs.colnames based on dimensions of object plotted;
#' vector of proper length required
#' @param by in cases where multiple rows and columns are plotted, whether to
#' provide a different plot for each value of \code{geography} (the default)
#' or \code{acs.colnames}; accepts either "geography" or "acs.colnames"
#' @param true.min whether to limit the lower bound of a confidence interval
#' to some value or now; \code{TRUE} (the default) allows for negative lower
#' bounds; also accepts \code{FALSE} to limit lower bounds to 0, or any
#' number, to use that as a minimum lower bound; see details.
#' @param ... provided to allow for passing of additional arguments to plot
#' functions
#' @section Methods:
#'
#' \describe{
#'
#' \item{list("signature(object = \"acs\")")}{
#'
#' When passed an acs object (possibly involving subsetting), \code{plot} will
#' call a plot showing both estimates and confidence intervals for the data
#' contained in the object.
#'
#' If the object contains only one row or only one column, \code{plot} will
#' use this dimension as the y-axis and will plot each observation along the
#' x-axis, as three points (an estimate bracketed by upper and lower
#' confidence bounds).  If the object contains multiple rows and columns,
#' \code{plot} will return a 1-by-y "plot of plots": by default there will be
#' one plot per row showing all the data for each geography, although this can
#' be changed by specifying \code{by="acs.colnames"}, to plot each variable as
#' its own plot, with all of the geographies along the x-axis.
#'
#' In the special case where the dimensions of the object are exactly c(1,1)
#' (i.e., a single geography and column), \code{plot} will return a density
#' plot of the estimate.  In this case, \code{conf.level}, \code{err.col},
#' \code{err.lty}, and \code{err.lwd} will be used to determine the properties
#' of the margins of error lines.  (For none, use \code{conf.level=FALSE}.
#' For these density plots, users may also wish to set \code{xlim} and
#' \code{x.res}, which specify the horizontal extent and resolution of the
#' plot.)
#'
#' \code{plot} accepts many of the standard graphical arguments to
#' \code{plot}, such as \code{main}, \code{sub}, \code{xlab}, \code{pch}, and
#' \code{col}, as well new ones listed above.
#'
#' In some cases, the lower bound of a confidence interval may extend below 0;
#' in some cases this is desired, especially when a variable is actually
#' stating the \emph{difference} between two estimates.  In other cases, this
#' may seem confusing (for example, when reporting the estimated count in a
#' particular category).  Setting \code{true.min} to \code{FALSE} (or 0) will
#' limit the lower boundary of any confidence intervals computed and plotted.
#'
#' }
#'
#' }
#'
#' @keywords methods
#' @examples
#'
#'
#' # load ACS data
#' data(kansas07)
#'
#' # plot a single value
#' plot(kansas07[4,4])
#'
#' # plot by geography
#' plot(kansas07[,10])
#'
#' # plot by columns
#' plot(kansas07[4,3:10])
#'
#' # a density plot for a single variable
#' plot(kansas07[7,10])
#'
#' # same, using some graphical parameters
#' plot(kansas07[7,10], col="blue", err.col="purple", err.lty=3)
#'
#' plot(kansas07[7,49], col="lightblue", type="h", x.res=3000,
#' err.col="purple", err.lty=3, err.lwd=4, conf.level=.99,
#' main=(paste("Distribution of Females>85 Years in ",
#' geography(kansas07)[7,1], sep="")),
#' sub="(99-percent margin of error shown in purple)")
#'
#' # something more complicated...
#'
#' plot(kansas07[c(1,3,4),3:25], err.col="purple",
#' pch=16, err.pch="x", err.cex=1, ylim=c(0,5000),
#' col=rainbow(23), conf.level=.99,
#' labels=paste("grp. ",1:23))
#'
#'
NULL





#' acs Methods for Function \code{sum}
#'
#' Returns the sum of all the estimates present in its arguments, along with
#' proper treatment of standard errors.
#'
#' Note: when aggregating ACS data, users may want to sum many fields with "0"
#' values for estimates, especially when working with small geographies or
#' detailed tables that split the population into many categories.  In these
#' cases, some analysts have suggested that the traditional summation
#' procedure for standard errors (taking the square-root of the sum of the
#' squares of the errors) may over-inflate the associated margins of error;
#' instead, they recommend an alternative method, which ignores all but the
#' single largest of the standard errors for any "zero-estimate" fields.
#' Although this is somewhat unconventional, it is provided as an additional
#' user-specified option here, through the "one.zero" argument.
#'
#' @name sum-methods
#' @aliases sum sum-methods sum,acs-method sum,acs,acs-method
#' @docType methods
#' @param x the acs object to be summed
#' @param agg.term a character vector (length 1 or 2) of labels to use for the
#' geography or acs.colnames of the new object
#' @param one.zero a logical flag indicating whether to include standard
#' errors for only one zero-value estimates or all (the default); see details.
#' @param ... reserved for other arguments to pass
#' @param na.rm whether to remove \code{NA}s from the values before
#' \code{sum}ming; defaults to \code{FALSE}.
#' @section Methods: \describe{
#'
#' \item{list("signature(object = \"acs\")")}{ When passed an acs object
#' (possibly involving subsetting), \code{sum} will return a new acs object
#' created by aggregating (adding) all estimates in the object, and adding the
#' corresponding standard errors in a statistically appropriate way.
#' (Aggregate standard errors are computed by taking the square root of the
#' sum of the squared standard errors of the terms to be aggregated.)
#'
#' If the original object contains a single row, the geographic metadata and
#' row name is preserved; if not, the geographic metadata is replaced with the
#' term "aggregate" (or the contents of the first item of the (vector) option
#' \code{agg.term}).
#'
#' If the original object contains a single column, the column names and
#' acs.units data are preserved; if not, the column names are replaced with
#' the term "aggregate" or the contents of the second item of the (vector)
#' option \code{agg.term}; note: if \code{agg.term} is only one item in
#' length, it will be repeated here if needed.
#'
#' All other acs-class metadata is preserved, except for the \code{modified}
#' flag, which is set to TRUE. } }
#' @keywords methods
#' @examples
#'
#'
#' # load ACS data
#' data(kansas09)
#'
#' # aggregate the third column, all rows
#' sum(kansas09[,3])
#'
#' # aggregate the fifth row, all column
#' sum(kansas09[5,])
#'
#' # aggregate all rows, columns 3 through 25, rename rows "Kansas" and columns "Total Males"
#' sum(kansas09[, 3:25], agg.term=c("Kansas","Total Males"))
#'
NULL



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
