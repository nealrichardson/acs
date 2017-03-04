#' Create a new geo.set object for use with the acs package.
#'
#' The \code{geo.make} function is used to create new user-specified
#' geographies for use with the \code{acs.fetch} function of the acs package.
#' At the most basic level, a user specifies some combination of existing
#' census levels (state, county, county subdivision, place, tract, block
#' group, msa, csa, puma, and more -- see arguments), and the function returns
#' a new geo.set object holding this information.
#'
#' When specifying state, county, county subdivision, place, american indian
#' area, and/or any of the state school district arguments, \code{geo.make}
#' will accept either FIPS codes or common geographic names, and will try to
#' match on partial strings; there is also limited support for regular
#' expressions, but by default the searches are case sensitive and matches are
#' expected at the start of names. (For example, \code{geo.make(state="WA",
#' county="Kits")} should find Kitsap County, and the more adventurous
#' \code{yakima=geo.make(state="Washi", county=".*kima")} should work to
#' create the a geo.set for Yakima county.)
#'
#' Other geographies (including tract, block.group, csa, msa, region,
#' division, urban.area, necta, puma, zip.code. and/or congressional.district)
#' can only be specified by FIPS codes (or "*" for all).
#'
#' Tracts should be specified as six digit numbers, although initial zeroes
#' may be removed; note that trailing zeroes are often removed in common
#' usage, so a tract that may be referred to as "tract 243" is technically
#' FIPS code 24300; likewise "tract 3872.01" is FIPS code 387201 for the
#' purposes of geo.make.
#'
#' In addition to creating individual combinations of census geographies,
#' users can pass vector arguments (with recycling) to geo.make to create sets
#' of geographies.  Important: each set of arguments must match with exactly
#' one known Census geography: if, for example, the names of two places (or
#' counties, or whatever) would both match, the \code{geo.make} function will
#' return an error. (To the development team, this seemed preferable to simply
#' including both matches, since all sorts of place names might match a
#' string, and it is doubtful a user really wants them all.)  The one
#' exception to this "single match" rule is that for the smallest level of
#' geography specified, a user can enter "*" to indicate that all geographies
#' at that level should be selected.
#'
#' When creating new geographies, note, too, that not all combinations are
#' valid.  In particular the package attempts to follow paths through the
#' Census summary levels (such as summary level 140: "state-county-tract" or
#' summary level 160: "state-place").  So when specifying, for example, state,
#' county, and place, the county will be ignored.
#'
#' Note: when a geo.set with "combine=TRUE" is passed to \code{acs.fetch},
#' downloaded data will be aggregated in the resulting acs abject.  Some users
#' may therefore wish to specify "one.zero=TRUE" as an additional argument to
#' \code{acs.fetch}; see \code{\link{sum-methods}}.
#'
#' The following table may be helpful in figuring out which options to set for
#' which Census summary levels.  For more information on which datasets and
#' endyear/span combinations are available for each summary level, see
#' \url{http://www.census.gov/data/developers/data-sets.html} (click each
#' dataset and search for "Examples and Supported Geography").
#'
#' \tabular{cl}{ SUMMARY LEVEL \tab ARGUMENTS REQUIRED \cr 010 \tab us \cr 020
#' \tab region \cr 030 \tab division \cr 040 \tab state \cr 050 \tab state,
#' county \cr 060 \tab state, county, county.subdivision \cr 140 \tab state,
#' county, tract \cr 150 \tab state, county, tract, block.group \cr 160 \tab
#' state, place \cr 250 \tab american.indian.area \cr 310 \tab msa \cr 320
#' \tab state, msa \cr 330 \tab csa \cr 340 \tab state, csa \cr 350 \tab necta
#' \cr 400 \tab urban.area \cr 500 \tab state, congressional.district \cr 510
#' \tab state, congressional.district, county \cr 610 \tab state,
#' state.legislative.district.upper \cr 620 \tab state,
#' state.legislative.district.lower \cr 795 \tab state, puma \cr 860 \tab
#' zip.code \cr 950 \tab state, school.district.elementary \cr 960 \tab state,
#' school.district.secondary \cr 970 \tab state, school.district.unified \cr }
#'
#' All other arguments/combinations will either be ignored or result in a
#' failure.
#'
#' @param us either the number 1, the character "*", or TRUE, indicating
#' whether the geo.set should contain data for the entire U.S.; if selected,
#' no other geography options may be specified; setting us corresponds to
#' using census summary level 010.
#' @param region a numeric code (or wildcard "*" for all) corresponding to the
#' desired FIPS region (e.g., region=1 for Census Northeast Region); if
#' selected, no other geography options may be specified; setting region
#' corresponds to using census summary level 020.
#' @param division a numeric code (or wildcard "*" for all) corresponding to
#' the desired FIPS division (e.g., division=4 for Census West North Central
#' Division); if selected, no other geography options may be specified;
#' setting division corresponds to using census summary level 030.
#' @param american.indian.area either the numeric code (or wildcard "*" for
#' all) corresponding to the desired FIPS American Indian Area/Alaska Native
#' Area/Hawaiian Home Land, or a character string to match in the names of
#' these Census areas; if selected, no other geography options may be
#' specified; setting american.indian.area corresponds to using census summary
#' level 250.
#' @param state either the two-digit numeric FIPS code for the state, the
#' two-letter postal abbreviation, or a character string to match in the state
#' name (or wildcard "*" for all); setting state without other options
#' corresponds to using census summary level 040, but it may be used in
#' conjunction with other summary levels below.
#' @param county either the numeric FIPS code (or wildcard "*" for all) for
#' the county or a character string to match in the county name; setting state
#' and county without other options corresponds to using census summary level
#' 050, but they may be used in conjunction with other summary levels below.
#' @param county.subdivision either the numeric FIPS code (or wildcard "*" for
#' all) for the county subdivision or a character string to match in the
#' county subdivision name; setting state, county, and county.subdivision
#' without other options corresponds to using census summary level 060.
#' @param place either the numeric FIPS code (or wildcard "*" for all) for the
#' place or a character string to match in the place name; setting state and
#' place without other options corresponds to using census summary level 160.
#' @param tract a six digit numeric FIPS code (or wildcard "*" for all) for
#' the census tract, including trailing zeroes; remove decimal points; leading
#' zeroes may be omitted; see description; tract may be used with state and
#' county to create geo.sets for census summary levels 140, and with state,
#' county, and block.group for summary level 150.
#' @param block.group the numeric FIPS code (or wildcard "*" for all) for the
#' block.group; block.group may be used with state, county, and tract to
#' create geo.sets for census summary levels 150.
#' @param msa a numeric code (or wildcard "*" for all) corresponding to the
#' desired FIPS metropolitan statistical area/micropolitan statistical area
#' (e.g., msa=10100 for Aberdeen, SD micropolitan statistical area); setting
#' msa without other options corresponds to using census summary level 310,
#' but it may be used in conjunction with state for summary level 320.
#' @param csa a numeric code (or wildcard "*" for all) corresponding to the
#' desired FIPS combined statistical area (e.g., csa=104 for Census
#' Albany-Schenectady-Amsterdam, NY CSA); setting csa without other options
#' corresponds to using census summary level 330, but it may be used in
#' conjunction with state for summary level 340.
#' @param necta a numeric code (or wildcard "*" for all) corresponding to the
#' desired FIPS New England City and Town Area (e.g., necta=77650 for Rutland,
#' VT Micropolitan NECTA); if selected, no other geography options may be
#' specified; setting necta corresponds to using census summary level 350.
#' @param urban.area a numeric code (or wildcard "*" for all) corresponding to
#' the desired FIPS urban area (e.g., urban.area=3169 for Aromas, CA Urban
#' Cluster); if selected, no other geography options may be specified; setting
#' urban.area corresponds to using census summary level 400.
#' @param congressional.district a numeric code (or wildcard "*" for all)
#' corresponding to the desired FIPS congressional district (e.g., state="ME"
#' and congressional.district=1 for Maine's first congressional district);
#' setting state and congressional.district without other options corresponds
#' to using census summary level 500, but they may be used in conjunction with
#' county for summary level 510.
#' @param state.legislative.district.upper a numeric or character code (or
#' wildcard "*" for all) corresponding to the desired FIPS state legislative
#' district (upper chamber); these codes vary from state to state, and are
#' sometimes numbers (1, 2, 3, etc. in Massachusetts) and sometimes letters
#' ("A", "B", "C", etc. in Alaska); setting state and
#' state.legislative.district.upper without other options corresponds to using
#' census summary level 610.
#' @param state.legislative.district.lower a numeric or character code (or
#' wildcard "*" for all) corresponding to the desired FIPS state legislative
#' district (lower chamber); these codes vary from state to state, and are
#' sometimes numbers (1, 2, 3, etc. in Massachusetts) and sometimes letters
#' ("A", "B", "C", etc. in Alaska); setting state and
#' state.legislative.district.lower without other options corresponds to using
#' census summary level 620.
#' @param puma a numeric code (or wildcard "*" for all) corresponding to the
#' desired FIPS public use microdata area (e.g., state=10 and puma=103 for
#' PUMA 103 in Delaware); setting state and puma without other options
#' corresponds to using census summary level 795.
#' @param zip.code a numeric code (or wildcard "*" for all) corresponding to
#' the desired zip code tabulation area (e.g., zip.code=91303 for zip code
#' 91303); if selected, no other geography options may be specified; setting
#' zip.code corresponds to using census summary level 860.
#' @param school.district.elementary a numeric code (or wildcard "*" for all)
#' corresponding to the desired FIPS state school district (elementary), or a
#' character string to search for in the names of these districts; setting
#' state and school.district.elementary without other options corresponds to
#' using census summary level 950.
#' @param school.district.secondary a numeric code (or wildcard "*" for all)
#' corresponding to the desired FIPS state school district (secondary), or a
#' character string to search for in the names of these districts; setting
#' state and school.district.secondary without other options corresponds to
#' using census summary level 960.
#' @param school.district.unified a numeric code (or wildcard "*" for all)
#' corresponding to the desired FIPS state school district (unified), or a
#' character string to search for in the names of these districts; setting
#' state and school.district.unified without other options corresponds to
#' using census summary level 970.
#' @param combine a logical flag to indicate whether the component geographies
#' of the geo.set are to be combined when data is downloaded; see details.
#' @param combine.term a character string to provide a label for aggregate
#' geography, if data is combined
#' @param check logical flag indicating whether to run a check for valid
#' geographies with Census API; defaults to FALSE; when TRUE, a current API
#' key must be provided or installed
#' @param key when check=TRUE and no API key has been previously installed
#' through \code{api.key.install}, a string key may be provided here
#' @return Returns a geo.set class object.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso \code{\link{geo.set-class}}
#' @references \enumerate{
#'
#' \item{"acs.R: An R Package for Neighborhood-Level Data from the U.S.
#' Census." Ezra Haber Glenn, Department of Urban Studies and Planning,
#' Massachusetts Institute of Technology.  Presented at the Computers in Urban
#' Planning and Urban Management Conference, July 6, 2011.
#' \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2171390}.}
#'
#' \item{Census API Supported Geography:
#' \url{http://www.census.gov/data/developers/data-sets.html}}}
#' @examples
#'
#'
#' # some single-state geo.sets
#' washington=geo.make(state=53)
#' alabama=geo.make(state="Alab")
#'
#' # a county match
#' yakima=geo.make(state="WA", county="Yakima")
#' yakima
#'
#' # a multiple-county geo.set
#' psrc=geo.make(state="WA", county=c(33,35,53,61))
#' psrc
#'
#' # combine geo.sets
#' north.mercer.island=geo.make(state=53, county=33, tract=c(24300,24400))
#' optional.tract=geo.make(state=53, county=33, tract=24500)
#' # add in one more tract to create new, larger geo
#' north.mercer.island.plus=north.mercer.island + optional.tract
#'
#' # using wildcards
#'
#' # all unified school districts in Kansas
#' geo.make(state="KS", school.district.unified="*")
#'
#' # all state house districts in Alaska
#' geo.make(state="AK", state.legislative.district.lower="*")
#'
#' # all tracts in Kings County, NY
#' geo.make(state="NY", county="King", tract="*")
#'
#'
geo.make <- function(us, region, division, state, county, county.subdivision,
    place, tract, block.group, msa, csa, necta, urban.area,
    congressional.district, state.legislative.district.upper,
    state.legislative.district.lower, puma, zip.code, american.indian.area,
    school.district.elementary, school.district.secondary,
    school.district.unified, combine=FALSE, combine.term = "aggregate",
    check=FALSE, key=api.key.load()) {

    .geo.unit.make <- function (us, region, division, state, county,
        county.subdivision,
        place, tract, block.group, msa, csa, necta, urban.area, congressional.district,
        state.legislative.district.upper, state.legislative.district.lower, puma,
        zip.code, american.indian.area, school.district.elementary, school.district.secondary,
        school.district.unified) {

        geo.obj <- NA
        nargs <- nargs()
        ## geos with only one argument: sumlev 010, 020, 030, 350, 400, 860 sumlev 010 --
        ## all US
        if (!missing(us) && (us == 1 || us == "*" || us == TRUE)) {
            if (nargs > 1) {
                warning("entire U.S. selected; no other geographic arguments allowed;\n  returning NA")
                return(NA)
            }
            if (us == TRUE)
                us <- 1
            geo.obj <- new(Class = "geo", api.for = list(us = us), api.in = list(),
                name = "US", sumlev = 10)
            return(geo.obj)
        }
        # sumlev 020 -- region
        if (!missing(region)) {
            if (nargs > 1) {
                warning("region selected; ; no other geographic arguments allowed;\n  returning NA")
                return(NA)
            }
            geo.obj <- new(Class = "geo", api.for = list(region = region), api.in = list(),
                name = paste("Region ", region, sep = ""), sumlev = 20)
            return(geo.obj)
        }
        # sumlev 030 -- division
        if (!missing(division)) {
            if (nargs > 1) {
                warning("division selected; no other geographic arguments allowed;\n  returning NA")
                return(NA)
            }
            geo.obj <- new(Class = "geo", api.for = list(division = division), api.in = list(),
                name = paste("Division ", division, sep = ""), sumlev = 30)
            return(geo.obj)
        }
        # sumlev 250 -- american indian area/alaska native area/hawaiian home land check
        # for american indian area names and convert to codes
        if (!missing(american.indian.area) && is.character(american.indian.area) &&
            american.indian.area != "*") {
            american.indian.area <- fips.american.indian.area[grepl(paste("^", american.indian.area,
                sep = ""), fips.american.indian.area$American.Indian.Area.Name),
                1]
            if (length(american.indian.area) > 1) {
                warning("More than one American Indian area/Alaska Native area/Hawaiian Home Land name matches search string;\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
            if (length(american.indian.area) == 0) {
                warning("No American Indian area/Alaska Native area/Hawaiian Home Land name matches search string;\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
        }
        ## deal with wildcards
        if (hasArg(american.indian.area) && american.indian.area != "*")
            american.indian.area.name <- fips.american.indian.area[fips.american.indian.area$American.Indian.Area.Code ==
                american.indian.area, 2] else american.indian.area.name <- "All American Indian/Alaska Native/Hawaiian Home Land areas"  # later, not used unless hasArg(county)
        ## make geo for sumlev 250
        if (!missing(american.indian.area)) {
            if (nargs > 1) {
                warning("american.indian.area selected; \n  no other geographic arguments allowed;\n  returning NA")
                return(NA)
            }
            geo.obj <- new(Class = "geo", api.for = list(`american+indian+area/alaska+native+area/hawaiian+home+land` = american.indian.area),
                api.in = list(), name = american.indian.area.name, sumlev = 250)
            return(geo.obj)
        }
        # sumlev 310 -- metropolitan statistical area/micropolitan statistical area **NO
        # STATE**
        if (!missing(msa) && missing(state)) {
            if (nargs > 1) {
                warning("msa selected; ignoring all other geographic arguments")
            }
            geo.obj <- new(Class = "geo", api.for = list(`metropolitan+statistical+area/micropolitan+statistical+area` = msa),
                api.in = list(), name = paste("MSA ", msa, sep = ""), sumlev = 310)
            return(geo.obj)
        }
        # sumlev 330 -- combined statistical area **NO STATE**
        if (!missing(csa) && missing(state)) {
            if (nargs > 1) {
                warning("csa selected; ignoring all other geographic arguments")
            }
            geo.obj <- new(Class = "geo", api.for = list(`combined+statistical+area` = csa),
                api.in = list(), name = paste("CSA ", csa, sep = ""), sumlev = 330)
            return(geo.obj)
        }
        # sumlev 350 -- new england city and town area
        if (!missing(necta)) {
            if (nargs > 1) {
                warning("necta selected;  no other geographic arguments allowed;\n  returning NA")
                return(NA)
            }
            geo.obj <- new(Class = "geo", api.for = list(`new+england+city+and+town+area` = necta),
                api.in = list(), name = paste("NECTA ", necta, sep = ""), sumlev = 350)
            return(geo.obj)
        }
        # sumlev 400 -- urban area
        if (!missing(urban.area)) {
            if (nargs > 1) {
                warning("urban.area selected;  no other geographic arguments allowed;\n  returning NA")
                return(NA)
            }
            geo.obj <- new(Class = "geo", api.for = list(`urban+area` = urban.area),
                api.in = list(), name = paste("Urban Area ", urban.area, sep = ""),
                sumlev = 400)
            return(geo.obj)
        }
        # sumlev 860 -- zip.code
        if (!missing(zip.code)) {
            if (nargs > 1) {
                warning("zip.code selected, no other geographic arguments allowed;\n  returning NA")
                return(NA)
            }
            geo.obj <- new(Class = "geo", api.for = list(`zip+code+tabulation+area` = zip.code),
                api.in = list(), name = paste("Zip Code Tabulation Area ", zip.code,
                  sep = ""), sumlev = 860)
            return(geo.obj)
        }
        # all other geos need a valid state
        if (missing(state)) {
            warning("state required")
            return(NA)
        }
        # check for state abbreviations/text names and convert to codes
        if (is.character(state) && state != "*") {
            if (nchar(state) == 2)
                state <- fips.state[fips.state$STUSAB == state, 1] else state <- fips.state[grep(paste("^", state, sep = ""), fips.state$STATE_NAME),
                1]
            if (length(state) == 0) {
                warning("No valid state names match search string;\n  returning NA")
                return(NA)
            }
            if (length(state) > 1) {
                warning("More than one state name matches search string;\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
        }
        # check for county names and convert to codes
        if (!missing(county) && is.character(county) && county != "*") {
            county <- fips.county[grepl(paste("^", county, sep = ""), fips.county$County.Name) &
                (fips.county$State.ANSI == state), 3]
            if (length(county) > 1) {
                warning("More than one county name matches search string in state ",
                  state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
            if (length(county) == 0) {
                warning("No county name matches search string in state ", state,
                  ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
        }
        # check for county subdivision names and convert to codes
        if (!missing(county.subdivision) && is.character(county.subdivision) && county.subdivision !=
            "*") {
            county.subdivision <- fips.county.subdivision[grepl(paste("^", county.subdivision,
                sep = ""), fips.county.subdivision$COUSUBNAME) & (fips.county.subdivision$STATEFP ==
                state), 5]
            if (length(county.subdivision) > 1) {
                warning("More than one county subdivision name matches search string in state ",
                  state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
            if (length(county.subdivision) == 0) {
                warning("No county subdivision name matches search string in state ",
                  state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
        }
        # check for place names and convert to codes
        if (!missing(place) && is.character(place) && place != "*") {
            place <- fips.place[grepl(paste("^", place, sep = ""), fips.place$PLACENAME) &
                (fips.place$STATEFP == state), 3]
            if (length(place) > 1) {
                if (isTRUE(all.equal(max(place), min(place))) && isTRUE(all.equal(max(place),
                  min(place)))) {
                  # i.e., all match same place #
                  place <- place[1]
                } else {
                  warning("More than one place name matches search string in state ",
                    state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                  return(NA)
                }
            }
            if (length(place) == 0) {
                warning("No place name matches search string in state ", state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
        }
        # check for school district names and convert to codes elementary
        if (!missing(school.district.elementary) && is.character(school.district.elementary) &&
            school.district.elementary != "*") {
            school.district.elementary <- fips.school[grepl(paste("^", school.district.elementary,
                sep = ""), fips.school$SDNAME) & (fips.school$STATEFP == state) &
                (fips.school$TYPE == "Elementary"), 3]
            if (length(school.district.elementary) > 1) {
                warning("More than one elementary school district name matches search string in state ",
                  state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
            if (length(school.district.elementary) == 0) {
                warning("No elementary school district name matches search string in state ",
                  state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
        }
        ## secondary
        if (!missing(school.district.secondary) && is.character(school.district.secondary) &&
            school.district.secondary != "*") {
            school.district.secondary <- fips.school[grepl(paste("^", school.district.secondary,
                sep = ""), fips.school$SDNAME) & (fips.school$STATEFP == state) &
                (fips.school$TYPE == "Secondary"), 3]
            if (length(school.district.secondary) > 1) {
                warning("More than one secondary school district name matches search string in state ",
                  state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
            if (length(school.district.secondary) == 0) {
                warning("No secondary school district name matches search string in state ",
                  state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
        }
        ## unified
        if (!missing(school.district.unified) && is.character(school.district.unified) &&
            school.district.unified != "*") {
            school.district.unified <- fips.school[grepl(paste("^", school.district.unified,
                sep = ""), fips.school$SDNAME) & (fips.school$STATEFP == state) &
                (fips.school$TYPE == "Unified"), 3]
            if (length(school.district.unified) > 1) {
                warning("More than one unified school district name matches search string in state ",
                  state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
            if (length(school.district.unified) == 0) {
                warning("No unified school district name matches search string in state ",
                  state, ";\n  returning NA;\n  Perhaps try 'geo.lookup()...?")
                return(NA)
            }
        }
        # deal with names for wildcards
        if (state != "*")
            state.name <- fips.state[fips.state$STATE == state, 3] else state.name <- "All states"
        if (hasArg(county) && county != "*")
            county.name <- fips.county[fips.county$County.ANSI == county & fips.county$State.ANSI ==
                state, 4] else county.name <- "All counties"  # later, not used unless hasArg(county)
        if (hasArg(place) && place != "*") {
            place.name <- fips.place[fips.place$PLACEFP == place & fips.place$STATEFP ==
                state, 4]
            if (length(place.name) > 1)
                place.name <- place.name[1]
        } else place.name <- "All places"  # later, not used unless hasArg(place)
        if (hasArg(county.subdivision) && county.subdivision != "*")
            subdivision.name <- fips.county.subdivision[fips.county.subdivision$COUSUBFP ==
                county.subdivision & fips.county.subdivision$STATEFP == state, 6] else subdivision.name <- "All subdivisions"  # later, not used unless hasArg(county.subdivision)
        if (hasArg(school.district.elementary) && school.district.elementary != "*")
            school.district.elementary.name <- fips.school[fips.school$LEA == school.district.elementary &
                fips.school$STATEFP == state, 4] else school.district.elementary.name <- "All elementary school districts"  # later, not used unless hasArg(county)
        if (hasArg(school.district.secondary) && school.district.secondary != "*")
            school.district.secondary.name <- fips.school[fips.school$LEA == school.district.secondary &
                fips.school$STATEFP == state, 4] else school.district.secondary.name <- "All secondary school districts"  # later, not used unless hasArg(county)
        if (hasArg(school.district.unified) && school.district.unified != "*")
            school.district.unified.name <- fips.school[fips.school$LEA == school.district.unified &
                fips.school$STATEFP == state, 4] else school.district.unified.name <- "All unified school districts"  # later, not used unless hasArg(county)
        # done with looking up names and codes sumlev 40 (state)
        if (nargs == 1 && hasArg(state))
            geo.obj <- new(Class = "geo", api.for = list(state = state), api.in = list(),
                name = state.name, sumlev = 40)
        # sumlev 50 (state-county)
        if (nargs == 2 && hasArg(county))
            geo.obj <- new(Class = "geo", api.for = list(county = county), api.in = list(state = state),
                name = paste(county.name, state.name, sep = ", "), sumlev = 50)
        # sumlev 160 (state-place) -- only sumlev with place
        if (hasArg(place)) {
            if (nargs > 2)
                warning("Using sumlev 160 (state-place)\n  Other levels not supported by census api at this time")
            geo.obj <- new(Class = "geo", api.for = list(place = place), api.in = list(state = state),
                name = paste(place.name, state.name, sep = ", "), sumlev = 160)
        }
        # sumlev 60 (state-county-county.subdivision)
        if (nargs == 3 && hasArg(county) && hasArg(county.subdivision))
            geo.obj <- new(Class = "geo", api.for = list(`county+subdivision` = county.subdivision),
                api.in = list(state = state, county = county), name = paste(subdivision.name,
                  county.name, state.name, sep = ", "), sumlev = 60)
        # sumlev 140 (state-county-tract)
        if (nargs == 3 && hasArg(county) && hasArg(tract))
            geo.obj <- new(Class = "geo", api.for = list(tract = tract), api.in = list(state = state,
                county = county), name = paste("Tract ", tract, ", ", county.name,
                ", ", state.name, sep = ""), sumlev = 140)
        # sumlev 150 (state-county-tract-block.group)
        if (nargs == 4 && hasArg(county) && hasArg(tract) && hasArg(block.group))
            geo.obj <- new(Class = "geo", api.for = list(`block+group` = block.group),
                api.in = list(state = state, county = county, tract = tract), name = paste("Tract ",
                  tract, ", Blockgroup ", block.group, ", ", county.name, ", ", state.name,
                  sep = ""), sumlev = 150)
        # sumlev 320 (state-msa)
        if (nargs == 2 && hasArg(msa))
            geo.obj <- new(Class = "geo", api.for = list(`metropolitan+statistical+area/micropolitan+statistical+area` = msa),
                api.in = list(state = state), name = paste("MSA ", msa, ", ", state.name,
                  sep = ""), sumlev = 320)
        # sumlev 340 (state-csa)
        if (nargs == 2 && hasArg(csa))
            geo.obj <- new(Class = "geo", api.for = list(`combined+statistical+area` = csa),
                api.in = list(state = state), name = paste("CSA ", csa, ", ", state.name,
                  sep = ""), sumlev = 340)
        # sumlev 500 (state-congressional.district)
        if (nargs == 2 && hasArg(congressional.district))
            geo.obj <- new(Class = "geo", api.for = list(`congressional+district` = congressional.district),
                api.in = list(state = state), name = paste("Congressional District ",
                  congressional.district, ", ", state.name, sep = ""), sumlev = 500)
        # sumlev 510 (state-congressional.district-county)
        if (nargs == 3 && hasArg(county) && hasArg(congressional.district))
            geo.obj <- new(Class = "geo", api.for = list(county = county), api.in = list(state = state,
                `congressional+district` = congressional.district), name = paste(county.name,
                ", Congressional District ", congressional.district, ", ", state.name,
                sep = ""), sumlev = 510)
        # sumlev 610 (state-state.leg.upper)
        if (nargs == 2 && hasArg(state.legislative.district.upper)) {
            # turn to string exactly three characters long
            if (state.legislative.district.upper != "*")
                state.legislative.district.upper <- str_sub(paste("000", state.legislative.district.upper,
                  sep = ""), -3, -1)
            geo.obj <- new(Class = "geo", api.for = list(`state+legislative+district+(upper+chamber)` = state.legislative.district.upper),
                api.in = list(state = state), name = paste("State Legislative District (upper chamber) ",
                  state.legislative.district.upper, ", ", state.name, sep = ""),
                sumlev = 610)
        }
        # sumlev 620 (state-state.leg.lower)
        if (nargs == 2 && hasArg(state.legislative.district.lower)) {
            # turn to string exactly three characters long
            if (state.legislative.district.lower != "*")
                state.legislative.district.lower <- str_sub(paste("000", state.legislative.district.lower,
                  sep = ""), -3, -1)
            geo.obj <- new(Class = "geo", api.for = list(`state+legislative+district+(lower+chamber)` = state.legislative.district.lower),
                api.in = list(state = state), name = paste("State Legislative District (lower chamber) ",
                  state.legislative.district.lower, ", ", state.name, sep = ""),
                sumlev = 620)
        }
        # sumlev 795 (state-puma)
        if (nargs == 2 && hasArg(puma))
            geo.obj <- new(Class = "geo", api.for = list(`public+use+microdata+area` = puma),
                api.in = list(state = state), name = paste("Public Use Microdata Area ",
                  puma, ", ", state.name, sep = ""), sumlev = 795)
        # sumlev 950 (state-school.elementary)
        if (nargs == 2 && hasArg(school.district.elementary))
            geo.obj <- new(Class = "geo", api.for = list(`school+district+(elementary)` = school.district.elementary),
                api.in = list(state = state), name = paste(school.district.elementary.name,
                  ", ", state.name, sep = ""), sumlev = 950)
        # sumlev 960 (state-school.secondary)
        if (nargs == 2 && hasArg(school.district.secondary))
            geo.obj <- new(Class = "geo", api.for = list(`school+district+(secondary)` = school.district.secondary),
                api.in = list(state = state), name = paste(school.district.secondary.name,
                  ", ", state.name, sep = ""), sumlev = 960)
        # sumlev 970 (state-school.unified)
        if (nargs == 2 && hasArg(school.district.unified))
            geo.obj <- new(Class = "geo", api.for = list(`school+district+(unified)` = school.district.unified),
                api.in = list(state = state), name = paste(school.district.unified.name,
                  ", ", state.name, sep = ""), sumlev = 970)
        # fail if still no match
        if (!is.geo(geo.obj)) {
            warning("No valid geography from these arguments; returning NA\n  Perhaps add or drop levels from this request?")
        }
        geo.obj
    }
    ## after here is actual function to recur through more complex requests recycle!

    arglist <- as.list(environment())
    missing.args <- unlist(lapply(arglist, is.symbol))
    arglist <- arglist[!missing.args]
    arglist <- arglist[names(arglist) != "combine"]
    arglist <- arglist[names(arglist) != "combine.term"]
    arglist <- arglist[names(arglist) != "key"]
    arglist <- arglist[names(arglist) != "check"]
    max.length <- max(sapply(arglist, length))
    arglist <- lapply(arglist, rep, length = max.length)
    if (max.length == 1) {
        geo.obj <- do.call(.geo.unit.make, arglist)
        geo.set.obj <- new(Class = "geo.set", geo.list = list(geo.obj), combine = combine,
            combine.term = combine.term)
    } else {
        geo.a <- do.call(.geo.unit.make, lapply(arglist, head, 1))
        geo.b <- do.call(geo.make, lapply(arglist, tail, -1))
        geo.set.obj <- geo.a + geo.b
    }
    if (is.geo.set(geo.set.obj)) {
        geo.set.obj@combine <- combine
        geo.set.obj@combine.term <- combine.term
    }
    if (check) {
        for (i in 1:length(geo.set.obj)) {
            cat(paste("Testing geography item ", i, ": ", name(geo.list(geo.set.obj[i])),
                " .... ", sep = ""))
            obj.test <- acs.fetch(endyear = 2010, geography = geo.set.obj[i], key = key,
                variable = "B01001_001", lookup = FALSE)
            cat("OK.\n")
        }
    }
    geo.set.obj
}
