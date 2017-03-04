#' Search Census geographies
#' 
#' 
#' When working with the acs package and the \code{acs.fetch} and
#' \code{geo.make} functions, it can be difficult to find exactly the right
#' geographic units: \code{geo.make} expects single matches to the groups of
#' arguments it is given, which can be problematic when trying to find names
#' for places or county subdivisions, which are unfamiliar to many users (and
#' often seem very close or redundant: e.g., knowing whether to look for
#' "Moses Lake city" vs. "Moses Lake CDP").  To help, the \code{geo.lookup}
#' function will search on the same arguments as \code{geo.make}, but outputs
#' all the matches for your inspection.
#' 
#' Unlike \code{geo.make}, \code{geo.lookup} searches for matches anywhere in
#' geographic names (except when dealing with state names), and will output a
#' dataframe showing candidates that match some or all of the arguments.
#' (When multiple arguments are provided, the logic is a little complicated:
#' basically, with the exception of American Indian Areas, to be included all
#' geographies must match the given state name; when a county and a
#' subdivision are both given, both must match; otherwise, geographies are
#' included that match any --- but not necessarily all --- of the other
#' arguments.)
#' 
#' @param state either the two-digit numeric FIPS code for the state, the
#' two-letter postal abbreviation, or a character string to match in the state
#' name
#' @param county either the numeric FIPS code for the county or a character
#' string to match in the county name
#' @param county.subdivision either the numeric FIPS code for the county
#' subdivision or a character string to match in the county subdivision name
#' @param place either the numeric FIPS code for the place or a character
#' string to match in the place name
#' @param american.indian.area either the numeric FIPS code for the American
#' Indian Area/Alaska Native Area/Hawaiian Home Land, or a character string to
#' match in the names of these Census areas
#' @param school.district either the numeric FIPS code for the state school
#' district (any type), or a character string to search for in the names of
#' the school districts.
#' @param school.district.elementary either the numeric FIPS code for the
#' state school district (elementary), or a character string to search for in
#' the names of these elementary school districts.
#' @param school.district.secondary either the numeric FIPS code for the state
#' school district (secondary), or a character string to search for in the
#' names of these secondary school districts.
#' @param school.district.unified either the numeric FIPS code for the state
#' school district (unified), or a character string to search for in the names
#' of these unified school districts.
#' @return Returns a dataframe of the matching geographies, with one column
#' for each of the given search terms.
#' @author Ezra Haber Glenn \email{eglenn@@mit.edu}
#' @seealso \code{\link{geo.make}}
#' @examples
#' 
#' geo.lookup(state="WA", county="Ska", county.subdivision="oo")
#' geo.lookup(state="WA", county="Kit", place="Ra")
#' 
#' # find all counties in WA or OR with capital M or B in name
#' geo.lookup(state=c("WA", "OR"), county=c("M","B"))
#' 
#' # find all unified school districts in Kansas with "Ma" in name
#' geo.lookup(state="KS", school.district.unified="Ma")
#' 
#' # find all american indian areas with "Hop" in name
#' geo.lookup(american.indian.area="Hop")
#' 
geo.lookup <- function(state, county, county.subdivision, place, american.indian.area,
    school.district, school.district.elementary, school.district.secondary, school.district.unified) {
    # first deal with american indian areas -- only one with no state
    if (!missing(american.indian.area)) {
        if (nargs() > 1) {
            warning("american indian area selected; no other options allowed; \n  returning NA")
            return(NA)
        }
        if (is.character(american.indian.area)) {
            fips.american.indian.area <- fips.american.indian.area[grepl(paste(american.indian.area,
                collapse = "|"), fips.american.indian.area$American.Indian.Area.Name),
                ]
        } else {
            fips.american.indian.area <- fips.american.indian.area[fips.american.indian.area$American.Indian.Area.Code %in%
                american.indian.area]
        }
        american.indian.area <- fips.american.indian.area[, 1]
        american.indian.area.name <- fips.american.indian.area[, 2]
        results <- data.frame(american.indian.area, american.indian.area.name, stringsAsFactors = FALSE)
        return(results)
    }
    # all remaining need state
    state.name <- NA
    if (missing(state)) {
        warning("state required for geo.lookup with these options; \n  returning NA")
        return(NA)
    }
    for (i in 1:length(state)) {
        if (is.character(state[i])) {
            if (nchar(state[i]) == 2) {
                state[i] <- fips.state[fips.state$STUSAB == state[i], 1]
            } else {
                state[i] <- fips.state[grep(paste("^", state[i], sep = ""), fips.state$STATE_NAME),
                  1]
            }
        }
    }
    state <- state[state %in% fips.state$STATE]  # remove non-matches
    state.name <- fips.state[fips.state$STATE %in% state, 3]
    if (length(state) == 0) {
        warning("No valid state names match search string;\n  returning NA")
        return(NA)
    }
    if (length(state) > 1) {
        state <- as.integer(state)
        a <- geo.lookup(state = state[1], county = county, county.subdivision = county.subdivision,
            place = place, school.district = school.district, school.district.elementary = school.district.elementary,
            school.district.secondary = school.district.secondary, school.district.unified = school.district.unified)
        b <- geo.lookup(state = state[2:length(state)], county = county, county.subdivision = county.subdivision,
            place = place, school.district = school.district, school.district.elementary = school.district.elementary,
            school.district.secondary = school.district.secondary, school.district.unified = school.district.unified)
        return(rbind.fill(a, b))
    }
    results <- data.frame(state = state, state.name = state.name, stringsAsFactors = FALSE)
    # check counties
    fips.county.sub <- fips.county.subdivision[fips.county.subdivision$STATEFP ==
        state, ]
    if (!missing(county)) {
        fips.county <- fips.county[fips.county$State.ANSI == state, ]
        if (is.character(county)) {
            fips.county <- fips.county[grepl(paste(county, collapse = "|"), fips.county$County.Name),
                ]
        } else {
            fips.county <- fips.county[fips.county$County.ANSI %in% county, ]
        }
        county <- fips.county[, 3]
        # need to fix for when county is numeric vector, here and below else
        # {county=county[county %in% fips.county$County.ANSI]}
        county.name <- fips.county[, 4]
        if (length(county) > 0) {
            results <- rbind.fill(results, data.frame(state, state.name, county,
                county.name, stringsAsFactors = FALSE))
        }
    }
    # check subdivisions, when no county given; state still required
    if (missing(county) && !missing(county.subdivision)) {
        if (is.character(county.subdivision)) {
            fips.county.sub <- fips.county.sub[grepl(paste(county.subdivision, collapse = "|"),
                fips.county.sub$COUSUBNAME), ]
        } else {
            fips.county.sub <- fips.county.sub[fips.county.sub$COUSUBFP %in% county.subdivision,
                ]
        }
        county.subdivision <- fips.county.sub[, 5]
        subdivision.name <- fips.county.sub[, 6]
        this.county <- fips.county.sub[, 3]
        this.county.name <- fips.county.sub[, 4]
        if (length(county.subdivision) > 0) {
            results <- rbind.fill(results, data.frame(state, state.name, county = this.county,
                county.name = this.county.name, county.subdivision, county.subdivision.name = subdivision.name,
                stringsAsFactors = FALSE))
        }
    }
    # check subdivisions, when county is given
    if (!missing(county) && !missing(county.subdivision)) {
        if (is.character(county.subdivision)) {
            fips.county.sub <- fips.county.sub[grepl(paste(county.subdivision, collapse = "|"),
                fips.county.sub$COUSUBNAME) & fips.county.sub$COUNTYFP %in% county,
                ]
        } else {
            fips.county.sub <- fips.county.sub[fips.county.sub$COUSUBFP %in% county.subdivision &
                fips.county.sub$COUNTYFP %in% county, ]
        }
        county.subdivision <- fips.county.sub[, 5]
        subdivision.name <- fips.county.sub[, 6]
        this.county <- fips.county.sub[, 3]
        this.county.name <- fips.county.sub[, 4]
        if (length(county.subdivision) > 0) {
            results <- rbind.fill(results, data.frame(state, state.name, county = this.county,
                county.name = this.county.name, county.subdivision, county.subdivision.name = subdivision.name,
                stringsAsFactors = FALSE))
        }
    }
    # check place
    if (!missing(place)) {
        fips.place <- fips.place[fips.place$STATEFP == state, ]
        if (is.character(place)) {
            fips.place <- fips.place[grepl(paste(place, collapse = "|"), fips.place$PLACENAME),
                ]
        } else fips.place <- fips.place[fips.place$PLACEFP %in% place, ]
        place <- fips.place[, 3]
        place.name <- fips.place[, 4]
        this.county.name <- fips.place[, 7]
        if (length(place) > 0) {
            results <- rbind.fill(results, data.frame(state, state.name, county.name = this.county.name,
                place, place.name, stringsAsFactors = FALSE))
        }
    }
    # check schools elementary
    if (!missing(school.district.elementary)) {
        fips.school.elementary <- fips.school[fips.school$STATEFP == state & fips.school$TYPE ==
            "Elementary", ]
        if (is.character(school.district.elementary)) {
            fips.school.elementary <- fips.school.elementary[grepl(paste(school.district.elementary,
                collapse = "|"), fips.school.elementary$SDNAME), ]
        } else fips.school.elementary <- fips.school.elementary[fips.school.elementary$LEA %in%
            school.district.elementary, ]
        school.district.elementary <- fips.school.elementary[, 3]  # fips code
        school.district.elementary.name <- fips.school.elementary[, 4]  # name
        school.district.elementary.type <- fips.school.elementary[, 5]  # type (elem, secondary, unified)
        if (length(school.district.elementary) > 0) {
            results <- rbind.fill(results, data.frame(state, state.name, school.district.elementary,
                school.district.elementary.name, school.district.elementary.type,
                stringsAsFactors = FALSE))
        }
    }
    ## secondary
    if (!missing(school.district.secondary)) {
        fips.school.secondary <- fips.school[fips.school$STATEFP == state & fips.school$TYPE ==
            "Secondary", ]
        if (is.character(school.district.secondary)) {
            fips.school.secondary <- fips.school.secondary[grepl(paste(school.district.secondary,
                collapse = "|"), fips.school.secondary$SDNAME), ]
        } else fips.school.secondary <- fips.school.secondary[fips.school.secondary$LEA %in%
            school.district.secondary, ]
        school.district.secondary <- fips.school.secondary[, 3]  # fips code
        school.district.secondary.name <- fips.school.secondary[, 4]  # name
        school.district.secondary.type <- fips.school.secondary[, 5]  # type (elem, secondary, unified)
        if (length(school.district.secondary) > 0) {
            results <- rbind.fill(results, data.frame(state, state.name, school.district.secondary,
                school.district.secondary.name, school.district.secondary.type, stringsAsFactors = FALSE))
        }
    }
    ## unified
    if (!missing(school.district.unified)) {
        fips.school.unified <- fips.school[fips.school$STATEFP == state & fips.school$TYPE ==
            "Unified", ]
        if (is.character(school.district.unified)) {
            fips.school.unified <- fips.school.unified[grepl(paste(school.district.unified,
                collapse = "|"), fips.school.unified$SDNAME), ]
        } else fips.school.unified <- fips.school.unified[fips.school.unified$LEA %in%
            school.district.unified, ]
        school.district.unified <- fips.school.unified[, 3]  # fips code
        school.district.unified.name <- fips.school.unified[, 4]  # name
        school.district.unified.type <- fips.school.unified[, 5]  # type (elem, secondary, unified)
        if (length(school.district.unified) > 0) {
            results <- rbind.fill(results, data.frame(state, state.name, school.district.unified,
                school.district.unified.name, school.district.unified.type, stringsAsFactors = FALSE))
        }
    }
    ## any type
    if (!missing(school.district)) {
        fips.school.any <- fips.school[fips.school$STATEFP == state, ]
        if (is.character(school.district)) {
            fips.school.any <- fips.school.any[grepl(paste(school.district, collapse = "|"),
                fips.school.any$SDNAME), ]
        } else fips.school.any <- fips.school.any[fips.school.any$LEA %in% school.district,
            ]
        school.district <- fips.school.any[, 3]  # fips code
        school.district.name <- fips.school.any[, 4]  # name
        school.district.type <- fips.school.any[, 5]  # type (elem, secondary, unified)
        if (length(school.district) > 0) {
            results <- rbind.fill(results, data.frame(state, state.name, school.district,
                school.district.name, school.district.type, stringsAsFactors = FALSE))
        }
    }
    results
}
