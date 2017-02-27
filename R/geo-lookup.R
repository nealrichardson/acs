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
        results <- data.frame(american.indian.area, american.indian.area.name, stringsAsFactors = F)
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
    results <- data.frame(state = state, state.name = state.name, stringsAsFactors = F)
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
                county.name, stringsAsFactors = F))
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
                stringsAsFactors = F))
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
                stringsAsFactors = F))
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
                place, place.name, stringsAsFactors = F))
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
                stringsAsFactors = F))
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
                school.district.secondary.name, school.district.secondary.type, stringsAsFactors = F))
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
                school.district.unified.name, school.district.unified.type, stringsAsFactors = F))
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
                school.district.name, school.district.type, stringsAsFactors = F))
        }
    }
    results
}
