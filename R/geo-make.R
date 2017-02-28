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
