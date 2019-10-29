# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "Biomass_fuels",
  description = "SpaDES version of the LANDIS-II Dynamic Biomass Fuels Extention v2.2 - 15 Jun 2017", #"insert module description here",
  keywords = c("fire fuels", "fuel type", "LANDIS", "LandR"),
  authors = person("Ceres", "Barros", email = "cbarros@mail.ubc.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Biomass_fuels.Rmd"),
  reqdPkgs = list("data.table", "dplyr", "sn",
                  "PredictiveEcology/SpaDES.core@development",
                  "PredictiveEcology/SpaDES.tools@development",
                  "PredictiveEcology/reproducible@development"),
  parameters = rbind(
    defineParameter("fireInitialTime", "numeric", NA,
                    desc = "The event time that the first fire disturbance event occurs"),
    defineParameter("fireTimestep", "numeric", NA,
                    desc = "The number of time units between successive fire events in a fire module"),
    defineParameter(name = "hardwoodMax", class = "integer", default = 15L,
                    desc = "Threshold of percent biomass below which fuel types are considered conifer or mixed.
                    Defaults to 15, as in LANDIS example file"),
    defineParameter(name = "nonForestFire", class = "logical", default = FALSE,
                    desc = paste("Determines whether fire fuels will be calculated for non-forest pixels.",
                                 "If TRUE, the user should provide fuels table ('nonForestFuelsTable') for non forested",
                                 "land cover classes in accordance to the classes in 'rstLCC'")),
    defineParameter("sppEquivCol", "character", "Boreal", NA, NA,
                    "The column in sim$specieEquivalency data.table to use as a naming convention"),
    defineParameter(".useCache", "logical", "init", NA, NA,
                    desc = "use caching for the spinup simulation?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                 succession time step", sourceURL = NA),
    expectsInput(objectName = "ForestFuelTypes", objectClass = "data.table",
                 desc = paste("Table of Fuel Type parameters, with  base fuel type, species (in LANDIS code),",
                              "their - or + contribution ('negSwitch'), min and max age for each species (see LANDIS-II",
                              "Dynamic Fire System Extension (v2.1) User Guide). Fuel types come from CF Fire Behaviour",
                              "Prediction System (2nd Ed.). Default values adapted from",
                              "https://raw.githubusercontent.com/CeresBarros/Extension-Dynamic-Biomass-Fuels/master/testings/version-tests/v6.0-2.0/dynamic-biomass-fuels.txt")),
    expectsInput(objectName = "fTypeEcoreg", objectClass = "data.table",
                 desc = paste("Table of Fuel Types per Ecoregion (optional, see LANDIS-II Dynamic Fire System",
                              "Extension (v2.1) User Guide). Fuel types come from CF Fire Behaviour Prediction System (2nd Ed.)",
                              "Default values adapted from",
                              "https://raw.githubusercontent.com/CeresBarros/Extension-Dynamic-Biomass-Fuels/master/testings/version-tests/v6.0-2.0/dynamic-biomass-fuels.txt")),
    expectsInput(objectName = "nonForestFuelsTable", objectClass = "data.table",
                 desc = paste("Table of correspondence between non-forested land-cover classes and fire fuels.",
                              "Fuel types come from CF Fire Behaviour Prediction System (2nd Ed.). Default values",
                              "use the LCC2005 land-cover product, and consider only grasslands and shurblands.")),
    expectsInput("rasterToMatch", "RasterLayer",
                 desc = "a raster of the studyArea in the same resolution and projection as biomassMap",
                 sourceURL = NA),
    expectsInput("rstLCC", "RasterLayer",
                 desc = paste("A land classification map in study area. It must be 'corrected', in the sense that:\n",
                              "1) Every class must not conflict with any other map in this module\n",
                              "    (e.g., speciesLayers should not have data in LCC classes that are non-treed);\n",
                              "2) It can have treed and non-treed classes. The non-treed will be removed within this\n",
                              "    module if P(sim)$omitNonTreedPixels is TRUE;\n",
                              "3) It can have transient pixels, such as 'young fire'. These will be converted to a\n",
                              "    the nearest non-transient class, probabilistically if there is more than 1 nearest\n",
                              "    neighbour class, based on P(sim)$LCCClassesToReplaceNN.\n",
                              "The default layer used, if not supplied, is Canada national land classification in 2005"),
                 sourceURL = "https://drive.google.com/file/d/1g9jr0VrQxqxGjZ4ckF6ZkSMP-zuYzHQC/view?usp=sharing"),
    expectsInput(objectName = "rstLCCRTM", objectClass = "RasterLayer",
                 desc = "Same as rstLCC, but masked to rasterToMatch"),
    expectsInput(objectName = "sppEquiv", objectClass = "data.table",
                 desc = "table of species equivalencies. See LandR::sppEquivalencies_CA.",
                 sourceURL = ""),
    expectsInput(objectName = "sppMultipliers", objectClass = "data.table",
                 desc = paste("Table of species biomass coefficient weights.",
                              "Recommended to be close to 1.0 for all species (see LANDIS-II Dynamic Fire System Extension",
                              "(v2.1) User Guide). Default values adapted from ",
                              "https://raw.githubusercontent.com/CeresBarros/Extension-Dynamic-Biomass-Fuels/master/testings/version-tests/v6.0-2.0/dynamic-biomass-fuels.txt"))
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "fuelTypesMaps", objectClass = "list",
                  desc = "List of RasterLayers of fuel types and coniferDominance per pixel."),
    createsOutput(objectName = "pixelNonForestFuels", objectClass = "data.table",
                  desc = paste("Table of non forest fuel attributes (pixel ID, land cover, fuel type",
                               "name and code, and degree of curing) for each pixel with non-forest fuels")),
    createsOutput(objectName = "rstLCCRTM", objectClass = "RasterLayer",
                  desc = "Same as rstLCC, but masked to rasterToMatch")
  )
))

doEvent.Biomass_fuels = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- fuelsInit(sim)

      sim <- scheduleEvent(sim, P(sim)$fireInitialTime,
                           "Biomass_fuels", "doFuelTypes", eventPriority = 1)
    },

    doFuelTypes = {
      # do stuff for this event
      sim <- calcFuelTypes(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep,
                           "Biomass_fuels", "doFuelTypes",  eventPriority = 1)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### template initialization
fuelsInit <- function(sim) {
  ## checks
  if (start(sim) == P(sim)$fireInitialTime)
    warning(red("start(sim) and P(sim)$fireInitialTime are the same.\nThis may create bad scheduling with init events"))

  return(invisible(sim))
}

calcFuelTypes <- function(sim) {
  ## PIXEL FUEL TYPES TABLE ------------------------
  ## create pixelFuelTypes table from cohorData
  ## subset cohort data and non-na fuel types
  pixelFuelTypes <- sim$cohortData[, .(speciesCode, pixelGroup, ecoregionGroup, age, B)]
  tempFT <- na.omit(copy(sim$ForestFuelTypes[, -c("FuelTypeDesc"), with = FALSE]))  ## keep only complete lines with spp codes

  ## merge the two tables
  pixelFuelTypes <- pixelFuelTypes[tempFT, on = .(speciesCode), allow.cartesian = TRUE,
                                   nomatch = NA]
  if (isTRUE(getOption("LandR.assertions"))) {
    cols <- c("FuelTypeFBP", "FuelType", "BaseFuel", "minAge",
              "maxAge", "speciesCode", "negSwitch")
    if (any(is.na(pixelFuelTypes[, ..cols]))) {
      stop("speciesCodes do not match between cohortData and FuelTypes table.")
    }
  }

  ## add sppMultipliers
  pixelFuelTypes <- pixelFuelTypes[sim$sppMultipliers[,.(speciesCode, Coefficient)],
                                   on = .(speciesCode), nomatch = NA]
  if (isTRUE(getOption("LandR.assertions"))) {
    cols <- c("Coefficient")
    if (any(is.na(pixelFuelTypes[, ..cols]))) {
      stop("speciesCodes do not match between cohortData and sppMultipliers table.")
    }
  }


  ## add fuel type ecoregions and remove fuel types in the wrong regions.
  origPGs <- unique(pixelFuelTypes$pixelGroup)
  pixelFuelTypes <- pixelFuelTypes[sim$fTypeEcoreg, on = .(FuelType), nomatch = NA]
  pixelFuelTypes <- pixelFuelTypes[, ftEcoregion := as.numeric(Ecoregions)]
  pixelFuelTypes <- pixelFuelTypes[, Ecoregions := NULL]

  if (isTRUE(getOption("LandR.assertions"))) {
    cols <- c("ftEcoregion")
    if (any(is.na(pixelFuelTypes[, ..cols]))) {
      warning("Some fuel types have no ecoregion in the fTypeEcoreg table (or are not listed there)")
    }
  }

  ## CHECK FUEL TYPES AND ECOREGIONS ------------------------------
  ## if fuel types are ecoregion-specific, remove fuel types
  ## that are in wrong ecoregion
  if (any(!is.na(pixelFuelTypes$ftEcoregion))) {
    subsetDT <- pixelFuelTypes[!is.na(ftEcoregion)]
    subsetDT <- subsetDT[ftEcoregion == ecoregionGroup]
    subsetDT <- rbind(pixelFuelTypes[is.na(ftEcoregion)], subsetDT)
    pixelFuelTypes <- subsetDT
  }

  ## CALCULATE SPP VALUES FOR EACH FUEL TYPE IN EACH PIXEL ------------------------------
  ## calculate total biomass per pixelGroup and fuel type
  ## (not necessary to group by ecoregion, since only one exists per pixelGroup)
  ## only species contributing to a given fuel type and with appropriate age are considered
  ## species biomass is weighted by the coeff and becomes
  ## negative if the species has a negative contribution (negSwitch) to that fuel type

  cols <- c("pixelGroup", "FuelType")
  pixelFuelTypes <- pixelFuelTypes[age >= minAge & age <= maxAge,
                                   forTypValue := sum(B*Coefficient*negSwitch),
                                   by = cols]
  ## remove lines that have no biomass for a fuel
  pixelFuelTypes <- pixelFuelTypes[!is.na(forTypValue)]

  ## ASSESS DOMINANT FUEL TYPE ----------------------------------------
  ## get max spp value (total biomass) in each pixelGroup and
  ## attribute corresponding fuel type in function of conifer/deciduous biomass
  ## (not necessary to group by ecoregion, since only one exists per pixelGroup)
  pixelFuelTypes[, maxValue := max(forTypValue, na.rm = TRUE),
                 by = "pixelGroup"]
  pixelFuelTypes <- calcFinalFuels(pixelFuelTypes = pixelFuelTypes,
                            hardwoodMax = P(sim)$hardwoodMax)

  ## rasterize forests fuel types table
  fuelTypesMaps <- rasterizeReduced(pixelGroupFuelTypes, sim$pixelGroupMap,
                                    newRasterCols = c("finalFuelType" , "coniferDom"),
                                    mapcode = "pixelGroup")

  ## ADD FUEL TYPES IN NON-FORESTED PIXELS ---------------------------
  if (P(sim)$nonForestFire) {
    nonForestPix <- which(sim$rstLCCRTM[] %in% sim$nonForestFuelsTable$LC)

    if (any(!is.na(fuelTypesMaps$finalFuelType[nonForestPix])))
      stop(paste("Either some pixelGroups correspond to non forest classes",
                 "in 'nonForestFuelsTable', or some of the clases in this table are forested"))

    if (!compareRaster(sim$rstLCCRTM, fuelTypesMaps$finalFuelType,
                       values = FALSE, stopiffalse = FALSE))
      stop("'rstLCCRTM' does not match 'fuelTypesMaps' rasters")

    # fuelTypesMaps$finalFuefuelTypesMaps[nonForestPix] <- sim$rstLCCRTM[nonForestPix]

    LC2FuelsTable <- data.table(LC = getValues(sim$rstLCCRTM)[nonForestPix],
                                pixID = nonForestPix)
    LC2FuelsTable <- sim$nonForestFuelsTable[LC2FuelsTable, on = "LC"]

    ## for fuel types with varying curing degree, using the a skewned normal with mean defined by
    ## the parameters in the table. SD/omega and alpha (i.e. skewnness) fixed to 10
    ## plot(0:100, dsn(c(0:100), xi = 60, omega = 10, alpha = 10), type = "l")
    ## then bound the values to defined min/max values
    LC2FuelsTable[which(!fixedCuring),
                  finalCuring := rsn(1, xi = curingMean, omega = 10, alpha = 10),
                  by = seq_len(nrow(LC2FuelsTable[which(!fixedCuring)]))]
    LC2FuelsTable[which(!fixedCuring), finalCuring := min(finalCuring, curingMax),
                  by = seq_len(nrow(LC2FuelsTable[which(!fixedCuring)]))]
    LC2FuelsTable[which(!fixedCuring), finalCuring := max(finalCuring, curingMin),
                  by = seq_len(nrow(LC2FuelsTable[which(!fixedCuring)]))]

    LC2FuelsTable[which(fixedCuring), finalCuring := curingMean]


    ## add non-forest fuels to map and attribute 0 conifer dominance values
    fuelTypesMaps$finalFuelType[LC2FuelsTable$pixID] <- as.numeric(LC2FuelsTable$FuelType)
    fuelTypesMaps$coniferDom[LC2FuelsTable$pixID] <- 0

    ## add make rasters of curing, etc due to projections (pixIds are not trackable)
    fuelTypesMaps$curing <- sim$rstLCCRTM
    fuelTypesMaps$curing[] <- NA
    fuelTypesMaps$curing[LC2FuelsTable$pixID] <-  LC2FuelsTable$finalCuring

    ## export to sim
    sim$pixelNonForestFuels <- LC2FuelsTable[, .(pixID, FuelTypeFBP, FuelType, finalCuring)]
  } else {
    ## if not running fires in non-forested pixel export an empty object
    sim$pixelNonForestFuels <- NULL
    fuelTypesMaps$curing <- fuelTypesMaps$finalFuelType
    fuelTypesMaps$curing[] <- NA
  }

  ## export to sim
  sim$fuelTypesMaps <- fuelTypesMaps

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  if (getOption("LandR.verbose", TRUE) > 0)
    message(currentModule(sim), ": using dataPath '", dPath, "'.")

  #######################################################

  ## SPECIES EQUIVALENCY TABLE ---------------------------
  if (!suppliedElsewhere("sppEquiv", sim)) {
    if (!is.null(sim$sppColorVect))
      stop("If you provide sppColorVect, you MUST also provide sppEquiv")

    data("sppEquivalencies_CA", package = "LandR", envir = environment())
    sim$sppEquiv <- as.data.table(sppEquivalencies_CA)

    ## By default, Abies_las is renamed to Abies_sp
    sim$sppEquiv[KNN == "Abie_Las", LandR := "Abie_sp"]
  }

  ## Get LANDIS example parameters -----------------------
  ## to use if others haven't been supplied in <module>/inputs
  if (any(!suppliedElsewhere("sppMultipliers", sim),
          !suppliedElsewhere("ForestFuelTypes", sim),
          !suppliedElsewhere("fTypeEcoreg", sim)) &
      any(!file.exists(file.path(dPath, "sppMultipliers.csv")),
          file.exists(file.path(dPath, "ForestFuelTypes.csv")))) {
    maxcol <- 21 #max(count.fields(file.path(getPaths()$dataPath, "dynamic-biomass-fuels.txt"), sep = ""))
    dynamicBiomassFuels <- Cache(prepInputs,targetFile = "dynamic-biomass-fuels.txt",
                                 url = paste0("https://raw.githubusercontent.com/CeresBarros/",
                                              "Extension-Dynamic-Biomass-Fuels/master/testings/",
                                              "version-tests/v6.0-2.0/dynamic-biomass-fuels.txt"),
                                 destinationPath = dPath,
                                 fun = "utils::read.table",
                                 fill = TRUE, row.names = NULL,
                                 sep = "",
                                 header = FALSE,
                                 blank.lines.skip = TRUE,
                                 col.names = c(paste("col",1:maxcol, sep = "")),
                                 stringsAsFactors = FALSE)
    dynamicBiomassFuels <- data.table(dynamicBiomassFuels[, 1:15])
    dynamicBiomassFuels <- dynamicBiomassFuels[!(col1 %in% c("LandisData", "Timestep", "MapFileNames",
                                                             "PctConiferFileName", "PctDeadFirFileName"))]
    dynamicBiomassFuels <- dynamicBiomassFuels[!(col2 %in% c("The", "Users", "Optional"))]
    dynamicBiomassFuels <- dynamicBiomassFuels[!(col1 == ">>" & grepl("---", col2))]
    dynamicBiomassFuels[col1 == ">>"] <- data.table(dynamicBiomassFuels[col1 == ">>", col2:col15], col15 = "NA")
    dynamicBiomassFuels[,col15:=NULL]
    dynamicBiomassFuels[col1 == "Fuel" & col2 == "Type"] <- data.table(dynamicBiomassFuels[col1 == "Fuel" & col2 == "Type", col2:col14],
                                                                       col14 = "NA")
    dynamicBiomassFuels[col1 == "Type", col1 := "FuelType"]
  }

  ## SPECIES COEFFICIENTS ---------------------------------
  if (!suppliedElsewhere("sppMultipliers", sim)) {
    if (file.exists(file.path(dPath, "sppMultipliers.csv"))) {
      sppMultipliers <- prepInputs(targetFile = "sppMultipliers.csv",
                                   destinationPath = dPath,
                                   fun = "read.csv",
                                   header = TRUE)
      sppMultipliers <- data.table(sppMultipliers)

      ## make sure columns are the right types
      sppMultipliers[, Coefficient := as.numeric(Coefficient)]
    } else {
      message(paste0("Can't find sppMultipliers.csv in ", dPath,
                     ".\nUsing LANDIS example file"))

      sppMultipliers <- dynamicBiomassFuels[(which(col1=="Fuel") + 1) : (which(col1 == "HardwoodMaximum") - 1),
                                            col1:col2]

      names(sppMultipliers) <- as.character(sppMultipliers[1,])
      sppMultipliers <- sppMultipliers[-1]
      sppMultipliers[, Coefficient := as.numeric(Coefficient)]

      # sim$sppMultipliers <- copy(sppMultipliers)
    }
    sppMultipliers <- prepSppMultipliers(sppMultipliers,
                                         sppEquiv = sim$sppEquiv,
                                         sppEquivCol = P(sim)$sppEquivCol)
    sim$sppMultipliers <- sppMultipliers
  }

  ## FUEL TYPES TABLE -------------------------------------
  if (!suppliedElsewhere("ForestFuelTypes", sim)) {
    if (file.exists(file.path(dPath, "ForestFuelTypes.csv"))) {
      ForestFuelTypes <- prepInputs(targetFile = "ForestFuelTypes.csv",
                              destinationPath = dPath,
                              fun = "utils::read.csv",
                              header = TRUE)
      ForestFuelTypes <- data.table(ForestFuelTypes)
    } else {
      message(paste0("Can't find ForestFuelTypes.csv in ", dPath,
                     ".\nUsing LANDIS example file"))

      ForestFuelTypes <- dynamicBiomassFuels[(which(col1=="FuelTypes") + 1) : (which(col1 == ">>EcoregionsTable") - 1),
                                       col1:col14]
      ## rename columns
      ForestFuelTypes[1, `:=`(col3 = "minAge",
                        col4 = "NA",
                        col5 = "maxAge",
                        col6 = "Species")]
      ForestFuelTypes[, col4 := NULL]
      names(ForestFuelTypes) <- c(as.character(ForestFuelTypes[1, 1:5, with = FALSE]), paste0("Species", 2:9))
      ForestFuelTypes <- ForestFuelTypes[-1]

      ## melt table to get all species in one column
      ForestFuelTypes <- melt(ForestFuelTypes, id.vars = 1:4, variable.name = "Species")
      ForestFuelTypes <-   ForestFuelTypes[value != ""]
      setnames(ForestFuelTypes, "value", "Species")

      ## create a column with negative switch
      ## species with a negative switch negatively contribute to a fuel type
      ForestFuelTypes[grepl("-", Species), negSwitch := -1L]
      ForestFuelTypes[is.na(negSwitch), negSwitch := 1]
      ForestFuelTypes[, Species := sub("-", "", Species)]
    }
    ForestFuelTypes <- prepFuelTypes(ForestFuelTypes,
                               sppEquiv = sim$sppEquiv,
                               sppEquivCol = P(sim)$sppEquivCol)

    sim$ForestFuelTypes <- ForestFuelTypes
  }

  ## FUEL TYPES AND ECOREGIONS TABLE ----------------------
  if (!suppliedElsewhere("fTypeEcoreg", sim)) {
    if (file.exists(file.path(dPath, "fTypesEcoregions.csv"))) {
      fTypeEcoreg <- prepInputs(targetFile = "fTypesEcoregions.csv",
                                destinationPath = dPath,
                                fun = "utils::read.csv",
                                header = TRUE,
                                userTags = cacheTags)
      fTypeEcoreg <- data.table(fTypeEcoreg)
    } else {
      message(paste0("Can't find fTypesEcoregions.csv in ", dPath,
                     ".\nAssigning NAs to ecoregions in sim$fTypeEcoreg"))
      ## in the LANDIS test examples this feature is turned off and the
      ## ecoregions do not correspond to those in the succession module examples.
      ## assigning NAs for ecoregions in all fuel types to simualte this behaviour
      fTypeEcoreg <- data.table(FuelType = sort(unique(sim$ForestFuelTypes$FuelType)), Ecoregions = NA)
    }

    ## assign NAs in fuel types with no ecoregion
    fTypeEcoreg <- fTypeEcoreg[sim$ForestFuelTypes[, .(FuelType)], on = "FuelType", nomatch = NA]

    fTypeEcoreg <- unique(fTypeEcoreg)
    sim$fTypeEcoreg <- fTypeEcoreg
  }

  ## FUEL TYPES FOR NON-FOREST LAND-COVER CLASSES ------
  if (P(sim)$nonForestFire) {
    if (!suppliedElsewhere("nonForestFuelsTable", sim)) {
      ## for non forest fuels classified as open vegetation/grassland (O1, O2)
      ## the decree of curing needs to be defined, and whether it is fixed (only mean necessary)
      ## or drawn from a distribution (mean, min and max required)
      ## if drawn from a distribution, a normal distribution with right-side fat tail will be used (Perrakis, pers. comm.)
      ## mean, min and max values from Perrakis (pers. comm.)
      sim$nonForestFuelsTable <- data.table(LC = c(16, 17, 21, 22, 23, 24, 25),
                                            FuelTypeFBP = c("O1b", "O1b", "O1b", "O1b", "O1b", "O1b", "NF"),
                                            FuelType = c(15, 15, 15, 15, 15, 15, 19),
                                            fixedCuring = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, NA),
                                            curingMean = c(60, 60, 60, 60, 35, 30, NA),
                                            curingMin = c(50, 50, 50, 50, 0, 30, NA),
                                            curingMax = c(80, 80, 80, 80, 60, 30, NA))
    }
  }

  ## LAND COVER RASTERS ----------------------------------
  if (!suppliedElsewhere("rstLCCRTM", sim)) {
    if (!suppliedElsewhere("rstLCC", sim)) {

      if (!suppliedElsewhere("studyArea", sim)) {
        message("'studyArea' was not provided by user. Using a polygon (6250000 m^2) in southwestern Alberta, Canada")
        sim$studyArea <- randomStudyArea(seed = 1234, size = (250^2)*100)
      }

      if (!suppliedElsewhere("studyAreaLarge", sim)) {
        message("'studyAreaLarge' was not provided by user. Using the same as 'studyArea'")
        sim <- objectSynonyms(sim, list(c("studyAreaLarge", "studyArea")))
      }

      if (!identical(crs(sim$studyArea), crs(sim$studyAreaLarge))) {
        warning("studyArea and studyAreaLarge have different projections.\n
            studyAreaLarge will be projected to match crs(studyArea)")
        sim$studyAreaLarge <- spTransform(sim$studyAreaLarge, crs(sim$studyArea))
      }

      ## check whether SA is within SALarge
      ## convert to temp sf objects
      studyArea <- st_as_sf(sim$studyArea)
      studyAreaLarge <- st_as_sf(sim$studyAreaLarge)

      if (!st_within(studyArea, studyAreaLarge)[[1]])
        stop("studyArea is not fully within studyAreaLarge.
           Please check the aligment, projection and shapes of these polygons")
      rm(studyArea, studyAreaLarge)

      ## Raster(s) to match ------------------------------------------------
      needRTM <- FALSE
      if (is.null(sim$rasterToMatch) || is.null(sim$rasterToMatchLarge)) {
        if (!suppliedElsewhere("rasterToMatch", sim) ||
            !suppliedElsewhere("rasterToMatchLarge", sim)) {      ## if one is not provided, re do both (safer?)
          needRTM <- TRUE
          message("There is no rasterToMatch/rasterToMatchLarge supplied; will attempt to use rawBiomassMap")
        } else {
          stop("rasterToMatch/rasterToMatchLarge is going to be supplied, but ", currentModule(sim), " requires it ",
               "as part of its .inputObjects. Please make it accessible to ", currentModule(sim),
               " in the .inputObjects by passing it in as an object in simInit(objects = list(rasterToMatch = aRaster)",
               " or in a module that gets loaded prior to ", currentModule(sim))
        }
      }

      if (needRTM) {
        if (!suppliedElsewhere("rawBiomassMap", sim)) {
          sim$rawBiomassMap <- Cache(prepInputs,
                                     targetFile = asPath(basename(rawBiomassMapFilename)),
                                     archive = asPath(c("kNN-StructureBiomass.tar",
                                                        "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.zip")),
                                     url = extractURL("rawBiomassMap"),
                                     destinationPath = dPath,
                                     studyArea = sim$studyAreaLarge,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
                                     # studyArea = sim$studyArea,
                                     rasterToMatch = if (!needRTM) sim$rasterToMatchLarge else NULL,
                                     # maskWithRTM = TRUE,    ## if RTM not supplied no masking happens (is this intended?)
                                     maskWithRTM = if (!needRTM) TRUE else FALSE,
                                     ## TODO: if RTM is not needed use SA CRS? -> this is not correct
                                     # useSAcrs = if (!needRTM) TRUE else FALSE,
                                     useSAcrs = FALSE,     ## never use SA CRS
                                     method = "bilinear",
                                     datatype = "INT2U",
                                     filename2 = TRUE, overwrite = TRUE,
                                     userTags = cacheTags,
                                     omitArgs = c("destinationPath", "targetFile", "userTags", "stable"))
        }
        ## if we need rasterToMatch/rasterToMatchLarge, that means a) we don't have it, but b) we will have rawBiomassMap
        ## even if one of the rasterToMatch is present re-do both.

        if (is.null(sim$rasterToMatch) != is.null(sim$rasterToMatchLarge))
          warning(paste0("One of rasterToMatch/rasterToMatchLarge is missing. Both will be created \n",
                         "from rawBiomassMap and studyArea/studyAreaLarge.\n
              If this is wrong, provide both rasters"))

        sim$rasterToMatchLarge <- sim$rawBiomassMap
        RTMvals <- getValues(sim$rasterToMatchLarge)
        sim$rasterToMatchLarge[!is.na(RTMvals)] <- 1

        sim$rasterToMatchLarge <- Cache(writeOutputs, sim$rasterToMatchLarge,
                                        filename2 = file.path(cachePath(sim), "rasters", "rasterToMatchLarge.tif"),
                                        datatype = "INT2U", overwrite = TRUE,
                                        userTags = cacheTags,
                                        omitArgs = c("userTags"))

        sim$rasterToMatch <- Cache(postProcess,
                                   x = sim$rawBiomassMap,
                                   studyArea = sim$studyArea,
                                   rasterToMatch = sim$rasterToMatchLarge,
                                   useSAcrs = FALSE,
                                   maskWithRTM = FALSE,   ## mask with SA
                                   method = "bilinear",
                                   datatype = "INT2U",
                                   filename2 = file.path(cachePath(sim), "rasterToMatch.tif"),
                                   overwrite = TRUE,
                                   userTags = cacheTags,
                                   omitArgs = c("destinationPath", "targetFile", "userTags", "stable"))

        ## covert to 'mask'
        RTMvals <- getValues(sim$rasterToMatch)
        sim$rasterToMatch[!is.na(RTMvals)] <- 1
      }

      sim$rstLCC <- Cache(prepInputs,
                          targetFile = lcc2005Filename,
                          archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
                          url = extractURL("rstLCC"),
                          destinationPath = dPath,
                          studyArea = sim$studyAreaLarge,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
                          # studyArea = sim$studyArea,
                          rasterToMatch = sim$rasterToMatchLarge,
                          # rasterToMatch = sim$rasterToMatch,
                          maskWithRTM = TRUE,
                          method = "bilinear",
                          datatype = "INT2U",
                          filename2 = FALSE, overwrite = TRUE,
                          userTags = c("prepInputsrstLCC_rtm", cacheTags), # use at least 1 unique userTag
                          omitArgs = c("destinationPath", "targetFile", "userTags"))
    }

    rstLCCRTM <- sim$rstLCC

    if (!compareRaster(rstLCCRTM, sim$rasterToMatch, values = FALSE, stopiffalse = FALSE)) {
      rstLCCRTM <- Cache(postProcess,
                         x = rstLCCRTM,
                         rasterToMatch = sim$rasterToMatch,
                         maskWithRTM = TRUE,
                         filename2 = NULL,
                         userTags = cacheTags,
                         omitArgs = "userTags")
    }

    sim$rstLCCRTM <- rstLCCRTM
  }

  return(invisible(sim))
}
