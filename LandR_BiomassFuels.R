# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "LandR_BiomassFuels",
  description = "SpaDES version of the LANDIS-II Dynamic Biomass Fuels Extention", #"insert module description here",
  keywords = c("fire fuels", "fuel type", "LANDIS", "LandR"), 
  authors = person("Ceres", "Barros", email = "cbarros@mail.ubc.ca", 
                   role = c("aut", "cre")),
  childModules = character(0),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandR_BiomassFuels.Rmd"),
  reqdPkgs = list("data.table", "dplyr",
                  "PredictiveEcology/SpaDES.core@development",
                  "PredictiveEcology/SpaDES.tools@development",
                  "CeresBarros/reproducible@development"),
  parameters = rbind(
    defineParameter(name = "hardwoodMax", class = "numeric", default = 0L, 
                    desc = "Threshold of percent biomass below which fuel types are considered conifer or mixed")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "speciesList", objectClass = c("character", "matrix"),
                 desc = "vector or matrix of species to select, provided by the user or BiomassSpeciesData. 
                 If a matrix, should have two columns of raw and 'end' species names", sourceURL = NA),
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                 succession time step", sourceURL = NA),
    expectsInput(objectName = "species", objectClass = "data.table",
                 desc = "Species table produced by LandR-Biomass that has species traits such as longevity...",
                 sourceURL = NA),
    expectsInput(objectName = "speciesTable", objectClass = "data.table",
                 desc = "species attributes table, coming from LandR or a LandR data prep module",
                 sourceURL = NA),
    expectsInput(objectName = "dynamicBiomassFuels", objectClass = "data.table",
                 desc = "Test parameter file from LANDIS-II Dynamic Biomass Fuels Extension", 
                 sourceURL = "https://raw.githubusercontent.com/CeresBarros/Extension-Dynamic-Biomass-Fuels/master/testings/version-tests/v6.0-2.0/dynamic-biomass-fuels.txt")
    ),
  outputObjects = bind_rows(
    createsOutput(objectName = "speciesNames", objectClass = "data.table",
                  desc = "Table of species names/codes correspondences between LANDIS and LandR"),
    createsOutput(objectName = "sppMultipliers", objectClass = "data.table",
                  desc = "Table of species biomass coefficient weights.
                  Recommended to be close to 1.0 for all species (see LANDIS-II Dynamic Fire System Extension (v2.1) User Guide"),
    createsOutput(objectName = "FuelTypes", objectClass = "data.table",
                  desc = "Table of Fuel Type parameters, with  base fuel type, species (in LANDIS code), their - or + contribution ('negSwitch'),
                  min and max age for each species"),
    createsOutput(objectName = "pixelFuelTypes", objectClass = "data.table", 
                  desc = "Fuel types per pixel group, calculated from cohort biomasses")
    )
  ))

doEvent.LandR_BiomassFuels = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- fuelsInit(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim) + sim@params$LBMR$successionTimestep,
                           "LandR_BiomassFuels", "doPrepareTables", eventPriority = 1)
      sim <- scheduleEvent(sim, start(sim) + sim@params$LBMR$successionTimestep,
                           "LandR_BiomassFuels", "doFuelTypes", eventPriority = 1.5)
    },
    doPrepareTables = {
      # do stuff for this event
      sim <- prepareTables(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + sim@params$LBMR$successionTimestep,
                           "LandR_BiomassFuels", "doPrepareTables", eventPriority = 1)
    },
    doFuelTypes = {
      # do stuff for this event
      sim <- calcFuelTypes(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + sim@params$LBMR$successionTimestep,
                           "LandR_BiomassFuels", "doFuelTypes",  eventPriority = 1.5)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### template initialization
fuelsInit <- function(sim) {
  ## this module doesn't require an initialisation as it's objects depend on other model outputs
  
  return(invisible(sim))
}

prepareTables <- function(sim) {
  ## SPECIES NAMES CORRESPONDENCES ------------------------
  speciesNames <- data.table(LANDISNames = unique(sim$speciesTable[,LandisCode]))
  speciesNames[, LANDISNames := tolower(gsub("\\.", "", LANDISNames))]
  speciesNames[, LANDISNames1:=as.character(substring(LANDISNames, 1, 4))]
  speciesNames[, LANDISNames2:=as.character(substring(LANDISNames, 5, 7))]
  speciesNames[LANDISNames2 == "spp" | LANDISNames2 == "all", LANDISNames2:="sp"]
  
  speciesNames[, ':='(LandRNames = paste0(toupper(substring(LANDISNames1, 1, 1)),
                                          tolower(as.character(substring(LANDISNames1, 2, 4))),
                                          "_", tolower(substring(LANDISNames2, 1, 1)),
                                          tolower(as.character(substring(LANDISNames2, 2, 3)))),
                      LANDISNames1 = NULL,
                      LANDISNames2 = NULL)]
  speciesNames$LANDISNames <- substring(speciesNames$LANDISNames, 1, 7)
  
  speciesList <- sim$speciesList
  rownames(speciesList) <- sapply(strsplit(speciesList[,1], "_"), function(x) {
    x[1] <- substring(x[1], 1, 4)
    x[2] <-  substring(x[2], 1, 3)
    paste(x, collapse = "_")
  }) 
  rownames(speciesList) <- sub("_spp", "_sp", rownames(speciesList))
  
  if(any(!tolower(rownames(speciesList)) %in% tolower(speciesNames$LandRNames)))
    warning(cat("\nFollowing selected species not found in the LANDIS species list.\nCheck if this is correct:\n",
                paste0(rownames(speciesList)[!tolower(rownames(speciesList)) %in%
                                               tolower(speciesNames$LandRNames)],
                       collapse = ", ")))
  
  
  ## append species codes
  rownames(speciesList) <- tolower(rownames(speciesList))
  matchNames <- tolower(speciesNames[tolower(LandRNames) %in% tolower(rownames(speciesList)), LandRNames])
  
  speciesNames[tolower(LandRNames) %in% rownames(speciesList), species := speciesList[matchNames,2]]
  speciesNames <- merge(speciesNames, sim$species[, c("species", "speciesCode"), with = FALSE],
                        by = "species", all.y = TRUE)
  
  sim$speciesNames <- copy(speciesNames)
  
  ## SPECIES COEFFICIENTS ---------------------------------
  if(!file.exists(file.path(inputPath(sim), "sppMultipliers.csv"))) {
    sppMultipliers <- sim$dynamicBiomassFuels[(which(col1=="Fuel") + 1) : (which(col1 == "HardwoodMaximum") - 1),
                                              col1:col2]
    
    names(sppMultipliers) <- as.character(sppMultipliers[1,])
    sppMultipliers <- sppMultipliers[-1]
    
    ## remove last character to match other tables
    sppMultipliers[, Species := substring(Species, 1, 7)]
    
    ## merge species names and codes
    ## keep all species, even thoe that do not match between fuels inputs and LandR tables
    sppMultipliers <- merge(speciesNames, sppMultipliers, by.x = "LANDISNames", by.y="Species", all = TRUE)
    sppMultipliers[, LANDISNames := NULL]
    sppMultipliers$Coefficient <- as.numeric(sppMultipliers$Coefficient)
    
    ## LANDIS advises keeping one for all species or values close to 1
    sppMultipliers[is.na(Coefficient), Coefficient := 1.0]
    
    ## exclude lines that have no spp codes
    sppMultipliers <- sppMultipliers[, sum(is.na(.SD)) < 3, by = 1:nrow(sppMultipliers), .SDcols = 1:3] %>%
      .$V1 %>%
      sppMultipliers[.]
    
    ## remove potential duplicates
    sppMultipliers <- sppMultipliers[!duplicated(sppMultipliers)]
    
  } else {
    sppMultipliers <- prepInputs(targetFile = "sppMultipliers.csv",
                                 destinationPath = inputPath(sim),
                                 fun = "read.csv", 
                                 header = TRUE) %>%
      data.table
  }
  
  sim$sppMultipliers <- copy(sppMultipliers)
  
  ## FUEL TYPES TABLE -------------------------------------
  if(!file.exists(file.path(inputPath(sim), "FuelTypes.csv"))) {
    FuelTypes <- sim$dynamicBiomassFuels[(which(col1=="FuelTypes") + 1) : (which(col1 == ">>EcoregionsTable") - 1),
                                         col1:col14]
    ## rename columns
    FuelTypes[1, `:=`(col3 = "minAge",
                      col4 = "NA",
                      col5 = "maxAge",
                      col6 = "Species")]
    FuelTypes[, col4 := NULL]
    names(FuelTypes) <- c(as.character(FuelTypes[1, 1:5, with = FALSE]), paste0("Species", 2:9))
    FuelTypes <- FuelTypes[-1]
    
    ## melt table to get all species in one column
    FuelTypes <- melt(FuelTypes, id.vars = 1:4, variable.name = "Species") %>%
      .[value != ""] %>%
      .[, Species := value] %>%
      .[, value := NULL]
    
    ## create a column with negative switch
    ## species with a negative switch negatively contribute to a fuel type
    FuelTypes[grepl("-", Species), negSwitch := -1L]
    FuelTypes[is.na(negSwitch), negSwitch := 1]
    FuelTypes[, Species := sub("-", "", Species)]
    
    ## remove last character to match other tables
    FuelTypes[, Species := substring(Species, 1, 7)]
    
    FuelTypes <- merge(FuelTypes, speciesNames, by.x = "Species", by.y = "LANDISNames", all.y = TRUE)
    FuelTypes[, Species := NULL]
    
    ## convert some columns to numeric
    numCols <- c("minAge", "maxAge", "negSwitch")
    FuelTypes[, (numCols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = numCols]
    
    sim$FuelTypes <- copy(FuelTypes)
    
  } else {
    FuelTypes <- prepInputs(targetFile = "FuelTypes.csv",
                            destinationPath = inputPath(sim),
                            fun = "utils::read.csv",
                            header = TRUE) %>%
      data.table
    
    ## remove last character to match other tables
    FuelTypes[, Species := substring(Species, 1, 7)]
    
    FuelTypes <- merge(FuelTypes, speciesNames, by.x = "Species", by.y = "LANDISNames", all.y = TRUE)
    FuelTypes[, Species := NULL]
    
    ## convert some columns to numeric
    numCols <- c("minAge", "maxAge", "negSwitch")
    FuelTypes[, (numCols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = numCols]
    
    sim$FuelTypes <- copy(FuelTypes)
  }
  
  
  ## PIXEL FUEL TYPES TABLE ------------------------
  ## create pixelFuelTypes table from cohorData
  ## subset cohort data and non-na fuel types
  pixelFuelTypes <- copy(sim$cohortData[, pixelGroup:B])
  tempFT <- na.omit(copy(FuelTypes[, -c("FuelTypeDesc", "species", "LandRNames"), with = FALSE]))  ## keep only complete lines with spp codes
  
  ## merge the two tables and add sppMultipliers
  pixelFuelTypes <- pixelFuelTypes[tempFT, on = .(speciesCode), allow.cartesian = TRUE, nomatch = 0] %>%
    .[order(pixelGroup)]
  pixelFuelTypes <- sppMultipliers[!is.na(speciesCode), speciesCode:Coefficient] %>%
    .[!duplicated(.)] %>%
    pixelFuelTypes[., on = .(speciesCode), nomatch =0] %>%
    .[order(pixelGroup)]
  
  sim$pixelFuelTypes <- copy(pixelFuelTypes)
  
  return(invisible(sim))
}

calcFuelTypes <- function(sim) {
  ## CALCULATE SPP VALUES FOR EACH FUEL TYPE IN EACH PIXEL ------------------------------
  ## calculate pixelFuelTypes per pixelGroup, ecoregion, fuel type and base fuel
  ## species biomass is weighted by the coeff and becomes 
  ## negative if the species has a negative contribution (negSwitch) to that fuel type
  sim$pixelFuelTypes <- sim$pixelFuelTypes %>%
    group_by(pixelGroup, ecoregionGroup, FuelType, BaseFuel) %>%
    filter(age>= minAge & age <= maxAge) %>%
    summarise(fuelTypeVal = sum(B*Coefficient*negSwitch)) %>%
    data.table()
  
  ## ASSIGN FINAL FUEL TYPES BASED ON SPP VALUES ----------------------------------------
  ## get max spp value in each pixelGroup
  cols <- c("pixelGroup", "ecoregionGroup")
  sim$pixelFuelTypes[, maxValue := max(fuelTypeVal),
                     by = cols]
  
  sim$pixelFuelTypes[, finalFuelType := as.integer()]
  sim$pixelFuelTypes[, finalBaseFuel := as.character()]
  sim$pixelFuelTypes[, c("finalFuelType", "finalBaseFuel") := calcFinalFuels(BaseFuel, 
                                                                             FuelType,
                                                                             fuelTypeVal,
                                                                             maxValue),
                     by = cols]
  
  ## CALCULATE CONIFEROUS/DECIDUOUS DOMINANCE --------------------------------------------
  ## sum pixelFuelTypes across conifer/deciduous BaseFuels 
  ## for each pixelGroup
  coniferDom <- sim$pixelFuelTypes[grepl("Conifer", BaseFuel)] %>%
    group_by(pixelGroup, ecoregionGroup) %>%
    summarise(sumCon = sum(fuelTypeVal)) %>%
    data.table
  
  deciduousDom <- sim$pixelFuelTypes[grepl("Deciduous", BaseFuel)] %>%
    group_by(pixelGroup, ecoregionGroup) %>%
    summarise(sumDec = sum(fuelTypeVal)) %>%
    data.table
  
  ## merge
  cols <- c("pixelGroup", "ecoregionGroup")
  sim$pixelFuelTypes <- merge(coniferDom, deciduousDom, by = cols, all = TRUE) %>%
    merge(sim$pixelFuelTypes, ., by = cols)
  
  ## ConiferPlantation, Open and Slash have their own rules for conifer/deciduos
  ## dominance, so sumCon and sumDec need to be overriden 
  cols <- c("sumCon", "sumDec")
  sim$pixelFuelTypes[finalBaseFuel == "ConiferPlantation", 
                     (cols) := list(100, 0)]
  sim$pixelFuelTypes[finalBaseFuel == "Slash", 
                     (cols) := list(0, 0)]
  sim$pixelFuelTypes[finalBaseFuel == "Open", 
                     (cols) := list(0, 0)]
  
  ## For Conifer and Deciduous, the conifer vs hardwood dominance
  ## are calculated for each as their percent dominance + 0.5
  ## NOTE: this != LANDIS source code, but approaches the idea of the manual
  ## replace NAs by zeros
  sim$pixelFuelTypes[is.na(sumCon), sumCon := 0]
  sim$pixelFuelTypes[is.na(sumDec), sumDec := 0]
  
  sim$pixelFuelTypes[, c("coniferDom", "hardwoodDom", "finalBaseFuel") := calcDominance(sumCon, sumDec, finalBaseFuel, P(sim)$hardwoodMax),
                     by = 1:nrow(sim$pixelFuelTypes)]
  
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  ## Get LANDIS test parameters, to use if others
  ## haven't been supplied in <module>/inputs
  
  if(!suppliedElsewhere("dynamicBiomassFuels", sim)) {
    maxcol <- 21 #max(count.fields(file.path(getPaths()$inputPath, "dynamic-biomass-fuels.txt"), sep = ""))
    dynamicBiomassFuels <- prepInputs(targetFile = "dynamic-biomass-fuels.txt", 
                                      url = extractURL("dynamicBiomassFuels", sim), 
                                      destinationPath = dataPath(sim),
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
    
    sim$dynamicBiomassFuels <- copy(dynamicBiomassFuels) 
  }
  
  return(invisible(sim))
}

