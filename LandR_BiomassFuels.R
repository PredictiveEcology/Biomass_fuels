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
  reqdPkgs = list("data.table", "dplyr"),
  parameters = rbind(
    defineParameter("hardwoodMax", "numeric", 0L, "Threshold of percent biomass below
                    which fuel types are considered conifer or mixed")
    ),
  inputObjects = bind_rows(
    expectsInput(objectName = "speciesNames", objectClass = "data.table",
                 desc = "Table of species names/codes correspondences between LANDIS and LandR", 
                 sourceURL = NA),
    expectsInput(objectName = "sppMultipliers", objectClass = "data.table",
                 desc = "Table of species biomass coefficient weights.
                 Recommended to be close to 1.0 for all species (see LANDIS-II Dynamic Fire System Extension (v2.1) User Guide",
                 sourceURL = NA),
    expectsInput(objectName = "FuelTypes", objectClass = "data.table",
                 desc = "Table of Fuel Type parameters, with  base fuel type, species (in LANDIS code), their - or + contribution ('negSwitch'),
                 min and max age for each species", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "pixelFuelTypes", objectClass = "data.table", 
                  desc = "Fuel types per pixel group, calculated from cohort biomasses")
  )
))

doEvent.LandR_BiomassFuels = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep,
                           "LandR_BiomassFuels", "doFuelTypes")
    },
    doFuelTypes = {
      # do stuff for this event
      sim <- calcFuelTypes(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep,
                           "LandR_BiomassFuels", "doFuelTypes")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  browser()
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
  
  sim$pixelFuelTypes <- pixelFuelTypes
  
  return(invisible(sim))
}

calcFuelTypes <- function(sim) {
  browser()
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
  
  sim$pixelFuelType[, finalFuelType := as.integer()]
  sim$pixelFuelType[, finalBaseFuel := as.character()]
  sim$pixelFuelType[, c("finalFuelType", "finalBaseFuel") := calcFinalFuels(BaseFuel, 
                                                                         FuelType,
                                                                         fuelTypeVal,
                                                                         maxValue),
                 by = cols]
  
  ## CALCULATE CONIFEROUS/DECIDUOUS DOMINANCE --------------------------------------------
  ## sum pixelFuelTypes across conifer/deciduous BaseFuels 
  ## for each pixelGroup
  coniferDom <- sim$pixelFuelType[grepl("Conifer", BaseFuel)] %>%
    group_by(pixelGroup, ecoregionGroup) %>%
    summarise(sumCon = sum(fuelTypeVal)) %>%
    data.table
  
  deciduousDom <- sim$pixelFuelType[grepl("Deciduous", BaseFuel)] %>%
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
  
  sim$pixelFuelTypes[, c("coniferDom", "hardwoodDom", "finalBaseFuel") := calcDominance(sumCon, sumDec, finalBaseFuel),
                 by = 1:nrow(sim$pixelFuelTypes)]
  
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  browser()
  ## Get LANDIS test parameters, to use if others
  ## haven't been supplied in <module>/inputs
  dynamicBiomassFuels <- prepInputs(targetFile = "dynamic-biomass-fuels.txt", 
                                    url = "https://raw.githubusercontent.com/CeresBarros/Extension-Dynamic-Biomass-Fuels/master/testings/version-tests/v6.0-2.0/dynamic-biomass-fuels.txt", 
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
  
  ## SPECIES NAMES CORRESPONDENCES ------------------------
  if(!suppliedElsewhere("speciesNames", sim)) {
    speciesNames <- data.table(LANDISNames = unique(sim$speciesTable$LandisCode))
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
    
    if(any(!tolower(speciesList[,1]) %in% tolower(speciesNames$LandRNames)))
      warning(cat("\nFollowing selected species not found in the LANDIS species list.\nCheck if this is correct:\n",
                  paste0(speciesList[!tolower(speciesList[,1]) %in%
                                       tolower(speciesNames$LandRNames), 1],
                         collapse = ", ")))
    
    ## Convert species names for selected species, creating a 'species' column that matches other tables
    ## append species codes only after init of LBMR
    tempList <- speciesList
    rownames(tempList) <- tolower(tempList[,1])
    commonSpp <- tolower(speciesNames$LandRNames[tolower(LandRNames) %in% rownames(tempList)])
    
    speciesNames[tolower(LandRNames) %in% rownames(tempList), species := tempList[commonSpp,2]]
    
    speciesNames <- merge(speciesNames, sim$species[, c("species", "speciesCode"), with = FALSE],
                          by = "species", all = TRUE)
    
    sim$speciesNames <- copy(speciesNames)
  }
  
  ## SPECIES COEFFICIENTS ---------------------------------
  if(!suppliedElsewhere("sppMultipliers", sim)) {
    if(!file.exists(inputPath(sim), "sppMultipliers.csv")) {
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
      
    } else {
      sppMultipliers <- prepInputs(targetFile = "sppMultipliers.csv",
                                   destinationPath = inputPath(sim),
                                   fun = "read.csv", 
                                   header = TRUE) %>%
        data.table
    }
    
    sim$sppMultipliers <- copy(sppMultipliers)
  }
  
  ## FUEL TYPES TABLE -------------------------------------
  if(!suppliedElsewhere("FuelTypes", sim)) {
    if(!file.exists(inputPath(sim), "FuelTypes.csv")) {
      FuelTypes <- dynamicBiomassFuels[(which(col1=="FuelTypes") + 1) : (which(col1 == ">>EcoregionsTable") - 1),
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
      
    } else {
      FuelTypes <- prepInputs(targetFile = "FuelTypes.csv",
                              destinationPath = inputPath(sim),
                              fun = "utils::read.csv",
                              header = TRUE) %>%
        data.table
      
      ## remove last character to match other tables
      FuelTypes[, Species := substring(Species, 1, 7)]
    }
    
    FuelTypes <- merge(FuelTypes, speciesNames, by.x = "Species", by.y = "LANDISNames", all = TRUE)
    FuelTypes[, Species := NULL]
    
    ## convert some columns to numeric
    numCols <- c("minAge", "maxAge", "negSwitch")
    FuelTypes[, (numCols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = numCols]
    
    sim$FuelTypes <- copy(FuelTypes)
  }
  
  return(invisible(sim))
}

