## -------------------------------------
## FUNCTIONS TO PREPARE TABLES
## -------------------------------------

prepSppMultipliers <- function(sppMultipliers, sppEquiv, sppEquivCol) {
  sppMultipliers <- as.data.table(sppMultipliers)
  sppMultipliers[, Species := equivalentName(Species, sppEquiv, column = sppEquivCol)]
  setnames(sppMultipliers, "Species", "speciesCode")

  if (all(is.na(sppMultipliers[, speciesCode])))
    stop(paste("None of the species in sppMultipliers were found in sppEquiv table"))

  sppMultipliers[, Coefficient := as.numeric(Coefficient)]

  ## LANDIS advises keeping one for all species or values close to 1
  sppMultipliers[is.na(Coefficient), Coefficient := 1.0]

  ## exclude lines that have no spp codes
  sppMultipliers <- sppMultipliers[!is.na(speciesCode)]

  ## remove potential duplicates
  sppMultipliers <- sppMultipliers[!duplicated(sppMultipliers)]

  assertSppMultipliers(sppMultipliers, sppEquiv, sppEquivCol,)

  return(sppMultipliers)
}


prepFuelTypes <- function(FuelTypes, sppEquiv, sppEquivCol) {
  FuelTypes[, Species := equivalentName(Species, sppEquiv, column = sppEquivCol)]
  setnames(FuelTypes, "Species", "speciesCode")

  if (all(is.na(FuelTypes[, speciesCode])))
    stop(paste("None of the species in FuelTypes were found in sppEquiv table"))

  ## exclude lines that have no spp codes
  FuelTypes <- FuelTypes[!is.na(speciesCode)]

  ## convert some columns to numeric
  numCols <- c("minAge", "maxAge", "negSwitch")
  FuelTypes[, (numCols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = numCols]

  assertFuelTypes(FuelTypes, sppEquiv, sppEquivCol)
  return(FuelTypes)
}
