## -------------------------------------
## FUNCTIONS TO PREPARE TABLES
## -------------------------------------

prepSppMultipliers <- function(sppMultipliers, sppEquiv, sppEquivCol) {
  sppMultipliers <- as.data.table(sppMultipliers)
  sppMultipliers[, Species := equivalentName(Species, sppEquiv, column = sppEquivCol)]
  setnames(sppMultipliers, "Species", "speciesCode")

  if (all(is.na(sppMultipliers[, speciesCode])))
    stop(paste("Species in sppMultipliers table not found in sppEquiv table"))

  sppMultipliers[, Coefficient := as.numeric(Coefficient)]

  ## LANDIS advises keeping one for all species or values close to 1
  sppMultipliers[is.na(Coefficient), Coefficient := 1.0]

  ## exclude lines that have no spp codes
  sppMultipliers <- sppMultipliers[!is.na(speciesCode)]

  ## remove potential duplicates
  sppMultipliers <- sppMultipliers[!duplicated(sppMultipliers)]

  return(sppMultipliers)
}


prepFuelTypes <- function(FuelTypes, sppEquiv, sppEquivCol) {
  FuelTypes[, Species := equivalentName(Species, sppEquiv, column = sppEquivCol)]
  setnames(FuelTypes, "Species", "speciesCode")

  if (all(is.na(FuelTypes[, speciesCode])))
    stop(paste("Species in sppMultipliers table not found in sppEquiv table"))

  ## convert some columns to numeric
  numCols <- c("minAge", "maxAge", "negSwitch")
  FuelTypes[, (numCols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = numCols]

  return(FuelTypes)
}
