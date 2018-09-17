## ----------------------------------------------------------------------------
## FUNCTIONS TO CALCULATE FUEL TYPES
## ----------------------------------------------------------------------------

## FINAL FUEL AND BASE TYPES BASED ON MAX SPP VALUE
## this function returns the fuel and base type that had the highest sppValue in a pixel
## note that there is no "Mixed" fuel type in LANDIS, as it becomes either conifer or deciduous and 
## the biomass of conifers and deciduous cohorts is used later when necessary (e.g. fire spread/severity)

calcFinalFuels <- function(BaseFuel, FuelType,
                           forTypValue, maxValue,
                           hardwoodMax) {
  ## make a temporary data.table
  tempDT <- data.table(BaseFuel = as.character(BaseFuel), FuelType = FuelType, 
                       forTypValue = forTypValue, maxValue = maxValue)
  tempDT <- tempDT[!duplicated(tempDT)]
  
  ## ignore "mixed" fuel type
  tempDT <- tempDT[BaseFuel != "Mixed"]
  
  ## CALCULATE CONIFEROUS/DECIDUOUS DOMINANCE ----
  ## sum biomass across conifer/deciduos fuel types in each 
  ## (the first option could be a way to account for mixed types?)
  # sumConifer <- sum(tempDT[grepl("Conifer|Mixed", BaseFuel), forTypValue], na.rm = TRUE)
  # sumDecid <- sum(tempDT[grepl("Deciduous|Mixed", BaseFuel), forTypValue], na.rm = TRUE)
  sumConifer <- sum(tempDT[grepl("Conifer", BaseFuel), forTypValue], na.rm = TRUE)
  sumDecid <- sum(tempDT[grepl("Deciduous", BaseFuel), forTypValue], na.rm = TRUE)
  
  
  ## DETERMINE DOMINANT FUEL TYPES PER BASE FUEL
  ## NOTE: if several fuel types have the maximum biomass
  ## for a given base fuel, they'll win based on table order
  ## determine the dominant conifer fuel type
  if (any(grepl("Conifer", tempDT$BaseFuel))) {
    coniferMaxValue <- max(tempDT[grepl("Conifer", BaseFuel), forTypValue], na.rm = TRUE)
    coniferFT <- tempDT[grepl("Conifer", BaseFuel)] %>%
      .[forTypValue == coniferMaxValue, FuelType] %>%
      unique()
    if (!length(coniferFT))
      coniferFT <- as.character("NA")
    if (length(coniferFT) > 1)
      coniferFT <- tail(coniferFT, 1)
  } else  {
    coniferFT <- as.character("NA")
    coniferMaxValue <- 0
  }
  
  ## determine the dominant conifer plantation fuel type
  ## only overrides if there are plantations
  if (any(grepl("Plantation", tempDT$BaseFuel))) {
    coniferPlantMaxValue <- max(tempDT[grepl("Plantation", BaseFuel), forTypValue], na.rm = TRUE)
    coniferFT <- tempDT[grepl("Plantation", BaseFuel)] %>%
      .[forTypValue == coniferPlantMaxValue, FuelType] %>%
      unique()
    if (length(coniferFT) > 1)
      coniferFT <- tail(coniferFT, 1)
  } else 
    coniferPlantMaxValue <- 0
  
  ## determine the dominant deciduous fuel type
  if (any(grepl("Deciduous", tempDT$BaseFuel))) {
    decidMaxValue <- max(tempDT[grepl("Deciduous", BaseFuel), forTypValue], na.rm = TRUE)
    decidFT <- tempDT[grepl("Deciduous", BaseFuel)] %>%
      .[forTypValue == decidMaxValue, FuelType] %>%
      unique()
    if (!length(decidFT))
      decidFT <- as.character("NA")
    if (length(decidFT) > 1)
      decidFT <- tail(decidFT, 1)
  } else {
    decidFT <- as.character("NA") 
    decidMaxValue <- 0
  }
  
  # if (any(grepl("Mixed", tempDT$BaseFuel))) {
  #   mixedMaxValue <- max(tempDT[grepl("Mixed", BaseFuel), forTypValue], na.rm = TRUE)
  #   mixedFT <- tempDT[grepl("Mixed", BaseFuel)] %>%
  #     .[forTypValue == mixedMaxValue, FuelType] %>%
  #     unique()
  #   if (!length(mixedFT))
  #     mixedFT <- as.character("NA")
  #   if (length(mixedFT) > 1)
  #     mixedFT <- tail(mixedFT, 1)
  # } else {
  #   mixedFT <- as.character("NA") 
  #   mixedMaxValue <- 0
  # }
  
  ## determine the dominant slash fuel type
  if (any(grepl("Slash", tempDT$BaseFuel))) {
    slashMaxValue <- max(tempDT[grepl("Slash", BaseFuel), forTypValue], na.rm = TRUE)
    slashFT <- tempDT[grepl("Slash", BaseFuel)] %>%
      .[forTypValue == slashMaxValue, FuelType] %>%
      unique()
    if (!length(slashFT))
      slashFT <- as.character("NA")
    if (length(slashFT) > 1)
      slashFT <- tail(slashFT, 1)
  } else {
    slashFT <- as.character("NA") 
    slashMaxValue <- 0
  }
  
  ## determine the dominant open fuel type
  if (any(grepl("Open", tempDT$BaseFuel))) {
    openMaxValue <- max(tempDT[grepl("Open", BaseFuel), forTypValue], na.rm = TRUE)
    openFT <- tempDT[grepl("Open", BaseFuel)] %>%
      .[forTypValue == openMaxValue, FuelType] %>%
      unique()
    if (!length(openFT))
      openFT <- as.character("NA")
    if (length(openFT) > 1)
      openFT <- tail(openFT, 1)
  } else {
    openFT <- as.character("NA") 
    openMaxValue <- 0
  }
  
  tempDT2 <- data.table(finalFuelType = c(coniferFT, coniferFT, decidFT, #mixedFT, 
                                          slashFT, openFT),
                        maxValueFT = c(coniferMaxValue, coniferPlantMaxValue, decidMaxValue, #mixedMaxValue, 
                                       slashMaxValue, openMaxValue), 
                        finalBaseFuel = c("Conifer", "ConiferPlantation", "Deciduous",#"Mixed",
                                          "Slash", "Open")) 
  
  ## DETERMINE FINAL FUEL TYPE FOR ALL BUT CONIFER AND DECIDUOUS -----
  ## get the fuel type with the maximum biomass. When there are ties, 
  ## conifer plantations take precendence, then open, then slash
  maxValue2 <- max(tempDT$maxValue, na.rm = TRUE)
  finalFuelType <- unique(as.integer(tempDT2[maxValueFT == maxValue2, finalFuelType]))
  finalBaseFuel <- unique(tempDT2[maxValueFT == maxValue2, finalBaseFuel])
  
  ## ASSESS CONFIER VS HARDWOOD DOMINANCE  -------
  ## this determines the final fuel type, when there are conifers and hard woods.
  ## it will overwrite the previous fuel type if there had been ties (e.g. between open and conifer)
  
  ## start at 0
  coniferDom <- 0
  hardwoodDom <- 0
  
  ## ConiferPlantation, Open and Slash have their own rules
  ## for conifer/deciduous dominance that override other values
  if (any(finalBaseFuel == "ConiferPlantation")) {
    coniferDom <- 100
    hardwoodDom <- 0
  } else {
    if (any(finalBaseFuel == "Slash")) {
      ## Slash takes precedence over open
      finalFuelType <- finalFuelType[which(finalBaseFuel == "Slash")]
      finalBaseFuel <- "Slash"
      coniferDom <- 0
      hardwoodDom <- 0
    } else {
      if (any(finalBaseFuel == "Open")) {
        finalFuelType <- finalFuelType[which(finalBaseFuel == "Open")]
        finalBaseFuel <- "Open"
        coniferDom <- 0
        hardwoodDom <- 0
      }
    }
  }
  
  ## calculate conifer & hardwood dominance and
  ## resolve dominant fuel type and ajust dominance accordingly
  if (sumConifer > 0 | sumDecid > 0) {
    coniferDom = ceiling(sumConifer/(sumConifer + sumDecid) * 100)
    hardwoodDom = ceiling(sumDecid/(sumConifer + sumDecid) * 100)
    
    if (hardwoodDom < hardwoodMax) {
      coniferDom <- 100 
      hardwoodDom <- 0
      finalFuelType <- coniferFT
      finalBaseFuel <- as.character(unique(tempDT[FuelType %in% coniferFT, BaseFuel]))
    } 
    if (coniferDom < hardwoodMax) {
      coniferDom <- 0 
      hardwoodDom <- 100
      finalFuelType <- decidFT
      finalBaseFuel <- as.character(unique(tempDT[FuelType %in% decidFT, BaseFuel]))
    } 
    if (hardwoodDom > hardwoodMax & coniferDom > hardwoodMax) {
      finalFuelType <- coniferFT
      finalBaseFuel <- as.character(unique(tempDT[FuelType %in% coniferFT, BaseFuel]))
    }
  }
  
  test <- list(sumConifer = as.integer(sumConifer), sumDecid = as.integer(sumDecid), 
               coniferDom = as.integer(coniferDom), hardwoodDom = as.integer(hardwoodDom), 
               finalBaseFuel = as.character(finalBaseFuel), finalFuelType = as.integer(finalFuelType))
}
