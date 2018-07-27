## ----------------------------------------------------------------------------
## FUNCTIONS TO CALCULATE FUEL TYPES
## ----------------------------------------------------------------------------

## FINAL FUEL AND BASE TYPES BASED ON MAX SPP VALUE
## this function returns the fuel and base type that had the highest sppValue in a pixel

calcFinalFuels <- function(BaseFuel, FuelType, fuelTypeVal, maxValue, hardwoodMax) {
  finalFuelType  <- FuelType[which(fuelTypeVal == unique(maxValue))]
  finalBaseFuel <- BaseFuel[which(fuelTypeVal == unique(maxValue))]
  
  ## in case more that one fuel type has maxValue:
  if(length(finalFuelType) > 1) {
    if(any(grepl("Conifer$", finalBaseFuel))){
      finalFuelType <- finalFuelType[finalBaseFuel == "Conifer"]
      finalBaseFuel <- "Conifer"
    } else if(any(grepl("Deciduous", finalBaseFuel))) {
      finalFuelType <- finalFuelType[finalBaseFuel == "Deciduous"]
      finalBaseFuel <- "Deciduous"
    } else if(any(grepl("Plantation", finalBaseFuel))) {
      finalFuelType <- finalFuelType[finalBaseFuel == "ConiferPlantation"]
      finalBaseFuel <- "ConiferPlantation" 
    } else if(any(grepl("Open", finalBaseFuel))) {
      finalFuelType <- finalFuelType[finalBaseFuel == "Open"]
      finalBaseFuel <- "Open"  
    } else if(any(grepl("Slash", finalBaseFuel))) {
      finalFuelType <- finalFuelType[finalBaseFuel == "Slash"]
      finalBaseFuel <- "Slash"
    }
  }
  
  list(finalFuelType = as.integer(finalFuelType), finalBaseFuel = as.character(finalBaseFuel))
}


## CONFIFER & DECIDUOUS DOMINANCE
## this function assesses the conifer/deciduous dominance according to the sum of
## conifer/deciduos sppValues across fuel types for a pixel compared to the hardwoodMax parameter
## ATTENTION: the algorithm differs from the original LANDIS-II source code, 
## but ideally is closer to what is described in the manual
calcDominance <- function(sumCon, sumDec, finalBaseFuel, hardwoodMax){
  ## get the initial fuel type attribute dominances
  finalBaseFuel <- finalBaseFuel
  if(finalBaseFuel == "ConiferPlantation") {
    coniferDom <- 100
    hardwoodDom <- 0
  } else {
    ## initial values will be kept for Open and Slash
    coniferDom <- 0
    hardwoodDom <- 0
  }
  
  
  if(sumCon > 0 | sumDec > 0) {
    coniferDom <- ceiling(sumCon/(sumCon + sumDec) * 100)
    hardwoodDom <- ceiling(sumDec/(sumCon + sumDec) * 100)
    
    if(coniferDom > hardwoodMax & hardwoodDom > hardwoodMax)
      finalBaseFuel <- "Mixed"
    
    if(coniferDom <= hardwoodMax & hardwoodDom <= hardwoodMax) {
      finalBaseFuel <- if(coniferDom > 0 & hardwoodDom > 0)
        "Mixed" else if(coniferDom > 0) 
          "Conifer" else
            "Deciduous"
    }
    
    if(coniferDom <= hardwoodMax & hardwoodDom > hardwoodMax) {
      finalBaseFuel <- "Deciduous"
      coniferDom <- 0
      hardwoodDom <-  100
    }
    
    if(coniferDom > hardwoodMax & hardwoodDom <= hardwoodMax) {
      finalBaseFuel <- "Conifer"
      coniferDom <- 100
      hardwoodDom <-  0
    }
  }
  
  list(coniferDom = coniferDom, hardwoodDom = hardwoodDom, finalBaseFuel = finalBaseFuel)
}