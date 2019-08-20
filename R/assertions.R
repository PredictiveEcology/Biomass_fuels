## ASSERTIONS - may be transferred to a package at some point

assertSppMultipliers <- function(sppMultipliers, sppEquiv, sppEquivCol,
                                 doAssertion = getOption("LandR.assertions", TRUE),
                                 verbose = getOption("LandR.verbose", TRUE)) {
  if (doAssertion) {
    if(!isTRUE("speciesCode" %in% names(sppMultipliers))) {
      stop("sppMultipliers must have speciesCode")
    }
    if(!isTRUE("Coefficient" %in% names(sppMultipliers))) {
      stop("sppMultipliers must have Coefficient")
    }

    test1 <- all(sppMultipliers$speciesCode %in% sppEquiv[[sppEquivCol]])
    if (!isTRUE(test1)) {
      stop(paste0("Some species in sppMultipliers could not be found in sppEquiv[, ", sppEquivCol, "]"))
    }
  }
}

assertFuelTypes <- function(FuelTypes, sppEquiv, sppEquivCol,
                            doAssertion = getOption("LandR.assertions", TRUE),
                            verbose = getOption("LandR.verbose", TRUE)) {
  if (doAssertion) {
    if(!isTRUE("speciesCode" %in% names(FuelTypes))) {
      stop("FuelTypes must have speciesCode")
    }

    test1 <- all(na.omit(FuelTypes$speciesCode) %in% sppEquiv[[sppEquivCol]])
    if (!isTRUE(test1)) {
      stop(paste0("Some species in FuelTypes could not be found in sppEquiv[, ", sppEquivCol, "]"))
    }
  }
}
