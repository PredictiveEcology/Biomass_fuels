#' Calculate fuel types
#'
#' This function returns the fuel type and base type that had the highest with the highest biomass in a pixel,
#' following the LANDIS-II Dynamic Biomass Fuels Extention v2.2 - 15 Jun 2017.
#' Note that there is no "Mixed" fuel type in LANDIS, as it becomes either conifer or deciduous.
#' In a mixed stand case, the proportion of conifers and deciduous biomass is used later necessary to \
#' for Fire Behaviour Predition system calculations
#'
#' @param pixelGroupFuelTypes a \code{data.table} with fuel types per pixel group, calculated from cohort biomasses
#' @param hardwoodMax an \code{integer} that defined the threshold of percent biomass below which fuel
#'    types are considered conifer or mixed. Defaults to 15, as in LANDIS example file
#'
#' @return an updated \code{pixelGroupFuelTypes} \code{data.table} with the added columns:
#'    finalFuelType - the final fuel type (\code{integer}),
#'    finalBaseFuel - th final base fuel (\code{character}),
#'    coniferDom and hardwoodDom - the proportion of conifer and deciduous biomass.
#'
#' @export
#' @importFrom data.table set setkey rbind

calcFinalFuels <- function(pixelGroupFuelTypes, hardwoodMax) {
  pixelGroupFuelTypes <- pixelGroupFuelTypes[, .(BaseFuel, FuelType, forTypValue, maxValue,
                                       pixelGroup)]
  pixelGroupFuelTypes <- pixelGroupFuelTypes[!duplicated(pixelGroupFuelTypes)]

  ## ignore "mixed" fuel type
  pixelGroupFuelTypes <- pixelGroupFuelTypes[BaseFuel != "Mixed"]

  ## CALCULATE CONIFEROUS/DECIDUOUS DOMINANCE ----
  ## sum conifer fuel types per pixelGroup and join
  coniferDT <- pixelGroupFuelTypes[grepl("Conifer", BaseFuel)]
  coniferDT[, sumConifer := sum(forTypValue, na.rm = TRUE),
            by = "pixelGroup"]
  setkey(coniferDT, pixelGroup)
  setkey(pixelGroupFuelTypes, pixelGroup)
  pixelGroupFuelTypes <- unique(coniferDT[, .(pixelGroup, sumConifer)])[pixelGroupFuelTypes, nomatch = NA]
  rm(coniferDT)

  ## sum deciduous fuel types per pixelGroup and join
  deciduousDT <- pixelGroupFuelTypes[grepl("Deciduous", BaseFuel)]
  deciduousDT[, sumDecid := sum(forTypValue, na.rm = TRUE),
             by = "pixelGroup"]
  setkey(deciduousDT, pixelGroup)
  setkey(pixelGroupFuelTypes, pixelGroup)
  pixelGroupFuelTypes <- unique(deciduousDT[, .(pixelGroup, sumDecid)])[pixelGroupFuelTypes, nomatch = NA]
  rm(deciduousDT)

  pixelGroupFuelTypes[is.na(sumConifer), sumConifer := 0]
  pixelGroupFuelTypes[is.na(sumDecid), sumDecid := 0]

  ## DETERMINE DOMINANT FUEL TYPES PER BASE FUEL
  ## NOTE: if several fuel types have the maximum biomass
  ## for a given base fuel, they'll win based on table order

  ## determine the dominant conifer fuel type
  coniferDT <- pixelGroupFuelTypes[grepl("Conifer", BaseFuel)]
  coniferPlantDT <- coniferDT[grepl("Plantation", coniferDT$BaseFuel)]
  coniferDT <- coniferDT[!grepl("Plantation", coniferDT$BaseFuel)]

  ## do plantations first if there are any
  ## they will override all conifer FTs
  if (NROW(coniferPlantDT)) {
    coniferPlantDT[, coniferPlantMaxValue := max(forTypValue, na.rm = TRUE),
                   by = "pixelGroup"]
    coniferPlantDT[forTypValue == coniferPlantMaxValue, finalFuelType := as.integer(unique(FuelType)),
                   by = "pixelGroup"]
  }

  if (NROW(coniferDT)) {
    ## now remaining conifer types
    coniferDT[, coniferMaxValue := max(forTypValue, na.rm = TRUE),
              by = "pixelGroup"]
    coniferDT[forTypValue == coniferMaxValue, finalFuelType := FuelType,
              by = "pixelGroup"]

    ## discard pixelGroups that have plantations and then join
    coniferDT <- coniferDT[!pixelGroup %in% coniferPlantDT$pixelGroup]  ## faster than !join in big tables
    coniferDT <- rbind(coniferDT, coniferPlantDT, fill = TRUE)
    rm(coniferPlantDT)
  }
  if (is.null(coniferDT[["coniferPlantMaxValue"]]))
    coniferDT[, coniferPlantMaxValue := integer(0)]

  if (is.null(coniferDT[["coniferPlantMaxValue"]]))
    coniferDT[, coniferMaxValue := integer(0)]

  ## determine the dominant deciduous fuel type
  deciduousDT <- pixelGroupFuelTypes[grepl("Deciduous", BaseFuel)]
  if (NROW(deciduousDT)) {
    deciduousDT[, decidMaxValue := max(forTypValue, na.rm = TRUE),
               by = "pixelGroup"]
    deciduousDT[forTypValue == decidMaxValue, finalFuelType := FuelType,
               by = "pixelGroup"]
  }

  if (is.null(deciduousDT[["decidMaxValue"]]))
    deciduousDT[, decidMaxValue := integer(0)]

  ## determine the mixed  fuel type -- not used in LANDIS
  # mixedDT <- pixelGroupFuelTypes[grepl("Mixed", BaseFuel)]
  # if (NROW(mixedDT)) {
  #   mixedDT[, mixedMaxValue := max(forTypValue, na.rm = TRUE),
  #           by = "pixelGroup"]
  #   mixedDT[forTypValue == mixedMaxValue, finalFuelType := FuelType,
  #           by = "pixelGroup"]
  # }
  #
  # if (is.null(mixedDT[["mixedMaxValue"]]))
  #   mixedDT[, mixedMaxValue := integer(0)]

  ## determine the dominant slash fuel type
  slashDT <- pixelGroupFuelTypes[grepl("Slash", BaseFuel)]
  if (NROW(slashDT)) {
    slashDT[, slashMaxValue := max(forTypValue, na.rm = TRUE),
            by = "pixelGroup"]
    slashDT[forTypValue == slashMaxValue, finalFuelType := FuelType,
            by = "pixelGroup"]
  }

  if (is.null(slashDT[["slashMaxValue"]]))
    slashDT[, slashMaxValue := integer(0)]


  ## determine the dominant open fuel type
  openDT <- pixelGroupFuelTypes[grepl("Open", BaseFuel)]
  if (NROW(openDT)) {
    openDT[, openMaxValue := max(forTypValue, na.rm = TRUE),
           by = "pixelGroup"]
    openDT[forTypValue == openMaxValue, finalFuelType := FuelType,
           by = "pixelGroup"]
  }

  if (is.null(openDT[["openMaxValue"]]))
    openDT[, openMaxValue := integer(0)]

  tempDT <- rbind(coniferDT, deciduousDT, slashDT, openDT, fill = TRUE)
  ## suppressWarning about coersion
  tempDT <- suppressWarnings(melt(tempDT, id.vars = c("pixelGroup", "sumDecid", "sumConifer",
                                                      "BaseFuel", "FuelType", "forTypValue", "maxValue", "finalFuelType"),
                                  variable.factor = FALSE))
  tempDT <- na.omit(tempDT)

  ## RESOLVE COMPETING NON-CONIFEROUS/DECIDUOUS FUEL TYPES ----------------
  ## get the fuel type with the maximum biomass - this will only work for
  ## plantations, slash and open, since maxValue for conifer/deciduous is
  ## summed and doesn't correspond directly to the values of each of these types
  ## when both exist.
  ## Ties are resolved later

  ## duplicated pixel groups have several "competing" fuel types
  dupsPG <- unique(tempDT$pixelGroup[duplicated(tempDT$pixelGroup)])

  if (length(dupsPG)) {
    dupsDT <- tempDT[pixelGroup %in% dupsPG]
    dupsDT[, c("finalFuelType2", "finalBaseFuel") := .findFinalFuelType(BaseFuel,
                                                                        finalFuelType,
                                                                        maxValue, value),
           by = "pixelGroup"]

    ## remove pixelGroups for which we could'nt find the final fuel type yet
    dupsDT <- na.omit(dupsDT)

    ## bind with unique PGs
    tempDT <- tempDT[!pixelGroup %in% dupsDT$pixelGroup]
    tempDT <- rbind(tempDT, dupsDT, fill = TRUE)
  }

  ## DETERMINE FINAL FUEL TYPES  -----
  ## ASSESS CONIFIER VS HARDWOOD DOMINANCE  -------
  ## this determines the final fuel type when there are conifers and hard woods.
  ## it will overwrite the previous fuel type if there had been ties (e.g. between open and conifer)
  ## In the case of ties conifer plantations take precendence, then open, then slash
  ## start at 0
  set(tempDT, NULL, c("coniferDom", "hardwoodDom"), 0L)

  ## ConiferPlantation, Open and Slash have their own rules
  ## for conifer/deciduous dominance that override other values
  coniferPlantDT <- tempDT[finalBaseFuel == "ConiferPlantation"]

  ## any slash? if so, it can't have plantations
  slashDT2 <- tempDT[finalBaseFuel == "Slash"]
  slashDT2 <- slashDT2[!pixelGroup %in% coniferPlantDT$pixelGroup]

  ## any open? if so, it can't have plantations/slash
  openDT2 <- tempDT[finalBaseFuel == "Open"]
  openDT2 <- openDT2[!pixelGroup %in% c(coniferPlantDT$pixelGroup, slashDT2$pixelGroup)]

  ## define conifer/hardwood proportions
  coniferPlantDT[, `:=`(coniferDom = 100L,
                        hardwoodDom = 0L)]
  slashDT2[, `:=`(coniferDom = 0L,
                  hardwoodDom = 0L)]
  slashDT2[, finalFuelType2 := finalFuelType]    ## because we've excluded other fuel types, we can use the one attributed initially
  openDT2[, `:=`(coniferDom = 0L,
                 hardwoodDom = 0L)]
  openDT2[, finalFuelType2 := finalFuelType]

  ## join the plantation/slash and open with unresolved conifer/deciduous fuel types
  tempDT2 <- rbind(coniferPlantDT, slashDT2, openDT2)
  tempDT <- tempDT[!pixelGroup %in% tempDT2$pixelGroup]
  tempDT <- rbind(tempDT, tempDT2)

  ## calculate conifer & hardwood dominance and
  ## resolve dominant fuel type and ajust dominance accordingly
  ## calculate the proportion of conifer/deciduous cover
  tempDT[sumConifer > 0 | sumDecid > 0,
         `:=` (coniferDom = as.integer(ceiling(sumConifer/(sumConifer + sumDecid) * 100)),
               hardwoodDom = as.integer(ceiling(sumDecid/(sumConifer + sumDecid) * 100))),
         by = "pixelGroup"]

  ## if the proportion of deciduous is lower than the threshold,
  ## the stand is considered pure conifer and gets the corresponding fuel type
  ## TODO: from my C# understanding, LANDIS assigns the last conifer FT in the table,
  ## if there are several final conifer FTs with the same biomass - I don't like this at all
  tempDT[hardwoodDom < hardwoodMax, `:=` (coniferDom = 100L,
                                          hardwoodDom = 0L),
         by = "pixelGroup"]
  tempDT[hardwoodDom < hardwoodMax,
         finalFuelType2 := last(finalFuelType[grepl("conifer", variable)]),
         by = "pixelGroup"]
  tempDT[hardwoodDom < hardwoodMax,
         finalBaseFuel := as.character(unique(BaseFuel[finalFuelType2 == FuelType])),
         by = "pixelGroup"]

  ## if the proportion of conifers is lower than the threshold,
  ## the stand is considered pure deciduous and gets the corresponding fuel type
  ## TODO: from my C# understanding, LANDIS assigns the last deciduous FT in the table,
  ## if there are several final deciduous FTs with the same biomass - I don't like this at all
  tempDT[coniferDom < hardwoodMax, `:=` (coniferDom = 0L,
                                         hardwoodDom = 100L),
         by = "pixelGroup"]
  tempDT[coniferDom < hardwoodMax,
         finalFuelType2 := last(finalFuelType[grepl("decid", variable)]),
         by = "pixelGroup"]
  tempDT[coniferDom < hardwoodMax,
         finalBaseFuel := as.character(unique(BaseFuel[finalFuelType2 == FuelType])),
         by = "pixelGroup"]

  ## if both the proportion of conifers and deciduous are higher than the threshold,
  ## the proportions are left untouched (like a mixed stand)
  ## and the fuel type is considered to be coniferous

  ## TODO: this, or the conditions above, seems to have a bug, has if coniferDom/hardwoodDom are = hardwoodMax
  ## there will be no fuel type assigned ## issue #7 in LANDIS-II-Foundation/Extension-Dynamic-Biomass-Fuels
  ## I "corrected" to >= below, in the mean time
  # tempDT[(coniferDom > hardwoodMax & hardwoodDom > hardwoodMax),
  #        finalFuelType2 := finalFuelType[grepl("conifer", variable)],
  #        by = "pixelGroup"]
  # tempDT[(coniferDom > hardwoodMax & hardwoodDom > hardwoodMax),
  #        finalBaseFuel := as.character(unique(BaseFuel[finalFuelType2 == FuelType])),
  #        by = "pixelGroup"]

  ## TODO 2: from my C# understanding, LANDIS assigns the last conifer FT in the table,
  ## if there are several final conifer FTs with the same biomass - I don't like this at all
  tempDT[coniferDom >= hardwoodMax & hardwoodDom >= hardwoodMax,
         finalFuelType2 := last(finalFuelType[grepl("conifer", variable)]),
         by = "pixelGroup"]
  tempDT[(coniferDom >= hardwoodMax & hardwoodDom >= hardwoodMax),
         finalBaseFuel := as.character(unique(BaseFuel[finalFuelType2 == FuelType])),
         by = "pixelGroup"]

  ## overwrite initial fuel types with revised ones
  tempDT[, finalFuelType := finalFuelType2]

  ## clean table and replace
  cols <- c("pixelGroup", "sumConifer", "sumDecid",
            "coniferDom", "hardwoodDom",
            "finalBaseFuel", "finalFuelType")
  tempDT <- unique(tempDT[, ..cols])

  pixelGroupFuelTypes <- tempDT
  return(pixelGroupFuelTypes)
}

#' Resolve conflicting fuel types
#'
#' Internal function that resolves competing fuel types in a pixel group, by
#'    returning the fuel type and base fuel that has biomass equal to the maximum
#'    biomass of a fuel in a pixel. Only used in situations where the fuel type
#'    is not a mix between coniferous and deciduous (in these cases the
#'    coniferous/deciduous fuel types will not have been attributed the maximum biomass).
#'    This function is applied in a data.table, per pixelGroup
#'
#' @param BaseFuel the base fuels in the pixel group
#' @param finalFuelType the final types previously obtained for the pixel group
#' @param maxValue the maximum biomass observed for a fuel type in the picel group
#' @param value the biomass value of each fuel type in the picel group
#'
#'
#' @return a \code{list} with finalFuelType2 and finalBaseFuel that will affect these columns

.findFinalFuelType <- function(BaseFuel, finalFuelType, maxValue, value) {
  finalFuelType2 <- finalFuelType[which(maxValue == max(value))]
  finalBaseFuel <- as.character(BaseFuel[which(maxValue == max(value))])

  list(finalFuelType2, finalBaseFuel)
}
