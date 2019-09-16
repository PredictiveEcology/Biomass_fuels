#' Calculate fuel types
#'
#' This function returns the fuel type and base type that had the highest with the highest biomass in a pixel,
#' following the LANDIS-II Dynamic Biomass Fuels Extention v2.2 - 15 Jun 2017.
#' Note that there is no "Mixed" fuel type in LANDIS, as it becomes either conifer or deciduous.
#' In a mixed stand case, the proportion of conifers and deciduous biomass is used later necessary to \
#' for Fire Behaviour Predition system calcualtions
#'
#' @param pixelFuelTypes a \code{data.table} with fuel types per pixel group, calculated from cohort biomasses
#' @param hardwoodMax an \code{integer} that defined the threshold of percent biomass below which fuel
#'    types are considered conifer or mixed. Defaults to 15, as in LANDIS example file
#'
#' @return an updated \code{pixelFuelTypes} \code{data.table} with the added columns:
#'    finalFuelType - the final fuel type (\code{integer}),
#'    finalBaseFuel - th final base fuel (\code{character}),
#'    coniferDom and hardwoodDom - the proportion of conifer and deciduous biomass.
#'
#' @export
#' @importFrom data.table set setkey rbind

calcFinalFuels <- function(pixelFuelTypes, hardwoodMax) {
  pixelFuelTypes <- pixelFuelTypes[, .(BaseFuel, FuelType, forTypValue, maxValue,
                               pixelGroup)]
  pixelFuelTypes <- pixelFuelTypes[!duplicated(pixelFuelTypes)]

  ## ignore "mixed" fuel type
  pixelFuelTypes <- pixelFuelTypes[BaseFuel != "Mixed"]

  ## CALCULATE CONIFEROUS/DECIDUOUS DOMINANCE ----
  ## sum conifer fuel types per pixelGroup and join
  coniferDT <- pixelFuelTypes[grepl("Conifer", BaseFuel)]
  coniferDT[, sumConifer := sum(forTypValue, na.rm = TRUE),
            by = "pixelGroup"]
  setkey(coniferDT, pixelGroup)
  setkey(pixelFuelTypes, pixelGroup)
  pixelFuelTypes <- coniferDT[, .(pixelGroup, sumConifer)][pixelFuelTypes, nomatch = NA]
  rm(coniferDT)

  ## sum deciduous fuel types per pixelGroup and join
  deciduosDT <- pixelFuelTypes[grepl("Deciduous", BaseFuel)]
  deciduosDT[, sumDecid := sum(forTypValue, na.rm = TRUE),
             by = "pixelGroup"]
  setkey(deciduosDT, pixelGroup)
  setkey(pixelFuelTypes, pixelGroup)
  pixelFuelTypes <- deciduosDT[, .(pixelGroup, sumDecid)][pixelFuelTypes, nomatch = NA]
  rm(deciduosDT)

  pixelFuelTypes[is.na(sumConifer), sumConifer := 0]
  pixelFuelTypes[is.na(sumDecid), sumDecid := 0]

  ## DETERMINE DOMINANT FUEL TYPES PER BASE FUEL
  ## NOTE: if several fuel types have the maximum biomass
  ## for a given base fuel, they'll win based on table order

  ## determine the dominant conifer fuel type
  coniferDT <- pixelFuelTypes[grepl("Conifer", BaseFuel)]
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
  deciduosDT <- pixelFuelTypes[grepl("Deciduous", BaseFuel)]
  if (NROW(deciduosDT)) {
    deciduosDT[, decidMaxValue := max(forTypValue, na.rm = TRUE),
               by = "pixelGroup"]
    deciduosDT[forTypValue == decidMaxValue, finalFuelType := FuelType,
               by = "pixelGroup"]
  }

  if (is.null(deciduosDT[["decidMaxValue"]]))
    deciduosDT[, decidMaxValue := integer(0)]

  ## determine the mixed  fuel type -- not used in LANDIS
  # mixedDT <- pixelFuelTypes[grepl("Mixed", BaseFuel)]
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
  slashDT <- pixelFuelTypes[grepl("Slash", BaseFuel)]
  if (NROW(slashDT)) {
    slashDT[, slashMaxValue := max(forTypValue, na.rm = TRUE),
            by = "pixelGroup"]
    slashDT[forTypValue == slashMaxValue, finalFuelType := FuelType,
            by = "pixelGroup"]
  }

  if (is.null(slashDT[["slashMaxValue"]]))
    slashDT[, slashMaxValue := integer(0)]


  ## determine the dominant open fuel type
  openDT <- pixelFuelTypes[grepl("Open", BaseFuel)]
  if (NROW(openDT)) {
    openDT[, openMaxValue := max(forTypValue, na.rm = TRUE),
           by = "pixelGroup"]
    openDT[forTypValue == openMaxValue, finalFuelType := FuelType,
           by = "pixelGroup"]
  }

  if (is.null(openDT[["openMaxValue"]]))
    openDT[, openMaxValue := integer(0)]

  tempDT <- rbind(coniferDT, deciduosDT, slashDT, openDT, fill = TRUE)
  ## suppressWarning about coersion
  tempDT <- suppressWarnings(melt(tempDT, id.vars = c("pixelGroup", "sumDecid", "sumConifer",
                                                        "BaseFuel", "FuelType", "forTypValue", "maxValue", "finalFuelType"),
                                   variable.factor = FALSE))
  tempDT <- na.omit(tempDT)

  ## RESOLVE COMPETING NON-CONIFEROUS/DECIDUOUS FUEL TYPES ----------------
  ## get the fuel type with the maximum biomass - this will only work for
  ## plantations, slash and open, since maxValue for conifer/deciduos is
  ## summed and doens't correspond directly to the values of each of these types
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
  coniferPlantDT[, `:=` (coniferDom = 100L,
                         hardwoodDom = 0L)]
  slashDT2[, `:=` (coniferDom = 0L,
                   hardwoodDom = 0L)]
  slashDT2[, finalFuelType2 := finalFuelType]    ## because we've excluded other fuel types, we can use the one attributed initially
  openDT2[, `:=` (coniferDom = 0L,
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
                hardwoodDom = as.integer(ceiling(sumDecid/(sumConifer + sumDecid) * 100)))]

  ## if the proportion of deciduous is lower than the threshold,
  ## the stand is considered pure conifer and gets the corresponding fuel type
  tempDT[hardwoodDom < hardwoodMax, `:=` (coniferDom = 100L,
                                           hardwoodDom = 0L)]
  tempDT[hardwoodDom < hardwoodMax,
          finalFuelType2 := finalFuelType[grepl("conifer", variable)],
          by = "pixelGroup"]
  tempDT[hardwoodDom < hardwoodMax,
          finalBaseFuel := as.character(unique(BaseFuel[finalFuelType2 == FuelType])),
          by = "pixelGroup"]

  ## if the proportion of conifers is lower than the threshold,
  ## the stand is considered pure deciduos and gets the corresponding fuel type
  tempDT[coniferDom < hardwoodMax, `:=` (coniferDom = 0L,
                                           hardwoodDom = 100L)]
  tempDT[coniferDom < hardwoodMax,
          finalFuelType2 := finalFuelType[grepl("decid", variable)],
          by = "pixelGroup"]
  tempDT[coniferDom < hardwoodMax,
          finalBaseFuel := as.character(unique(BaseFuel[finalFuelType2 == FuelType])),
          by = "pixelGroup"]

  ## if both the proportion of conifers and diciduos are higher than the threshold,
  ## the proportions are left untouched (like a mixed stand)
  ## and the fuel type is considered to be coniferous
  tempDT[(coniferDom > hardwoodMax & hardwoodDom > hardwoodMax),
          finalFuelType2 := finalFuelType[grepl("conifer", variable)],
          by = "pixelGroup"]
  tempDT[(coniferDom > hardwoodMax & hardwoodDom > hardwoodMax),
          finalBaseFuel := as.character(unique(BaseFuel[finalFuelType2 == FuelType])),
          by = "pixelGroup"]

  ## overwrite initial fuel types with revised ones
  tempDT[, finalFuelType := finalFuelType2]

  ## clean table and replace
  cols <- c("pixelGroup", "sumConifer", "sumDecid",
            "coniferDom", "hardwoodDom",
            "finalBaseFuel", "finalFuelType")
  tempDT <- unique(tempDT[, ..cols])

  pixelFuelTypes <- tempDT
  return(pixelFuelTypes)
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
