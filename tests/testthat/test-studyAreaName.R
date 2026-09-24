## `.studyAreaName` names the land-cover raster this module makes (rstLCCRTM) and tags its cache entry.
## It must work when the user sets nothing: NA becomes a hash of `studyArea`.

moduleRoot <- normalizePath(testthat::test_path("..", ".."))
moduleName <- basename(moduleRoot)

test_that(".studyAreaName is a parameter of this module, defaulting to NA", {
  p <- SpaDES.core::moduleParams(moduleName, dirname(moduleRoot))
  expect_true(".studyAreaName" %in% p$paramName)
  expect_true(is.na(p$default[[which(p$paramName == ".studyAreaName")]]))
  ins <- SpaDES.core::moduleInputs(moduleName, dirname(moduleRoot))$objectName
  expect_true(all(c("studyArea", "rasterToMatchLarge") %in% ins))
})

runInputObjects <- function(studyAreaName = NULL) {
  skip_if_not_installed("sn")
  skip_if_not_installed("raster")
  suppressPackageStartupMessages({library(raster); library(sp)})
  withr::local_options(spades.useRequire = FALSE, reproducible.useCache = FALSE,
                       spades.moduleCodeChecks = FALSE)
  r <- raster::raster(nrows = 10, ncols = 10, xmn = 0, xmx = 1000, ymn = 0, ymx = 1000,
                      crs = "+proj=utm +zone=12 +datum=WGS84", vals = 1)
  sa <- as(raster::extent(r), "SpatialPolygons")
  raster::crs(sa) <- raster::crs(r)
  got <- new.env()
  ## the land-cover download itself is not under test: record what it is asked to write
  local_mocked_bindings(prepInputsLCC = function(..., filename2, userTags) {
    got$filename2 <- filename2
    got$userTags <- userTags
    r
  }, .package = "LandR")
  tbl <- data.table::data.table()
  params <- list()
  params[[moduleName]] <- list(sppEquivCol = "LandR")
  if (!is.null(studyAreaName)) params[[moduleName]]$.studyAreaName <- studyAreaName
  td <- withr::local_tempdir()
  sim <- SpaDES.core::simInit(
    modules = moduleName, params = params,
    paths = list(modulePath = dirname(moduleRoot), inputPath = td, outputPath = td, cachePath = td),
    objects = list(studyArea = sa, rasterToMatch = r, rasterToMatchLarge = r,
                   sppEquiv = LandR::sppEquivalencies_CA[LandR %in% c("Pice_mar", "Pinu_ban")],
                   ForestFuelTypes = tbl, sppMultipliers = tbl, fTypeEcoreg = tbl,
                   FirePFGs = tbl, FirePFGs2Fuels = tbl))
  list(param = SpaDES.core::P(sim, module = moduleName)$.studyAreaName,
       filename2 = got$filename2, userTags = got$userTags, hash = reproducible::studyAreaName(sa))
}

test_that("with .studyAreaName unset, a hash of studyArea names the raster and the cache entry", {
  out <- runInputObjects()
  expect_identical(out$param, out$hash)
  expect_match(basename(out$filename2), paste0("_", out$hash, "\\.tif$"))
  expect_true(out$hash %in% out$userTags)
  expect_false(any(grepl("NA", c(out$filename2, out$userTags))))
})

test_that("a .studyAreaName the user sets is used as given", {
  out <- runInputObjects("myArea")
  expect_identical(out$param, "myArea")
  expect_identical(basename(out$filename2), "rstLCCRTM_myArea.tif")
  expect_true("myArea" %in% out$userTags)
})
