## The module's metadata is its public contract: a project using this module binds
## to these object names and classes. Renaming or retyping one breaks every caller,
## which is exactly the class of change the raster -> terra migration makes, so it is
## worth asserting here rather than discovering downstream.
##
## When a change is deliberate, update this file in the same commit and bump the
## module version to match: removed, renamed or retyped is a MAJOR bump.

test_that("module metadata parses", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_type(md, "list")
  expect_identical(md$name, moduleName)
})

test_that("inputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  inputs <- stats::setNames(md$inputObjects$objectClass, md$inputObjects$objectName)
  expect_identical(
    inputs[order(names(inputs))],
    c(cohortData          = "data.table",
      ForestFuelTypes     = "data.table",
      fTypeEcoreg         = "data.table",
      nonForestFuelsTable = "data.table",
      rasterToMatch       = "RasterLayer",
      rstLCCRTM           = "RasterLayer",
      sppEquiv            = "data.table",
      sppMultipliers      = "data.table",
      studyArea           = "SpatialPolygonsDataFrame")
  )
})

test_that("outputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  outputs <- stats::setNames(md$outputObjects$objectClass, md$outputObjects$objectName)
  expect_identical(
    outputs[order(names(outputs))],
    c(fuelTypesMaps       = "list",
      pixelNonForestFuels = "data.table",
      rstLCCRTM           = "RasterLayer")
  )
})

test_that("parameters are the expected names", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_identical(
    sort(md$parameters$paramName),
    sort(c(".studyAreaName", ".useCache", "fireInitialTime", "fireTimestep",
           "hardwoodMax", "nonForestFire", "sppEquivCol"))
  )
})
