# Biomass_fuels 0.3.0

This release fixes the module so it can prepare its own land-cover map. Before, making that map always failed unless a project supplied it, because the study area was not declared as an input and the map was checked against an input most projects do not have.

The map's file name and cache label now come from a new study area name setting. If it is not set, a short code made from the study area is used, and a message says so. The module also gains automatic checks that run on every change.

# Biomass_fuels 0.2.0.9001

* The message for an unset `.studyAreaName` comes from `reproducible::studyAreaName(notSupplied = ".studyAreaName")` (PredictiveEcology/reproducible#638), so it reads the same in every module that uses it: "`.studyAreaName` not supplied; using a hash of `<object>`: <hash>". With an older reproducible the name is the same and there is no message.
* The land-cover raster made in `.inputObjects` (`rstLCCRTM`) is compared with `rasterToMatch`, which it is then
  projected to, instead of `rasterToMatchLarge`; that input is no longer declared. Without a `rasterToMatchLarge`
  the comparison errored. Same fix as Biomass_fuelsPFG ce302b3.

# Biomass_fuels 0.2.0.9000

* New parameter `.studyAreaName`. It names the land-cover raster this module makes (`rstLCCRTM_<name>.tif`) and
  tags its cache entry; the module read it before without defining it. If `NA` (the default), a hash of
  `studyArea` is used.
* `studyArea` is now a declared input. `.inputObjects` uses it to make `rstLCCRTM`, but
  undeclared objects are not visible there, so making `rstLCCRTM` always failed.
