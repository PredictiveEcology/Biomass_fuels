# Biomass_fuels 0.2.0.9001

* The land-cover raster made in `.inputObjects` (`rstLCCRTM`) is compared with `rasterToMatch`, which it is then
  projected to, instead of `rasterToMatchLarge`; that input is no longer declared. Without a `rasterToMatchLarge`
  the comparison errored. Same fix as Biomass_fuelsPFG ce302b3.

# Biomass_fuels 0.2.0.9000

* New parameter `.studyAreaName`. It names the land-cover raster this module makes (`rstLCCRTM_<name>.tif`) and
  tags its cache entry; the module read it before without defining it. If `NA` (the default), a hash of
  `studyArea` is used.
* `studyArea` is now a declared input. `.inputObjects` uses it to make `rstLCCRTM`, but
  undeclared objects are not visible there, so making `rstLCCRTM` always failed.
