# Biomass_fuels 0.2.0.9000

* New parameter `.studyAreaName`. It names the land-cover raster this module makes (`rstLCCRTM_<name>.tif`) and
  tags its cache entry; the module read it before without defining it. If `NA` (the default), a hash of
  `studyArea` is used.
* `studyArea` and `rasterToMatchLarge` are now declared inputs. `.inputObjects` uses both to make `rstLCCRTM`, but
  undeclared objects are not visible there, so making `rstLCCRTM` always failed.
