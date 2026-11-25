# campsisfit 0.1.0

* Initial version.
* `estimate()` fits a Campsis model with nlmixr2 and returns an `EstimationResults` object holding the Campsis model with the estimated THETAs and OMEGAs (OMEGA types such as `cv%` are preserved), the settings and the nlmixr2 fit.
* `EstimationSettings()` S7 class (estimation method and nlmixr2 control options), with JSON import/export (`read_settings()`, `write_settings()`, `to_json()`).
