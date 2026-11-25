# campsisfit

<!-- badges: start -->
[![R-CMD-check](https://github.com/lynxsoft-labs/campsisfit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lynxsoft-labs/campsisfit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Estimation of [Campsis](https://campsisverse.github.io/) models. Models written with `campsismod` are exported to `nlmixr2` and fitted with the estimation methods it provides.

## Installation

```r
# install.packages("remotes")
remotes::install_github("lynxsoft-labs/campsisfit")
```

## Example

```r
library(campsismod)
library(campsisfit)

model <- read.campsis(system.file("extdata", "theo_sd_model.json", package = "campsisfit"))
settings <- EstimationSettings(method = "focei", control = list(print = 0))

results <- estimate(model = model, dataset = nlmixr2data::theo_sd, settings = settings)
results

results@model # Campsis model with the estimated parameters
results@fit   # nlmixr2 fit
```

THETA `min`/`max` values of the Campsis model are used as estimation bounds. Estimated OMEGAs are written back in their original type (e.g. `cv%`).

Estimation settings can be saved to and loaded from JSON:

```r
write_settings(settings, "settings.json")
settings <- read_settings("settings.json")
```

```json
{
  "method": "saem",
  "control": {
    "nBurn": 200,
    "nEm": 300
  }
}
```

The supported methods are given by `supported_methods()`: `focei`, `foce`, `foi`, `fo`, `laplace`, `saem` and `posthoc`. Control options are passed to the nlmixr2 control function of the method (`nlmixr2est::foceiControl()`, `nlmixr2est::saemControl()`, etc.).
