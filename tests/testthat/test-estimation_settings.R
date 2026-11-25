test_that("Default settings use FOCEi and nlmixr2 default controls", {
  settings <- EstimationSettings()
  expect_true(S7::S7_inherits(settings, EstimationSettings))
  expect_equal(settings@method, "focei")
  expect_equal(settings@control, list())
})

test_that("Invalid settings are rejected", {
  expect_error(EstimationSettings(method = "unknown"), regexp = "@method must be one of")
  expect_error(EstimationSettings(method = c("focei", "saem")), regexp = "single character string")
  expect_error(EstimationSettings(control = list(1)), regexp = "unique, non-empty names")
  expect_error(EstimationSettings(control = list(a = 1, a = 2)), regexp = "unique, non-empty names")
  expect_error(EstimationSettings(control = list(a = list(1))), regexp = "must be atomic")
})

test_that("Settings can be exported to JSON and read back", {
  settings <- EstimationSettings(method = "saem", control = list(nBurn = 200, nEm = 300, print = 0, covMethod = "linFim"))
  expect_equal(read_settings(to_json(settings)), settings)

  file <- tempfile(fileext = ".json")
  on.exit(unlink(file))
  write_settings(settings, file)
  expect_equal(read_settings(file), settings)
})

test_that("Empty control is exported as a JSON object", {
  json <- to_json(EstimationSettings(method = "fo"))
  expect_match(json, "\"control\": {}", fixed = TRUE)
  expect_equal(read_settings(json), EstimationSettings(method = "fo"))
})

test_that("Control is optional in JSON", {
  expect_equal(read_settings('{"method": "foce"}'), EstimationSettings(method = "foce"))
})

test_that("JSON settings are validated against the schema", {
  expect_error(read_settings('{"method": "unknown"}'))
  expect_error(read_settings('{"method": "focei", "extra": 1}'))
  expect_error(read_settings('{"control": {}}'))
  expect_error(read_settings('{"method": "focei", "control": {"a": {"b": 1}}}'))
})

test_that("Schema and R code support the same methods", {
  schema <- jsonlite::read_json(campsisfit:::get_settings_schema())
  expect_setequal(unlist(schema$properties$method$enum), supported_methods())
})

test_that("Example settings file can be read", {
  settings <- read_settings(system.file("extdata", "settings_focei.json", package = "campsisfit"))
  expect_equal(settings, EstimationSettings(method = "focei", control = list(print = 0)))
})
