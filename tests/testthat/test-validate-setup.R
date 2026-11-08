test_that("R version check compares against the specified minimum", {
  local_mocked_bindings(
    currentRVersion = function() package_version("4.3.0"),
    .package = "OmopTrainer"
  )

  expect_true(OmopTrainer:::checkRVersion("4.2")$passed)
  expect_false(OmopTrainer:::checkRVersion("4.4")$passed)
})

test_that("Rtools check handles platform and availability cases", {
  local_mocked_bindings(
    currentOperatingSystem = function() "linux",
    .package = "OmopTrainer"
  )
  expect_true(OmopTrainer:::checkRToolsInstalled(TRUE)$passed)
  expect_equal(OmopTrainer:::checkRToolsInstalled(TRUE)$symbol, "i")
  expect_true(OmopTrainer:::checkRToolsInstalled(FALSE)$passed)

  local_mocked_bindings(
    currentOperatingSystem = function() "windows",
    whichExecutables = function(names) stats::setNames(rep("path/to/tool", length(names)), names),
    .package = "OmopTrainer"
  )
  expect_true(OmopTrainer:::checkRToolsInstalled(TRUE)$passed)

  local_mocked_bindings(
    whichExecutables = function(names) stats::setNames(rep("", length(names)), names),
    .package = "OmopTrainer"
  )
  expect_false(OmopTrainer:::checkRToolsInstalled(TRUE)$passed)
})

test_that("package check enforces only explicitly stated minimum versions", {
  installed <- matrix(
    c("present", "1.0.0", "old", "1.4.9", "newer", "2.0.0"),
    nrow = 3, byrow = TRUE,
    dimnames = list(NULL, c("Package", "Version"))
  )

  local_mocked_bindings(
    installedPackages = function() installed,
    .package = "OmopTrainer"
  )
  expect_true(OmopTrainer:::checkPackages("present")$passed)
  expect_true(OmopTrainer:::checkPackages("old")$passed)
  expect_true(OmopTrainer:::checkPackages("newer (>=1.5.0)")$passed)
  expect_true(OmopTrainer:::checkPackages("present (>=1.0.0)")$passed)
  expect_false(OmopTrainer:::checkPackages("old (>=1.5.0)")$passed)
  expect_false(OmopTrainer:::checkPackages("missing")$passed)
  expect_false(OmopTrainer:::checkPackages("missing (>=1.0.0)")$passed)
  baseRequirements <- OmopTrainer:::parsePackageRequirements(
    OmopTrainer:::specifications$base$packages
  )
  expect_equal(
    baseRequirements$minimum_version[baseRequirements$package == "duckdb"],
    "1.5.0"
  )
  expect_equal(
    baseRequirements$minimum_version[baseRequirements$package == "pak"],
    ""
  )

  customRepos <- c(CRAN = "https://cran.example.org")
  expect_identical(
    OmopTrainer:::packageInstallCommand("missing", customRepos, "binary"),
    'install.packages(c("missing"), repos = c(CRAN = "https://cran.example.org"), type = "binary")'
  )
})

test_that("Quarto check handles optional, present, and missing cases", {
  expect_true(OmopTrainer:::checkQuartoInstalled(FALSE)$passed)

  local_mocked_bindings(
    whichExecutables = function(names) "path/to/quarto",
    .package = "OmopTrainer"
  )
  expect_true(OmopTrainer:::checkQuartoInstalled(TRUE)$passed)

  local_mocked_bindings(
    whichExecutables = function(names) "",
    .package = "OmopTrainer"
  )
  expect_false(OmopTrainer:::checkQuartoInstalled(TRUE)$passed)
})

test_that("dataset check is deterministic and does not alter the user's setup", {
  expect_true(OmopTrainer:::checkDatasets(character())$passed)

  local_mocked_bindings(
    omopDataFolder = function() "",
    .package = "OmopTrainer"
  )
  expect_false(OmopTrainer:::checkDatasets("example")$passed)

  local_mocked_bindings(
    omopDataFolder = function() "/temporary/data",
    isPackageInstalled = function(package) TRUE,
    isMockDatasetDownloaded = function(dataset) dataset == "downloaded",
    .package = "OmopTrainer"
  )
  expect_false(OmopTrainer:::checkDatasets("missing")$passed)
  expect_true(OmopTrainer:::checkDatasets("downloaded")$passed)
})

test_that("validateSetup combines the individual check results", {
  pass <- function(...) {
    dplyr::tibble(message = "ok", symbol = "v", passed = TRUE)
  }
  local_mocked_bindings(
    checkRVersion = pass,
    checkRToolsInstalled = pass,
    checkPackages = pass,
    checkDatasets = pass,
    checkQuartoInstalled = pass,
    .package = "OmopTrainer"
  )

  result <- withVisible(OmopTrainer::validateSetup())
  expect_false(result$visible)
  expect_true(result$value)
})
