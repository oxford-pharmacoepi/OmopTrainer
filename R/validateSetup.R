
#' Validate if the setup for your session has been properly done
#'
#' @param training Character which training to validate setup for.
#'
#' @returns Whether your environment is ready
#' @export
#'
#' @examples
#' \dontrun{
#' library(OmopTrainer)
#' validateSetup()
#' }
#'
validateSetup <- function(training = "base") {
  # input check
  omopgenerics::assertChoice(training, names(specifications), length = 1)

  # get specifications
  specs <- specifications[[training]]

  # compile all the status
  status <- list()

  # check R minimum version
  status$r_version <- checkRVersion(minRVersion = specs$`min-R-version`)

  # check that RTools is installed
  status$r_tools <- checkRToolsInstalled(installed = specs$`R-tools`)

  # check list of packages
  status$packages <- checkPackages(pkgs = specs$packages)

  # check datasets
  status$datasets <- checkDatasets(datasets = specs$datasets)

  # check quarto installed
  status$quarto <- checkQuartoInstalled(installed = specs$quarto)

  invisible(reportStatus(status))
}

# Keep system probes behind small helpers so checks can be tested without
# changing the machine running the tests.
currentRVersion <- function() getRversion()
currentOperatingSystem <- function() .Platform$OS.type
whichExecutables <- function(names) Sys.which(names)
installedPackages <- function() utils::installed.packages()
availablePackages <- function(repos) utils::available.packages(repos = repos)
isPackageInstalled <- function(package) rlang::is_installed(package)
omopDataFolder <- function() Sys.getenv("OMOP_DATA_FOLDER", unset = "")
isMockDatasetDownloaded <- function(dataset) omock::isMockDatasetDownloaded(dataset)

checkRVersion <- function(minRVersion) {
  minRVersion <- as.numeric_version(minRVersion)
  currentVersion <- currentRVersion()
  oldVersion <- currentVersion < minRVersion
  return(dplyr::tibble(
    message = paste0(
      "R version ", currentVersion,
      " (minimum required: ", minRVersion, ")."
    ),
    symbol = ifelse(oldVersion, "x", "v"),
    passed = !oldVersion
  ))
}
checkRToolsInstalled <- function(installed) {
  if (!installed) {
    return(dplyr::tibble(
      message = "Rtools not needed for this training.",
      symbol = "v",
      passed = TRUE
    ))
  }

  if (currentOperatingSystem() != "windows") {
    return(dplyr::tibble(
      message = "Rtools is only required on Windows; check skipped.",
      symbol = "i",
      passed = TRUE
    ))
  }

  tools <- whichExecutables(c("make", "gcc"))
  available <- all(nzchar(tools))

  if (!available) {
    cli::cli_inform(c(
      x = "Rtools build tools were not found.",
      i = "Install the Rtools version matching R {getRversion()}, and restart R. {.url https://cran.r-project.org/bin/windows/Rtools/}"
    ))
    return(dplyr::tibble(
      message = "Rtools are not available.",
      symbol = "x",
      passed = FALSE
    ))
  } else {
    return(dplyr::tibble(
      message = "Rtools are available.",
      symbol = "v",
      passed = TRUE
    ))
  }
}
checkPackages <- function(pkgs) {
  repos <- getOption("repos")
  if (is.null(repos) || !length(repos)) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  } else {
    if (is.null(names(repos))) {
      names(repos) <- paste0("repo", seq_along(repos))
    }
    if ("CRAN" %in% names(repos) && identical(repos[["CRAN"]], "@CRAN@")) {
      repos[["CRAN"]] <- "https://cloud.r-project.org"
    }
  }
  available <- tryCatch(
    availablePackages(repos = repos),
    error = function(e) NULL
  )
  installed <- installedPackages()
  availableVersion <- if (is.null(available) || nrow(available) == 0L) {
    rep(NA_character_, length(pkgs))
  } else {
    available[match(pkgs, available[, "Package"]), "Version"]
  }

  # check status
  pkgs <- dplyr::tibble(
    package = pkgs,
    is_installed = pkgs %in% installed[, "Package"],
    version = installed[match(pkgs, installed[, "Package"]), "Version"],
    available_version = availableVersion
  )

  # report
  notInstalled <- pkgs$package[!pkgs$is_installed]
  if (length(notInstalled) > 0) {
    cli::cli_inform(c("x" = "The following packages are not installed: {.pkg {notInstalled}}."))
  }
  versionUnknown <- pkgs$package[pkgs$is_installed & is.na(pkgs$available_version)]
  notLatest <- pkgs$package[
    pkgs$is_installed & !is.na(pkgs$available_version) &
      pkgs$version != pkgs$available_version
  ]
  if (length(notLatest) > 0) {
    cli::cli_inform(c("!" = "The following packages are not at the latest repository version: {.pkg {notLatest}}."))
  }
  if (length(versionUnknown) > 0 || is.null(available) || nrow(available) == 0L) {
    cli::cli_inform(c(i = "Could not check repository versions for all required packages."))
  }

  allPkgs <- sort(unique(c(notInstalled, notLatest)))
  if (length(allPkgs) > 0) {
    packageList <- paste0('"', allPkgs, '"', collapse = ", ")
    code <- paste0(
      "install.packages(c(", packageList,
      '), repos = "https://cloud.r-project.org")'
    )
    cli::cli_inform(c(
      i = "Copy and run this command in the R console to install or update the packages:",
      ">" = "{.code {code}}"
    ))
  }

  # report
  if (length(notInstalled) > 0) {
    if (length(notLatest) > 0) {
      ms <- c("x" = paste0(length(notInstalled), " packages not installed, ", length(notLatest), " packages do not have the latest version."))
    } else {
      ms <- c("x" = paste0(length(notInstalled), " packages not installed."))
    }
  } else {
    if (length(notLatest) > 0) {
      ms <- c("!" = paste0(length(notLatest), " packages are not at the latest repository version."))
    } else if (length(versionUnknown) > 0 || is.null(available) || nrow(available) == 0L) {
      ms <- c("i" = "All required packages are installed; repository versions could not be checked.")
    } else {
      ms <- c("v" = "All required packages are installed and up to date.")
    }
  }

  dplyr::tibble(
    message = ms,
    symbol = names(ms),
    passed = names(ms) %in% c("v", "i")
  )
}
checkDatasets <- function(datasets) {
  if (length(datasets) == 0) {
    return(dplyr::tibble(
      message = "No datasets are required.",
      symbol = "i",
      passed = TRUE
    ))
  }

  # check OMOP_DATA_FOLDER is setup
  key <- "OMOP_DATA_FOLDER"
  folder <- omopDataFolder()
  if (identical(folder, "")) {
    cli::cli_inform(c(
      x = "`{key}` is not setup.",
      i = "Please open the `.Renviron` file ({.run usethis::edit_r_environ()})",
      ">" = "Populate as:",
      " " = "{key}=\"path/to/a/local/folder\"",
      "e.g. {key}=\"/Users/OmopTrainer/Documents/OMOP\"",
      i = "Restart R once the `.Renviron` file is populated."
    ))
    return(dplyr::tibble(
      message = paste0(key, " is not set up; datasets cannot be downloaded."),
      symbol = "x",
      passed = FALSE
    ))
  }

  if (!isPackageInstalled("omock")) {
    return(dplyr::tibble(
      message = "omock is not installed; datasets cannot be downloaded.",
      symbol = "x",
      passed = FALSE
    ))
  }

  # datasets are downloaded
  notDownloaded <- datasets[!purrr::map_lgl(datasets, isMockDatasetDownloaded)] |>
    suppressMessages()
  if (length(notDownloaded) > 0) {
    cli::cli_inform(c(
      x = "The following datasets have not been downloaded: {.pkg {notDownloaded}}.",
      i = "To download a dataset, run {.code omock::downloadMockDataset(\"{notDownloaded[1]}\")} in the R console."
    ))
    return(dplyr::tibble(
      message = paste0(length(notDownloaded), " datasets not downloaded"),
      symbol = "x",
      passed = FALSE
    ))
  }

  # duckdb is downloaded
  if (!isPackageInstalled("duckdb")) {
    return(dplyr::tibble(
      message = "duckdb is not installed, so datasets cannot be built into a CDM.",
      symbol = "x",
      passed = FALSE
    ))
  }

  return(dplyr::tibble(
    message = "All datasets have been downloaded and cdm can be created",
    symbol = "v",
    passed = TRUE
  ))
}
checkQuartoInstalled <- function(installed) {
  if (!installed) {
    return(dplyr::tibble(
      message = "quarto not needed for this training.",
      symbol = "v",
      passed = TRUE
    ))
  }

  quarto <- whichExecutables("quarto")
  available <- nzchar(quarto)

  if (!available) {
    cli::cli_inform(c(
      x = "quarto not found.",
      i = "Install it from {.url https://quarto.org/docs/get-started/}."
    ))
    return(dplyr::tibble(
      message = "quarto is not available.",
      symbol = "x",
      passed = FALSE
    ))
  } else {
    return(dplyr::tibble(
      message = "quarto available.",
      symbol = "v",
      passed = TRUE
    ))
  }
}
reportStatus <- function(status) {
  status <- dplyr::bind_rows(status, .id = "check")

  cli::cli_h1("Environment status")
  for (st in seq_len(nrow(status))) {
    ms <- rlang::set_names(status$message[st], status$symbol[st])
    cli::cli_inform(ms)
  }

  all(status$passed)
}
