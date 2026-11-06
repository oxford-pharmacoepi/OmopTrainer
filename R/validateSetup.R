
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

  # check that R is at least 4.2 is installed
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

checkRVersion <- function(minRVersion) {
  minRVersion <- as.numeric_version(minRVersion)
  currentVersion <- getRversion()
  oldVersion <- currentVersion < minRVersion
  return(dplyr::tibble(
    message = paste0("R version: ", currentVersion),
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

  if (.Platform$OS.type != "windows") {
    return(dplyr::tibble(
      message = "Rtools is only required on Windows; check skipped.",
      symbol = "i",
      passed = TRUE
    ))
  }

  tools <- Sys.which(c("make", "gcc"))
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
  } else if ("CRAN" %in% names(repos) && identical(repos[["CRAN"]], "@CRAN@")) {
    repos[["CRAN"]] <- "https://cloud.r-project.org"
  }
  available <- dplyr::as_tibble(utils::available.packages(repos = repos))
  installed <- dplyr::as_tibble(utils::installed.packages())

  # check status
  pkgs <- dplyr::tibble(
    package = pkgs,
    is_installed = pkgs %in% installed$Package,
    version = installed$Package[match(pkgs, installed$Package)],
    available_version = available$Version[match(pkgs, available$Package)]
  )

  # report
  notInstalled <- pkgs$package[!pkgs$is_installed]
  if (length(notInstalled) > 0) {
    cli::cli_inform(c("x" = "The following packages are not installed: {.pkg {notInstalled}}."))
  }
  notLatest <- pkgs$package[pkgs$version != pkgs$available_version & pkgs$is_installed]
  if (length(notLatest) > 0) {
    cli::cli_inform(c("!" = "The following packages latest version are not installed: {.pkg {notLatest}}."))
  }

  allPkgs <- sort(unique(c(notInstalled, notLatest)))
  if (length(allPkgs) > 0) {
    fun <- ifelse("pak" %in% notInstalled, "install.packages", "pak::pkg_install")
    code <- paste0(fun, "(c(\"", paste0(allPkgs, collapse = "\", \""), "\"))")
    cli::cli_inform(c(i = "RUN THE FOLLOWING BOLCK TO FIX THIS: {.run {code}}"))
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
      ms <- c("!" = paste0(length(notLatest), " packages do not have the latest version."))
    } else {
      ms <- c("v" = "All packages installed and up to date.")
    }
  }

  dplyr::tibble(message = ms, symbol = names(ms), passed = names(ms) == "v")
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
  folder <- Sys.getenv(x = key, unset = "")
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
      message = paste0(key, " is not setup, datasets can not be downloaded"),
      symbol = "x",
      passed = FALSE
    ))
  }

  if (!rlang::is_installed("omock")) {
    return(dplyr::tibble(
      message = "omock is not installed, datasets can not be downloaded",
      symbol = "x",
      passed = FALSE
    ))
  }

  # datasets are downloaded
  notDownloaded <- datasets[!purrr::map_lgl(datasets, omock::isMockDatasetDownloaded)] |>
    suppressMessages()
  if (length(notDownloaded) > 0) {
    cli::cli_inform(c(
      x = "{.pkg {notDownloaded}} not been downloaded.",
      i = "To download a dataset use the following code: {.run omock::omock::downloadMockDataset(\"{notDownloaded[1]}\")}."
    ))
    return(dplyr::tibble(
      message = paste0(length(notDownloaded), " datasets not downloaded"),
      symbol = "x",
      passed = FALSE
    ))
  }

  # duckdb is downloaded
  if (!rlang::is_installed("duckdb")) {
    return(dplyr::tibble(
      message = "duckdb is not installed, datasets can not be build into cdm",
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

  quarto <- Sys.which("quarto")
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
