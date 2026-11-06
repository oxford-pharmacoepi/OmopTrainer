
#' Validate if the setup for your session has been properly done
#'
#' @returns Whether your environment is ready
#' @export
#'
#' @examples
#' \dontrun{
#' library(OmopTraining)
#' validateSetup()
#' }
#'
validateSetup <- function() {
  # compile all the status
  status <- list()

  # check that RTools is installed
  status$r_tools <- checkRToolsInstalled()

  # check list of packages
  pkgs <- c(
    "devtools", "usethis", "here", "gh", "duckdb", "dplyr", "dbplyr",
    "omopgenerics", "omock", "PatientProfiles", "DrugUtilisation",
    "OmopStudyBuilder", "OmopViewer", "IncidencePrevalence",
    "OmopConstructor", "CodelistGenerator", "OmopIndices", "OmopConstructor",
    "CohortCharacteristics", "CohortConstructor", "CohortSurvival",
    "PhenotypeR", "visOmopResults", "CohortSymmetry", "OmopSketch",
    "gt", "ggplot2", "MeasurementDiagnostics", "EpiStandard", "scales",
    "flextable"
  )
  status$packages <- checkPackages(pkgs)

  # check OMOP_DATA_FOLDER is setup
  # check delphi-100k, synpuf-1k is downloaded
  # hability to create the cdm with delphi and synpuf
  # check quarto installed

  invisible(reportStatus(status))
}

checkRToolsInstalled <- function() {

}
checkPackages <- function(pkgs) {
  available <- dplyr::as_tibble(available.packages())
  installed <- dplyr::as_tibble(installed.packages())

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
reportStatus <- function(status) {
  status <- dplyr::bind_rows(status, .id = "check")

  cli::cli_h1("Environment status")
  for (st in seq_len(nrow(status))) {
    ms <- rlang::set_names(status$message[st], status$symbol[st])
    cli::cli_inform(ms)
  }

  all(status$passed)
}
