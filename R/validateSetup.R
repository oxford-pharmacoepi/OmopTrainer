
#' Validate if the setup for your session has been properly done
#'
#' @returns Whether your environment is ready
#' @export
#'
#' @examples
#' library(OmopTrainer)
#' validateSetup()
#'
validateSetup <- function() {
  specs <- specifications$base

  # compile all the status
  status <- list()

  # check that R is at least 4.2 is installed
  status$r_version <- checkRVersion(minRVersion = specs$`min-R-version`)

  # check that RTools is installed
  if (specs$`R-tools`) {
    status$r_tools <- checkRToolsInstalled()
  }

  # check list of packages
  status$packages <- checkPackages(pkgs = specs$packages)

  # check datasets
  status$datasets <- checkDatasets(datasets = specs$datasets)

  # check quarto installed

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
checkRToolsInstalled <- function() {
  if (.Platform$OS.type != "windows") {
    return(dplyr::tibble(
      message = "Rtools is only required on Windows; check skipped.",
      symbol = "i",
      passed = TRUE
    ))
  }

  tools <- Sys.which(c("make", "gcc"))
  available <- all(nzchar(tools))

  dplyr::tibble(
    message = if (available) {
      paste0(
        "Rtools build tools found (make: ", tools[["make"]],
        "; gcc: ", tools[["gcc"]], ")."
      )
    } else {
      paste0(
        "Rtools build tools were not found. Install the Rtools version ",
        "matching R ", getRversion(), " and restart R. ",
        "{.url https://cran.r-project.org/bin/windows/Rtools/}"
      )
    },
    symbol = if (available) "v" else "x",
    passed = available
  )
}
checkPackages <- function(pkgs) {
  available <- dplyr::as_tibble(utils::available.packages())
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
    fun <- ifelse("pak" %in% allPkgs, "install.packages", "pak::pkg_install")
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
  # check delphi-100k, synpuf-1k is downloaded
  # hability to create the cdm with delphi and synpuf
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
