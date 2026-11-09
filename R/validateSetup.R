
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

  # check GitHub Desktop installed
  status$github_desktop <- checkGitHubDesktopInstalled(
    installed = specs$`github-desktop`
  )

  invisible(reportStatus(status))
}

# Keep system probes behind small helpers so checks can be tested without
# changing the machine running the tests.
currentRVersion <- function() getRversion()
currentOperatingSystem <- function() .Platform$OS.type
currentSystemName <- function() unname(Sys.info()[["sysname"]])
whichExecutables <- function(names) Sys.which(names)
installedPackages <- function() utils::installed.packages()
packageRepositories <- function() getOption("repos")
packageInstallType <- function() {
  type <- getOption("pkgType")
  if (identical(type, "both")) {
    if (identical(.Platform$pkgType, "source")) "source" else "binary"
  } else {
    type
  }
}
packageInstallCommand <- function(packages, repos, type) {
  packageList <- paste0('"', packages, '"', collapse = ", ")
  reposCode <- paste(deparse(repos, width.cutoff = 500L), collapse = "")
  paste0(
    "install.packages(c(", packageList, "), repos = ", reposCode,
    ', type = "', type, '")'
  )
}
parsePackageRequirements <- function(requirements) {
  pattern <- "^([[:alnum:].]+)(?:\\s*\\(>=\\s*([^()[:space:]]+)\\))?$"
  matches <- regmatches(requirements, regexec(pattern, requirements, perl = TRUE))

  package <- vapply(matches, `[[`, character(1), 2L)
  minimumVersion <- vapply(matches, function(x) {
    if (length(x) >= 3L) x[[3L]] else ""
  }, character(1))

  dplyr::tibble(
    package = package,
    minimum_version = minimumVersion
  )
}
isPackageInstalled <- function(package) rlang::is_installed(package)
omopDataFolder <- function() Sys.getenv("OMOP_DATA_FOLDER", unset = "")
isMockDatasetDownloaded <- function(dataset) omock::isMockDatasetDownloaded(dataset)
githubDesktopCandidates <- function() {
  system <- currentSystemName()

  if (identical(system, "Darwin")) {
    return(c(
      "/Applications/GitHub Desktop.app",
      file.path(path.expand("~"), "Applications", "GitHub Desktop.app"),
      file.path(path.expand("~"), "Downloads", "GitHub Desktop.app")
    ))
  }

  if (identical(currentOperatingSystem(), "windows")) {
    roots <- Sys.getenv(c("LOCALAPPDATA", "PROGRAMFILES", "PROGRAMFILES(X86)"))
    roots <- roots[nzchar(roots)]
    return(c(
      file.path(roots, "GitHubDesktop", "GitHubDesktop.exe"),
      file.path(roots, "GitHub Desktop", "GitHub Desktop.exe")
    ))
  }

  executables <- unname(whichExecutables(c("github-desktop", "github-desktop-bin")))
  executables[nzchar(executables)]
}

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
  requirements <- parsePackageRequirements(pkgs)
  repos <- packageRepositories()
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
  repoNames <- names(repos)
  repos <- as.character(repos)
  names(repos) <- repoNames
  installed <- installedPackages()
  packageIndex <- match(requirements$package, installed[, "Package"])
  requirements$version <- installed[packageIndex, "Version"]
  requirements$is_installed <- !is.na(packageIndex)
  requirements$below_minimum <- FALSE
  hasMinimum <- nzchar(requirements$minimum_version)
  hasVersion <- hasMinimum & requirements$is_installed & !is.na(requirements$version)
  requirements$below_minimum[hasVersion] <-
    package_version(requirements$version[hasVersion]) <
    package_version(requirements$minimum_version[hasVersion])
  requirements$version_unknown <- hasMinimum & requirements$is_installed &
    is.na(requirements$version)

  # report
  notInstalled <- requirements$package[!requirements$is_installed]
  if (length(notInstalled) > 0) {
    cli::cli_inform(c("x" = "The following packages are not installed: {.pkg {notInstalled}}."))
  }
  belowMinimum <- requirements[requirements$below_minimum, , drop = FALSE]
  if (nrow(belowMinimum) > 0L) {
    details <- paste0(
      belowMinimum$package,
      " (installed ", belowMinimum$version,
      "; required >= ", belowMinimum$minimum_version, ")"
    )
    cli::cli_inform(c(
      "!" = "These packages do not meet their minimum required versions: {.val {details}}."
    ))
  }
  versionUnknown <- requirements$package[requirements$version_unknown]
  if (length(versionUnknown) > 0L) {
    cli::cli_inform(c(
      x = "Could not determine installed versions for: {.pkg {versionUnknown}}."
    ))
  }

  if (nrow(belowMinimum) > 0L) {
    cli::cli_inform(c(
      i = "Install these packages from a repository that provides the stated minimum versions. The configured repository may not have a suitable version."
    ))
  }

  allPkgs <- sort(unique(c(notInstalled, versionUnknown)))
  if (length(allPkgs) > 0) {
    code <- packageInstallCommand(allPkgs, repos, packageInstallType())
    cli::cli_inform(c(
      i = "Copy and run this command in the R console to install or update the packages:",
      ">" = "{.code {code}}",
      "Restart R before checking again."
    ))
  }

  # report
  if (length(notInstalled) > 0) {
    if (nrow(belowMinimum) > 0L) {
      ms <- c("x" = paste0(
        length(notInstalled), " packages not installed, ",
        nrow(belowMinimum), " packages below their minimum required version."
      ))
    } else {
      ms <- c("x" = paste0(length(notInstalled), " packages not installed."))
    }
  } else {
    if (nrow(belowMinimum) > 0L) {
      ms <- c("!" = paste0(nrow(belowMinimum), " packages are below their minimum required version."))
    } else if (length(versionUnknown) > 0L) {
      ms <- c("x" = "Could not verify the minimum version for some installed packages.")
    } else {
      ms <- c("v" = "All required packages are installed and meet the specified minimum versions.")
    }
  }

  dplyr::tibble(
    message = ms,
    symbol = names(ms),
    passed = names(ms) == "v"
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
checkGitHubDesktopInstalled <- function(installed) {
  if (!installed) {
    return(dplyr::tibble(
      message = "GitHub Desktop is not needed for this training.",
      symbol = "v",
      passed = TRUE
    ))
  }

  available <- any(file.exists(githubDesktopCandidates()))
  if (!available) {
    cli::cli_inform(c(
      x = "GitHub Desktop was not found.",
      i = "Install it from {.url https://desktop.github.com/}."
    ))
    return(dplyr::tibble(
      message = "GitHub Desktop is not available.",
      symbol = "x",
      passed = FALSE
    ))
  }

  dplyr::tibble(
    message = "GitHub Desktop is available.",
    symbol = "v",
    passed = TRUE
  )
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
