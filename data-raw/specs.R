specifications <- list.files("data-raw/specs", pattern = "\\.yml$", full.names = TRUE)
names(specifications) <- tools::file_path_sans_ext(basename(specifications))
specifications <- specifications |>
  purrr::map(yaml::read_yaml)

usethis::use_data(specifications, internal = TRUE, overwrite = TRUE)
