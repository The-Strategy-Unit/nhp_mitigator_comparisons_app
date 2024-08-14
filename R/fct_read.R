read_mitigator_lookup <- function(file) {
  file |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::select("Strategy variable", "Mitigator code")
}

read_nee <- function(file, as_decimal = TRUE) {

  nee <- file |>
    readr::read_rds() |>
    dplyr::select(param_name, percentile10, percentile90, mean) |>
    dplyr::filter(!stringr::str_detect(param_name, "bads"))

  if (as_decimal) {
    nee <- nee |>
      dplyr::mutate(
        dplyr::across(c(percentile10, percentile90, mean), \(x) x / 100)
      )
  }

  nee

}

read_mitigator_groups <- function(file) {
  file |>
    readxl::read_excel() |>
    dplyr::mutate(
      Grouping = stringr::str_replace_all(Grouping, "ameanable", "amenable")
    )
}
