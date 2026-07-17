read_nee <- function(
  filename = app_sys("app", "reference", "nee_table.rds"),
  as_decimal = TRUE
) {
  nee <- readr::read_rds(filename) |>
    dplyr::select(
      # We only need to match on param_name (others may be outdated)
      -c(tidyselect::starts_with("strategy"), "type")
    )

  if (as_decimal) {
    nee <- nee |>
      dplyr::mutate(
        dplyr::across(
          c("percentile10", "percentile90", "mean"),
          \(x) {
            x / 100
          }
        )
      )
  }

  nee
}
