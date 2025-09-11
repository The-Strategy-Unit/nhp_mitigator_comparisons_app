read_nee <- function(
  container_support,
  filename = "nee_table.rds",
  as_decimal = TRUE
) {
  nee <- container_support |>
    AzureStor::storage_load_rds(filename, type = "none")

  if (as_decimal) {
    nee <- nee |>
      dplyr::mutate(
        dplyr::across(c(.data$percentile10, .data$percentile90, mean), \(x) {
          x / 100
        })
      )
  }

  nee
}
