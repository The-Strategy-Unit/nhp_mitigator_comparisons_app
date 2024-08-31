read_nee <- function(
    container_support,
    filename = "trust-peers.rds",
    as_decimal = TRUE
) {

  nee <- container_support |>
    AzureStor::storage_load_rds(filename) |>
    dplyr::mutate(
      param_name = dplyr::case_match(
        param_name,
        "bads_daycase" ~ "day_procedures_usually_dc",
        "bads_daycase_occasional" ~ "day_procedures_occasionally_dc",
        "bads_outpatients" ~ "day_procedures_usually_op",
        "bads_outpatients_or_daycase" ~ "day_procedures_occasionally_op",
        .default = param_name
      )
    )

  if (as_decimal) {
    nee <- nee |>
      dplyr::mutate(
        dplyr::across(c(percentile10, percentile90, mean), \(x) x / 100)
      )
  }

  nee

}
