#' Generate a Simple Demo Dataset
#' @param n_schemes Integer.
#' @param n_mitigators Integer.
#' @param n_mitigator_groups Integer.
#' @param prop_selected Numeric.
#' @param seed Integer.
#' @return A data.frame.
#' @noRd
generate_test_dataset <- function(
    n_schemes = 10,
    n_mitigators = 50,
    n_mitigator_groups = 8,
    prop_selected = 0.6,
    seed = 123
) {

  scheme <- paste(
    "Scheme",
    stringr::str_pad(1:n_schemes, width = 2, side = "left", pad = "0")
  )

  mitigator <- paste(
    "Mitigator",
    stringr::str_pad(1:n_mitigators, width = 2, side = "left", pad = "0")
  )

  mitigator_lookup <- mitigator |>
    split(
      cut(
        seq_along(mitigator),
        n_mitigator_groups,
        labels = paste("Group", 1:n_mitigator_groups)
      )
    ) |>
    tibble::enframe(name = "mitigator_group", value = "mitigator") |>
    tidyr::unnest(mitigator)

  combos <- tidyr::crossing(scheme, mitigator) |>
    dplyr::left_join(mitigator_lookup, by = "mitigator")

  nrows <- nrow(combos)

  withr::with_seed(
    seed = seed,
    code = {
      combos |>
        dplyr::mutate(
          lo = runif(nrows),
          hi = pmin(lo + runif(nrows, 0.25, 0.5), 1),
          mid = (lo + hi) / 2,
          years = sample(20:30, nrows, replace = TRUE)
        ) |>
        dplyr::slice_sample(prop = prop_selected)
    }
  )

}
