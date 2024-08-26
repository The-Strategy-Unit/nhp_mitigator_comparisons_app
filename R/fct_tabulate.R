extract_params <- function(params, runs_meta) {

  possibly_report_params_table <- purrr::possibly(report_params_table)

  activity_avoidance <- params |>
    purrr::map(possibly_report_params_table, "activity_avoidance") |>
    purrr::list_rbind()

  efficiencies <- params |>
    purrr::map(possibly_report_params_table, "efficiencies") |>
    purrr::list_rbind()

  runs_meta <- runs_meta |> dplyr::select(dataset, scenario, run_stage)

  activity_avoidance |>
    dplyr::bind_rows(efficiencies) |>
    dplyr::mutate(
      peer_year = paste0(
        peer,
        "_", stringr::str_sub(baseline_year, 3, 4),
        "_", stringr::str_sub(horizon_year, 3, 4)
      )
    ) |>
    dplyr::left_join(runs_meta, by = dplyr::join_by("peer" == "dataset"))

}

report_params_table <- function(
    p,  # a single scheme's params
    parameter = c("activity_avoidance", "efficiencies")
) {

  parameter_data <- p[[parameter]]

  time_profiles <- p[["time_profile_mappings"]][[parameter]] |>
    purrr::map(unlist) |>
    purrr::map(tibble::enframe, "strategy", "time_profile") |>
    data.table::rbindlist(idcol = "activity_type") |>
    dplyr::tibble()

  parameter_data |>
    purrr::map_depth(2, "interval") |>
    purrr::map(tibble::enframe, "strategy") |>
    dplyr::bind_rows(.id = "activity_type") |>
    tidyr::unnest_wider("value", names_sep = "_") |>
    dplyr::left_join(
      time_profiles,
      by = dplyr::join_by("activity_type", "strategy")
    ) |>
    dplyr::arrange("activity_type_name", "mitigator_name") |>
    dplyr::mutate(
      parameter = parameter,
      peer = p[["dataset"]],
      baseline_year = p[["start_year"]],
      horizon_year = p[["end_year"]]
    )

}

prepare_skeleton_table <- function(extracted_params) {

  strategies <- extracted_params |>
    dplyr::distinct(activity_type, strategy, parameter)

  tidyr::expand_grid(
    "strategy" = unique(extracted_params[["strategy"]]),
    "peer_year" = unique(extracted_params[["peer_year"]])
  ) |>
    dplyr::left_join(strategies, by = dplyr::join_by(strategy)) |>
    dplyr::select(peer_year, activity_type, parameter, strategy) |>
    dplyr::arrange(peer_year, activity_type, parameter, strategy)

}

populate_table <- function(
    skeleton_table,
    extracted_params,
    trust_code_lookup,
    mitigator_lookup,
    nee_results
) {

  data_joined <- skeleton_table |>
    dplyr::left_join(
      extracted_params,
      by = dplyr::join_by(peer_year, activity_type, parameter, strategy)
    ) |>
    dplyr::left_join(
      trust_code_lookup |>
        dplyr::mutate(
          scheme_code = `Trust ODS Code`,
          scheme_name = `Name of Hospital site`,
          .keep = "none"
        ),
      by = dplyr::join_by(peer == scheme_code)
    ) |>
    dplyr::left_join(
      mitigator_lookup,
      by = dplyr::join_by(strategy == "Strategy variable")
    ) |>
    dplyr::left_join(
      nee_results,
      by = dplyr::join_by(strategy == param_name)
    )

  data_prepared <- data_joined |>
    dplyr::filter(
      (value_1 <= 1 & value_2 <= 1) |
        is.na(value_1) & is.na(value_2)
    ) |>
    dplyr::mutate(
      .keep = "none",
      # schemes
      scheme_name,
      scheme_code = peer,
      scheme_year = dplyr::if_else(
        stringr::str_detect(run_stage, "Final"),
        paste0(peer_year, "*"), peer_year
      ),
      # model run
      run_scenario = scenario,
      run_stage,
      # mitigators
      mitigator_code = `Mitigator code`,
      mitigator_name = `Strategy name`,
      mitigator_variable = strategy,
      mitigator_activity_type = `Activity type`,
      mitigator_type = `Mitigator type`,
      mitigator_group = Grouping,
      # mitigator value selections
      value_lo = value_1,
      value_hi = value_2,
      value_mid = value_lo + (value_hi - value_lo) / 2,
      value_range = value_hi - value_lo,
      value_point_or_range = dplyr::if_else(
        value_hi - value_lo == 0,
        "point",
        "range"
      ),
      value_time_profile = time_profile,
      # years
      year_baseline = baseline_year,
      year_horizon = horizon_year,
      year_range = year_horizon - year_baseline,
      # national elicitation exercise
      nee_p10 = percentile10,
      nee_p90 = percentile90,
      nee_p50 = nee_p10 - (nee_p10 - nee_p90) / 2,
      nee_mean = mean
    )

  data_prepared |>
    dplyr::select(
      tidyselect::starts_with("scheme"),
      tidyselect::starts_with("run"),
      tidyselect::starts_with("mitigator"),
      tidyselect::starts_with("value"),
      tidyselect::starts_with("year"),
      tidyselect::starts_with("nee")
    ) |>
    dplyr::arrange(scheme_name, mitigator_code)

}

get_all_schemes <- function(dat) {
  dat |>
    shiny::req() |>
    dplyr::distinct(scheme_name, scheme_code) |>
    dplyr::filter(!is.na(scheme_code)) |>
    dplyr::mutate(scheme_name = paste0(scheme_name, " (", scheme_code, ")")) |>
    dplyr::arrange(scheme_name) |>
    tibble::deframe()
}

get_all_mitigators <- function(dat) {
  dat |>
    shiny::req() |>
    dplyr::distinct(mitigator_name, mitigator_code) |>
    dplyr::filter(!is.na(mitigator_code)) |>
    dplyr::mutate(
      mitigator_name = paste0(mitigator_code, ": ", mitigator_name)
    ) |>
    dplyr::arrange(mitigator_code) |>
    tibble::deframe()
}

get_all_mitigator_groups <- function(dat) {
  dat |>
    shiny::req() |>
    dplyr::distinct(mitigator_group) |>
    dplyr::pull() |>
    sort()
}
