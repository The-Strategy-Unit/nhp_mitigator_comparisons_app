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
    mitigator_groups,
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
          scheme = `Trust ODS Code`,
          scheme_name = paste0(`Name of Hospital site`, " (", scheme, ")"),
          .keep = "none"
        ),
      by = dplyr::join_by(peer == scheme)
    ) |>
    dplyr::left_join(
      mitigator_lookup,
      by = dplyr::join_by(strategy == "Strategy variable")
    ) |>
    dplyr::left_join(
      nee_results,
      by = dplyr::join_by(strategy == param_name)
    ) |>
    dplyr::left_join(
      mitigator_groups |> dplyr::select("Mitigator code", Grouping),
      by = dplyr::join_by("Mitigator code")
    )

  data_augmented <- data_joined |>
    dplyr::filter(
      (value_1 <= 1 & value_2 <= 1) |
        is.na(value_1) & is.na(value_2)
    ) |>
    dplyr::mutate(
      years = horizon_year - baseline_year,
      midpoint = value_1 + (value_2 - value_1) / 2,
      percentile50 = percentile10 - (percentile10 - percentile90) / 2,
      point_or_range = dplyr::if_else(
        (value_2 - value_1) == 0,
        "point",
        "range"
      ),
      peer_year = dplyr::case_match(
        run_stage,
        "Final" ~ paste0(peer_year, "*"),
        .default = peer_year
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(
          value_1,
          value_2,
          midpoint,
          percentile10,
          percentile50,
          percentile90,
          mean
        ),
        \(x) janitor::round_half_up(x, 3)
      )
    )

  data_augmented |>
    dplyr::select(
      # schemes
      scheme_name,
      scheme_code = peer,
      peer_year,
      # run
      scenario,
      run_stage,
      # mitigators
      mitigator_code = `Mitigator code`,
      mitigator = strategy,
      mitigator_group = Grouping,
      parameter,
      activity_type,
      # mitigator selections
      lo = value_1,
      hi = value_2,
      mid = midpoint,
      point_or_range,
      time_profile,
      # years
      baseline_year,
      horizon_year,
      years,
      # NEE
      nee_p10 = percentile10,
      nee_p50 = percentile50,
      nee_p90 = percentile90,
      nee_mean = mean
    ) |>
    dplyr::arrange(scheme_name, mitigator_code)

}
