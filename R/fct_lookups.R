make_mitigator_lookup <- function(mitigator_groups) {

  mitigator_groups_prepared <- mitigator_groups |>
    dplyr::filter(!`Mitigator type` == "Efficiencies") |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  mitigator_groups_prepared |>
    DT::datatable(
      options = list(
        dom = 'ftp',
        pageLength = 5
      ),
      filter = "top"
    )

}

make_raw_dt <- function(dat) {

  dat_prepared <- dat |>
    dplyr::filter(!is.na(value_1)) |>  # only show rows where mitigator was used
    dplyr::mutate(
      dplyr::across(
        c(
          activity_type,
          strategy,
          time_profile,
          parameter,
          peer,
          peer_year,
          `Mitigator code`,
          Grouping,
          point_or_range,
          scenario,
          run_stage
        ),
        factor
      )
    ) |>
    dplyr::select(
      # Scheme info
      "Scheme code" = peer,
      # Run info
      Scenario = scenario,
      "Run stage" = run_stage,
      "Baseline year" = baseline_year,
      "Horizon year" = horizon_year,
      # Mitigator info
      "Mitigator code",
      Mitigator = strategy,
      "Mitigator group" = Grouping,
      "Activity type" = activity_type,
      "Mitigator type" = parameter,
      Low = value_1,
      High = value_2,
      Midpoint = midpoint,
      point_or_range,
      "Time profile" = time_profile,
      # NEE
      "NEE p10" = percentile10,
      "NEE p50" = percentile50,
      "NEE p90" = percentile90,
      "NEE mean" = mean
    )

  dat_prepared |>
    DT::datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bftp',
        pageLength = 5,
        buttons = list(
          list(
            extend = "csv",
            filename = "nhp_inputs_raw_data",
            text = "Download Raw Data (.csv)"
          )
        )
      ),
      filter = "top",
      rownames = FALSE
    )

}

make_trust_code_lookup <- function(dat, trust_code_lookup) {

  trust_code_lookup_prepared <- trust_code_lookup |>
    dplyr::select(
      `Name of Trust`,
      `Trust ODS Code`,
      `Name of Hospital site`
    )

  dat_prepared <- dat |>
    dplyr::select(
      peer,
      peer_year,
      baseline_year,
      horizon_year,
      scenario,
      run_stage
    ) |>
    dplyr::filter(!is.na(peer)) |>
    dplyr::group_by(dplyr::pick(tidyselect::everything())) |>
    dplyr::summarise(tmp = dplyr::n()) |>
    dplyr::select(-tmp) |>
    dplyr::left_join(
      trust_code_lookup_prepared,
      by = c(peer = "Trust ODS Code")
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "Scheme code" = peer,
      "Site name" = "Name of Hospital site",
      Scenario = scenario,
      "Run stage" = run_stage,
      "Baseline year" = baseline_year,
      "Horizon year" = horizon_year
    ) |>
    dplyr::arrange(`Site name`) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  dat_prepared |>
    DT::datatable(
      options = list(
        dom = 'ftp',
        pageLength = 5
      ),
      filter = "top"
    )

}


