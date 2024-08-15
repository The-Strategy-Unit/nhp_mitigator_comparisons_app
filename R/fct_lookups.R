make_raw_dt <- function(dat) {

  dat_prepared <- dat |>
    dplyr::filter(!is.na(lo)) |>  # only show rows where mitigator was used
    dplyr::mutate(
      dplyr::across(
        c(
          scheme_name,
          scheme_code,
          peer_year,
          scenario,
          run_stage,
          mitigator_code,
          mitigator,
          mitigator_group,
          parameter,
          activity_type,
          time_profile
        ),
        factor  # enables discrete selection in interactive table
      )
    ) |>
    dplyr::rename_with(\(x) {
      x |> stringr::str_to_sentence() |> stringr::str_replace_all("_", " ")
    }) |>
    dplyr::rename_with(\(x) x |> stringr::str_replace("Nee", "NEE")) |>
    dplyr::rename(
      `Mitigator variable` = Mitigator,
      `Mitigator type` = Parameter,
      Low = Lo,
      High = Hi,
      Midpoint = Mid,
      `Years in horizon` = Years,
    )

  dat_prepared |>
    DT::datatable(
      fillContainer = TRUE,
      filter = "top",
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "Bftp",
        buttons = list(
          list(
            extend = "csv",
            filename = paste0(Sys.Date(), "mitigator-comparison-data"),
            text = "Download (CSV)"
          )
        )
      )
    )

}

make_mitigator_dt <- function(mitigator_groups) {

  mitigator_groups_prepared <- mitigator_groups |>
    dplyr::select(
      `Mitigator code`,
      `Mitigator name` = `Strategy name`,
      `Mitigator variable` = `Strategy variable`,
      `Mitigator type`,
      `Activity type`,
      `Mitigator grouping` = Grouping
    ) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  mitigator_groups_prepared |>
    DT::datatable(
      fillContainer = TRUE,
      filter = "top",
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "Bftp",
        buttons = list(
          list(
            extend = "csv",
            filename = paste0(Sys.Date(), "mitigator-lookup"),
            text = "Download (CSV)"
          )
        )
      )
    )

}


make_scheme_dt <- function(trust_code_lookup) {

  schemes_prepared <- trust_code_lookup |>
    dplyr::mutate(
      `Scheme name` = `Name of Hospital site`,
      `Trust name` = `Name of Trust`,
      `Scheme code` = `Trust ODS Code`,
      across(c(`Scheme name`, `Trust name`, `Scheme code`), factor),
      .keep = "none"
    ) |>
    dplyr::arrange(`Scheme name`)

  schemes_prepared |>
    DT::datatable(
      fillContainer = TRUE,
      filter = "top",
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "Bftp",
        buttons = list(
          list(
            extend = "csv",
            filename = paste0(Sys.Date(), "mitigator-comparison-data"),
            text = "Download (CSV)"
          )
        )
      )
    )

}
