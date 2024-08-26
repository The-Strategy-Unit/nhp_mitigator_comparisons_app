make_raw_dt <- function(dat) {

  dat_prepared <- dat |>
    dplyr::filter(!is.na(value_lo)) |>  # only want mitigators selected by schemes
    dplyr::mutate(
      dplyr::across(
        c(
          tidyselect::starts_with("scheme"),
          tidyselect::starts_with("run"),
          tidyselect::starts_with("mitigator"),
          value_point_or_range,
          value_time_profile
        ),
        factor  # enables discrete selection in interactive table
      )
    )

  dat_prepared |>
    DT::datatable(
      fillContainer = TRUE,
      filter = "top",
      rownames = FALSE,
      selection = "none",
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
      ),
      callback = DT::JS(
        "$('button.buttons-csv').css('background','#337ab7');
        $('button.buttons-csv').css('border','#2e6da4');
        return table;"
      )
    ) |>
    DT::formatRound(  # present to 3 dp, download will have unrestricted dp
      columns = c(
        "value_lo", "value_hi", "value_mid", "value_range",
        "nee_p10", "nee_p90", "nee_p50", "nee_mean"
      ),
      digits = 3
    )

}

make_mitigator_dt <- function(mitigator_lookup) {

  mitigator_lookup_prepared <- mitigator_lookup |>
    dplyr::select(
      `Mitigator code`,
      `Mitigator name` = `Strategy name`,
      `Mitigator variable` = `Strategy variable`,
      `Mitigator type`,
      `Activity type`,
      `Mitigator grouping` = Grouping
    ) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  mitigator_lookup_prepared |>
    DT::datatable(
      fillContainer = TRUE,
      filter = "top",
      rownames = FALSE,
      selection = "none",
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
      ),
      callback = DT::JS(
        "$('button.buttons-csv').css('background','#337ab7');
        $('button.buttons-csv').css('border','#2e6da4');
        return table;"
      )
    )

}


make_scheme_dt <- function(trust_code_lookup) {

  schemes_prepared <- trust_code_lookup |>
    dplyr::mutate(
      `Scheme name` = `Name of Hospital site`,
      `Trust name` = `Name of Trust`,
      `Scheme code` = `Trust ODS Code`,
      .keep = "none"
    ) |>
    dplyr::mutate(across(tidyselect::everything(), factor)) |>
    dplyr::arrange(`Scheme name`)

  schemes_prepared |>
    DT::datatable(
      fillContainer = TRUE,
      filter = "top",
      rownames = FALSE,
      selection = "none",
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
      ),
      callback = DT::JS(
        "$('button.buttons-csv').css('background','#337ab7');
        $('button.buttons-csv').css('border','#2e6da4');
        return table;"
      )
    )

}
