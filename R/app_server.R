#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Reactives ----

  ## Results (Azure) ----

  if (getOption("golem.app.prod")) {

    # In production, files are read from Azure

    container_results <- reactive({
      get_container(container_name = Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS"))
    })

    runs_meta <- reactive({
      fetch_labelled_runs_meta(container_results())
    })

    params <- reactive({
      fetch_labelled_runs_params(runs_meta(), container_results())
    })

    extracted_params <- reactive({
      extract_params(params(), runs_meta())
    })

    container_support <- reactive({
      get_container(container_name = Sys.getenv("AZ_STORAGE_CONTAINER_SUPPORT"))
    })

    # all_params <- reactive({
    #   temp_file <- withr::local_tempfile(fileext = ".json")
    #   AzureStor::download_blob(container_support(), "all_params.json", temp_file)
    #   jsonlite::read_json(temp_file, simplifyVector = FALSE)
    # })

    mitigator_lookup <- reactive({
      container_support() |>
        AzureStor::storage_read_csv(
          "mitigator-lookup.csv",
          show_col_types = FALSE
        )
    })

    nee_results <- reactive({
      container_support() |> AzureStor::storage_load_rds("nee_table.rds")
    })

    trust_code_lookup <- reactive({
      container_support() |>
        AzureStor::storage_read_csv(
          "nhp-scheme-lookup.csv",
          show_col_types = FALSE
        )
    })

    skeleton_table <- reactive({
      extracted_params() |> prepare_skeleton_table()
    })

    dat <- reactive({
      populate_table(
        skeleton_table(),
        extracted_params(),
        trust_code_lookup(),
        mitigator_lookup(),
        nee_results()
      )
    })

    peers <- reactive({
      container_support() |>
        AzureStor::storage_load_rds("trust-peers.rds") |>
        dplyr::rename(scheme = procode)
    })

  }

  ## Results (local) ----

  if (!getOption("golem.app.prod")) {

    # In dev, these files must be stored in inst/app/data/

    dat <- reactive({
      app_sys("app", "data", "demo-dat.csv") |>
        readr::read_csv(show_col_types = FALSE)
    })

    peers <- reactive({
      app_sys("app", "data", "trust-peers.rds") |>
        readr::read_rds() |>
        dplyr::rename(scheme = procode)
    })

  }


  ## Prep data ----

  peer_set <- shiny::reactive({
    peers() |>
      dplyr::filter(scheme == input$focus_scheme) |>
      dplyr::pull(peer)
  })

  all_schemes <- shiny::reactive({
    dat() |>
      shiny::req() |>
      dplyr::distinct(scheme_name, scheme_code) |>
      dplyr::filter(!is.na(scheme_code)) |>
      dplyr::mutate(scheme_name = paste0(scheme_name, " (", scheme_code, ")")) |>
      dplyr::arrange(scheme_name) |>
      tibble::deframe()  # named vector: value is code, name is scheme name
  })

  all_mitigators <- shiny::reactive({
    dat() |>
      shiny::req() |>
      dplyr::distinct(mitigator_name, mitigator_code) |>
      dplyr::filter(!is.na(mitigator_code)) |>
      dplyr::mutate(
        mitigator_name = paste0(mitigator_code, ": ", mitigator_name)
      ) |>
      dplyr::arrange(mitigator_code) |>
      tibble::deframe()
  })

  all_mitigator_groups <- shiny::reactive({
    dat() |>
      shiny::req() |>
      dplyr::distinct(mitigator_group) |>
      dplyr::pull() |>
      sort()
  })

  ## Select data ----

  dat_selected_pointrange <- shiny::reactive({

    shiny::validate(
      need(input$schemes, message = "Select at least one scheme.")
    )

    shiny::validate(
      need(input$mitigators, message = "Select at least one mitigator.")
    )

    dat <- dat()

    if (input$toggle_horizon_pointrange) {
      dat <- dat |>
        dplyr::mutate(
          dplyr::across(
            c(value_lo, value_hi, value_mid),
            \(x) x / year_range
          )
        )
    }

    dat |>
      dplyr::filter(
        scheme_code %in% input$schemes,
        mitigator_code %in% input$mitigators
      )

  })

  dat_selected_heatmap <- shiny::reactive({

    shiny::validate(
      need(input$schemes, message = "Select at least one scheme.")
    )

    shiny::validate(
      need(input$mitigators, message = "Select at least one mitigator.")
    )

    dat <- dat()

    if (input$toggle_horizon_heatmap) {
      dat <- dat |>
        dplyr::mutate(
          dplyr::across(
            c(value_lo, value_hi, value_mid),
            \(x) x / year_range)
        )
    }

    dat |>
      dplyr::mutate(
        value_binary = dplyr::if_else(!is.na(value_lo), 1, 0),
        dplyr::across(
          c(value_lo, value_hi, value_mid, value_range),
          \(x) janitor::round_half_up(x, 3)
        )
      ) |>
      tidyr::pivot_longer(
        c(value_lo, value_hi, value_mid, value_range, value_binary),
        names_to = "value_type",
        values_to = "value"
      ) |>
      dplyr::filter(
        value_type == input$heatmap_type,
        scheme_code %in%  input$schemes,
        mitigator_code %in% input$mitigators,
      )

  })

  # Observers ----

  ## Update inputs ----

  shiny::observe({
    shiny::updateSelectInput(
      session,
      "focus_scheme",
      choices = all_schemes(),
      selected = all_schemes()[1]
    )
  })

  shiny::observe({
    shiny::updateSelectInput(
      session,
      "schemes",
      choices = all_schemes(),
      selected = c(input$focus_scheme, peer_set()) |> sort()
    )
  })

  shiny::observe({
    shiny::updateSelectInput(
      session,
      "mitigator_groups",
      choices = all_mitigator_groups(),
      selected = all_mitigator_groups()[1]
    )
  })

  ## Events ----

  shiny::observeEvent(input$mitigator_groups, {

    mitigator_group_set <- dat() |>
      dplyr::filter(mitigator_group == input$mitigator_groups) |>
      dplyr::distinct(mitigator_code) |>
      dplyr::pull()

    shiny::updateSelectInput(
      session,
      "mitigators",
      choices = all_mitigators(),
      selected = mitigator_group_set
    )

  })

  ## Enablers ----

  shiny::observe({

    if (input$heatmap_type == "binary") {
      shinyjs::disable("toggle_horizon_heatmap")
    }

    if (input$heatmap_type != "binary") {
      shinyjs::enable("toggle_horizon_heatmap")
    }

  })

  # Renders ----

  ## Plots ----

  output$pointrange <- shiny::renderPlot({
    dat_selected_pointrange() |> plot_pointrange(input)
  })

  output$heatmap <- shiny::renderPlot({
    dat_selected_heatmap() |> plot_heatmap(input)
  })

  ## Tables ----

  output$raw_data_dt <- DT::renderDT({
    dat() |> make_raw_dt()
  })

  output$mitigator_lookup_dt <- DT::renderDT({
    mitigator_lookup() |> make_mitigator_dt()
  })

  output$scheme_lookup_dt <- DT::renderDT({
    trust_code_lookup() |> make_scheme_dt()
  })

}
