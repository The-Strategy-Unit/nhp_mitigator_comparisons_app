#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Prepare data ----

  ## Read data ----

  container_results <-
    get_container(container_name = Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS"))

  container_support <-
    get_container(container_name = Sys.getenv("AZ_STORAGE_CONTAINER_SUPPORT"))

  board <- pins::board_connect()
  params <- pins::pin_read(board, name = "matt.dray/nhp_tagged_runs_params")
  runs_meta <- pins::pin_read(board, name = "matt.dray/nhp_tagged_runs_meta")
  extracted_params <- extract_params(params, runs_meta)

  skeleton_table <- prepare_skeleton_table(extracted_params)

  trust_code_lookup <- container_support |>
    AzureStor::storage_read_csv("nhp-scheme-lookup.csv", show_col_types = FALSE)

  mitigator_lookup <- container_support |>
    AzureStor::storage_read_csv("mitigator-lookup.csv", show_col_types = FALSE)

  nee_results <- container_support |>
    AzureStor::storage_load_rds("nee_table.rds")

  peers <- container_support |>
    AzureStor::storage_load_rds("trust-peers.rds") |>
    dplyr::rename(scheme = procode)

  ## Prep data ----

  dat <- populate_table(
    skeleton_table,
    extracted_params,
    trust_code_lookup,
    mitigator_lookup,
    nee_results
  )

  all_schemes <- get_all_schemes(dat)
  all_mitigators <- get_all_mitigators(dat)
  all_mitigator_groups <- get_all_mitigator_groups(dat)

  # Reactives ----

  peer_set <- shiny::reactive({
    peers |>
      dplyr::filter(scheme == input$focus_scheme) |>
      dplyr::pull(peer)
  })

  dat_selected_pointrange <- shiny::reactive({

    shiny::validate(
      need(input$schemes, message = "Select at least one scheme.")
    )

    shiny::validate(
      need(input$mitigators, message = "Select at least one mitigator.")
    )

    if (input$toggle_horizon_pointrange) {
      dat <- dat |>
        dplyr::mutate(
          dplyr::across(
            c(value_lo, value_hi, value_mid),
            \(x) x / year_range
          )
        )
    }

    if (!input$toggle_invert_facets) {
      dat <- dat |> dplyr::mutate(scheme_name = forcats::fct_rev(scheme_name))
    }

    if (input$toggle_invert_facets) {
      dat <- dat |>
        dplyr::mutate(mitigator_code = forcats::fct_rev(mitigator_code))
    }

    dat |>
      dplyr::filter(
        scheme_code %in% input$schemes,
        mitigator_code %in% input$mitigators
      ) |>
      dplyr::mutate(
        point_colour = dplyr::if_else(
          scheme_code == input$focus_scheme,
          TRUE,
          FALSE
        )
      )

  })

  dat_selected_heatmap <- shiny::reactive({

    shiny::validate(
      need(input$schemes, message = "Select at least one scheme.")
    )

    shiny::validate(
      need(input$mitigators, message = "Select at least one mitigator.")
    )

    if (input$toggle_horizon_heatmap) {
      dat <- dat |>
        dplyr::mutate(
          dplyr::across(
            c(value_lo, value_hi, value_mid),
            \(x) x / year_range
          )
        )
    }

    dat |>
      dplyr::mutate(
        value_binary = dplyr::if_else(!is.na(value_lo), 1, 0),
        dplyr::across(
          c(value_lo, value_hi, value_mid, value_range),
          \(x) janitor::round_half_up(x, 3)
        ),
        mitigator_code = forcats::fct_rev(mitigator_code)
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

  ## Updates ----

  shiny::observe({
    shiny::updateSelectInput(
      session,
      "focus_scheme",
      choices = all_schemes,
      selected = all_schemes[1]
    )
  })

  shiny::observe({

    selected_schemes <- c(input$focus_scheme, peer_set()) |> sort()

    if (input$toggle_all_schemes) {
      selected_schemes <- all_schemes
    }

    shiny::updateSelectInput(
      session,
      "schemes",
      choices = all_schemes,
      selected = selected_schemes
    )

  })

  shiny::observe({
    shiny::updateSelectInput(
      session,
      "mitigator_groups",
      choices = all_mitigator_groups,
      selected = all_mitigator_groups[1]
    )
  })

  ## Events ----

  shiny::observeEvent(input$mitigator_groups, {

    mitigator_group_set <- dat |>
      dplyr::filter(mitigator_group == input$mitigator_groups) |>
      dplyr::distinct(mitigator_code) |>
      dplyr::pull()

    shiny::updateSelectInput(
      session,
      "mitigators",
      choices = all_mitigators,
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
    dat |> make_raw_dt()
  })

  output$mitigator_lookup_dt <- DT::renderDT({
    mitigator_lookup |> make_mitigator_dt()
  })

  output$scheme_lookup_dt <- DT::renderDT({
    trust_code_lookup |> make_scheme_dt()
  })

}
