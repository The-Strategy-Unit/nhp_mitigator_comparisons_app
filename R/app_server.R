#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Reactives (Azure) ----

  if (getOption("golem.app.prod")) {  # avoids Azure calls in dev

    container <- reactive({
      get_container()  # relies on .Renviron variables
    })

    runs_meta <- reactive({
      fetch_labelled_runs_meta(container())  # exposes run_stage metadata
    })

    params <- reactive({
      fetch_labelled_runs_params(runs_meta(), container())  # final, intermediate, initial
    })

    extracted_params <- reactive({
      extract_params(params(), runs_meta())
    })

  }

  # Reactives (data read) ----

  all_params <- reactive({
    app_sys("app", "data", "all_params.json") |> jsonlite::fromJSON()
  })

  mitigator_lookup <- reactive({
    app_sys("app", "data", "mitigator_name_lookup.csv") |>
      read_mitigator_lookup()
  })

  mitigator_groups <- reactive({
    app_sys("app", "data", "mitigator groupings.xlsx") |>
      read_mitigator_groups()
  })

  nee <- reactive({
    app_sys("app", "data", "nee_table.rds") |> read_nee()
  })

  trust_code_lookup <- reactive({
    app_sys("app", "data", "NHP_trust_code_lookup.xlsx") |> readxl::read_excel()
  })


  # Reactives (data prep) ----

  if (getOption("golem.app.prod")) {  # avoids Azure calls in dev

    skeleton_table <- reactive({
      extracted_params() |> prepare_skeleton_table()
    })

    dat <- reactive({
      # TODO: probably update populate_table() with the extra wrangling below?
      populate_table(
        skeleton_table(),
        extracted_params(),
        mitigator_lookup(),
        mitigator_groups(),
        nee()
      ) |>
        dplyr::left_join(
          trust_code_lookup() |>
            dplyr::mutate(
              scheme = `Trust ODS Code`,
              scheme_name = paste0(`Name of Hospital site`, " (", scheme, ")"),
              .keep = "none"
            ),
          by = "scheme"
        ) |>
        dplyr::rename(
          mitigator = strategy,
          mitigator_group = Grouping,
          lo = value_1,
          hi = value_2,
          mid = midpoint,
        ) |>
        dplyr::mutate(years = horizon_year - baseline_year)
    })

  } else if (!getOption("golem.app.prod")) {  # read demo data in dev
    dat <- reactive({
      app_sys("app", "data", "demo-dat.csv") |>
        readr::read_csv(show_col_types = FALSE)
    })
  }

  peers <- reactive({
    app_sys("app", "data", "peers.rds") |>  # from 'Trust Peer Finder Tool' online
      readr::read_rds() |>
      dplyr::rename(scheme = procode)
  })

  peer_set <- shiny::reactive({
    peers() |>
      dplyr::filter(scheme == input$focus_scheme) |>
      dplyr::pull(peer)
  })

  all_schemes <- shiny::reactive({
    dat() |>
      shiny::req() |>
      dplyr::distinct(scheme_name, scheme) |>
      dplyr::arrange(scheme_name) |>
      tibble::deframe()  # named vector: value is code, name is scheme name
  })

  all_mitigators <- shiny::reactive({
    dat() |>
      shiny::req() |>
      dplyr::distinct(mitigator) |>
      dplyr::pull() |>
      sort()
  })

  all_mitigator_groups <- shiny::reactive({
    dat() |>
      shiny::req() |>
      dplyr::distinct(mitigator_group) |>
      dplyr::pull() |>
      sort()
  })

  # Reactives (data selection) ----

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
        dplyr::mutate(dplyr::across(c(lo, hi, mid), \(x) x / years))
    }

    dat |>
      dplyr::filter(
        scheme %in% input$schemes,
        mitigator %in% input$mitigators
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
        dplyr::mutate(dplyr::across(c(lo, hi, mid), \(x) x / years))
    }

    dat |>
      dplyr::mutate(
        range = hi - lo,
        binary = dplyr::if_else(!is.na(lo), 1, 0),
      ) |>
      dplyr::mutate(
        across(
          c(lo, hi, mid, range),
          \(x) janitor::round_half_up(x, 3)
        )
      ) |>
      tidyr::pivot_longer(
        c(lo, hi, mid, range, binary),
        names_to = "type",
        values_to = "value"
      ) |>
      dplyr::filter(
        type == input$heatmap_type,
        scheme %in%  input$schemes,
        mitigator %in% input$mitigators,
      )

  })

  # Observers ----

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


  shiny::observe({

    if (input$heatmap_type == "binary") {
      shinyjs::disable("toggle_horizon_heatmap")
    }

    if (input$heatmap_type != "binary") {
      shinyjs::enable("toggle_horizon_heatmap")
    }

  })

  shiny::observeEvent(input$mitigator_groups, {

    mitigator_group_set <- dat() |>
      dplyr::filter(mitigator_group == input$mitigator_groups) |>
      dplyr::distinct(mitigator) |>
      dplyr::pull()

    shiny::updateSelectInput(
      session,
      "mitigators",
      choices = all_mitigators(),
      selected = mitigator_group_set
    )

  })

  # Renders

  output$pointrange <- shiny::renderPlot({

    pointrange <- dat_selected_pointrange() |>
      dplyr::mutate(
        point_colour = dplyr::if_else(
          scheme == input$focus_scheme,
          TRUE,
          FALSE
        )
      ) |>
      ggplot2::ggplot()

    if (!input$toggle_invert_facets) {
      pointrange <- pointrange +
        ggplot2::geom_pointrange(
          ggplot2::aes(
            x = mid, y = scheme_name,
            xmin = lo, xmax = hi,
            colour = point_colour
          )
        ) +
        ggplot2::facet_wrap(~mitigator, nrow = input$facet_rows)
    }

    if (input$toggle_invert_facets) {
      pointrange <- pointrange +
        ggplot2::geom_pointrange(
          ggplot2::aes(
            x = mid, y = mitigator,
            xmin = lo, xmax = hi,
            colour = point_colour
          )
        ) +
        ggplot2::facet_wrap(~scheme, nrow = input$facet_rows)
    }

    if (!input$toggle_horizon_pointrange) {
      pointrange <- pointrange + ggplot2::xlim(0, 1)  # should be fixed if raw
    }

    pointrange +
      ggplot2::scale_color_manual(
        values = c("FALSE" = "black", "TRUE" = "red")
      ) +
      ggplot2::labs(x = "80% Prediction Interval") +
      ggplot2::theme_bw(base_size = 20) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        legend.position = "none"
      )

  })

  output$heatmap <- shiny::renderPlot({

    heatmap <-  dat_selected_heatmap() |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = scheme_name,
          y = mitigator,
          fill = value
        )
      ) +
      ggplot2::geom_tile() +
      ggplot2::scale_x_discrete(
        position = "top",
        labels = \(x) stringr::str_wrap(x, width = 10)
      ) +
      ggplot2::theme_minimal(base_size = 20) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      )

    if (input$heatmap_type != "binary") {
      heatmap <- heatmap +
        ggplot2::geom_text(
          ggplot2::aes(label = value),
          color = "white",
          size = 6
        )
    }

    if (input$heatmap_type == "binary") {
      heatmap <- heatmap + ggplot2::theme(legend.position = "none")
    }

    heatmap

  })

}
