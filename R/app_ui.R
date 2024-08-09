#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "NHP mitigator comparisons",
      window_title = "NHP mitigator comparisons",
      sidebar = bslib::sidebar(
        shiny::selectInput(
          inputId = "schemes",
          label = bslib::tooltip(
            trigger = list(
              "Select schemes",
              bsicons::bs_icon("info-circle")
            ),
            "Select one or more schemes"
          ),
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          inputId = "focus_scheme",
          label = "Select focal scheme",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        shiny::selectInput(
          inputId = "mitigator_groups",
          label = "Select mitigator group",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        shiny::selectInput(
          inputId = "mitigators",
          label = "Select mitigators",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        shiny::checkboxInput(
          inputId = "toggle_horizon",
          label = "Standardise by horizon length?",
          value = FALSE
        ),
        shiny::checkboxInput(
          inputId = "toggle_invert_facets",
          label = "Facet by scheme?",
          value = FALSE
        ),
        shiny::numericInput(
          inputId = "facet_rows",
          label = "Number of facet rows",
          value = 1,
          min = 1,
          max = 5,
          step = 1
        )
      ),
      bslib::nav_panel(
        title = "Information",
        bslib::navset_card_underline(
          bslib::nav_panel(
            title = "Introduction",
            htmltools::p("Information about the app.")
          ),
          bslib::nav_panel(
            title = "Notes",
            htmltools::p("Notes about use of the app.")
          )
        )
      ),
      bslib::nav_panel(
        title = "Peer comparison",
        bslib::navset_card_underline(
          bslib::nav_panel(
            title = "Inpatients",
            shiny::plotOutput("p")
          ),
          bslib::nav_panel(
            title = "Outpatients",
            htmltools::p("Placeholder for plot")
          ),
          bslib::nav_panel(
            title = "A&E",
            htmltools::p("Placeholder for plot")
          )
        )
      ),
      bslib::nav_panel(
        title = "Heatmaps",
        bslib::navset_card_underline(
          bslib::nav_panel(
            "Binary",
            htmltools::p("Placeholder for heatmap.")
          ),
          bslib::nav_panel(
            "Midpoints",
            htmltools::p("Placeholder for heatmap.")
          ),
          bslib::nav_panel(
            "Ranges",
            htmltools::p("Placeholder for heatmap.")
          )
        )
      ),
      bslib::nav_panel(
        title = "Data",
        bslib::navset_card_underline(
          bslib::nav_panel(
            "Raw data",
            htmltools::p("Placeholder for table.")
          ),
          bslib::nav_panel(
            "Mitigator lookup",
            htmltools::p("Placeholder for table.")
          ),
          bslib::nav_panel(
            "Scheme lookup",
            htmltools::p("Placeholder for table.")
          )
        )
      ),
      bslib::nav_spacer(),
      bslib::nav_item(bslib::input_dark_mode(mode = "light"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "nhp.inputs.report.app"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
