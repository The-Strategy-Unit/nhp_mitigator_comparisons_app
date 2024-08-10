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
        title = "Global settings",
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel(
            "Select schemes",
            icon = bsicons::bs_icon("hospital"),
            shiny::selectInput(
              inputId = "schemes",
              label = bslib::tooltip(
                trigger = list(
                  "Schemes to visualise",
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
              label = bslib::tooltip(
                trigger = list(
                  "Focal scheme",
                  bsicons::bs_icon("info-circle")
                ),
                "The scheme to highlight in plots. Must be one of the schemes selected for display."
              ),
              choices = NULL,
              selected = NULL,
              multiple = FALSE
            )
          ),
          bslib::accordion_panel(
            "Select mitigators",
            icon = bsicons::bs_icon("sliders"),
            shiny::selectInput(
              inputId = "mitigator_groups",
              label = bslib::tooltip(
                trigger = list(
                  "Mitigator group",
                  bsicons::bs_icon("info-circle")
                ),
                "Select a group to pre-populate the mitigator selection box."
              ),
              choices = NULL,
              selected = NULL,
              multiple = FALSE
            ),
            shiny::selectInput(
              inputId = "mitigators",
              label = bslib::tooltip(
                trigger = list(
                  "Mitigators to visualise",
                  bsicons::bs_icon("info-circle")
                ),
                "Prepopulated given the mitigator group selection, but you can add or remove individual mitigators."
              ),
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            )
          )
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
        title = "Point-ranges",
        bslib::card(
          full_screen = TRUE,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              title = "Plot settings",
              open = TRUE,
              shiny::checkboxInput(
                inputId = "toggle_horizon",
                label = bslib::tooltip(
                  trigger = list(
                    "Standardise by horizon length?",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Divides the scheme's chosen mitigator values by the number of years between the chosen start and final year."
                ),
                value = FALSE
              ),
              shiny::checkboxInput(
                inputId = "toggle_invert_facets",
                label = bslib::tooltip(
                  trigger = list(
                    "Facet by scheme?",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Invert the pointrange plots to show mitigators on the y axis and scheme as the faceting variable."
                ),
                value = FALSE
              ),
              shiny::numericInput(
                inputId = "facet_rows",
                label = bslib::tooltip(
                  trigger = list(
                    "Number of facet rows",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Choose the number of rows over which to break the faceted pointrange charts."
                ),
                value = 1,
                min = 1,
                max = 5,
                step = 1
              )
            ),
            shiny::plotOutput("p"),
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
