#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # External resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    # Application UI logic
    bslib::page_navbar(
      id = "page_navbar",
      title = "NHP mitigator comparisons",
      sidebar = bslib::sidebar(
        id = "sidebar",
        title = "Global settings",
        width = 400,
        bslib::accordion(
          id = "global_accordion",
          open = FALSE,
          bslib::accordion_panel(
            id = "accordion_schemes",
            title = "Select schemes",
            icon = bsicons::bs_icon("hospital"),
            shiny::selectInput(
              inputId = "focus_scheme",
              label = bslib::tooltip(
                trigger = list(
                  "Focal scheme",
                  bsicons::bs_icon("info-circle")
                ),
                "The scheme to highlight in plots. Causes autoselection of peers."
              ),
              choices = NULL,
              selected = NULL,
              multiple = FALSE
            ),
            shiny::selectInput(
              inputId = "schemes",
              label = bslib::tooltip(
                trigger = list(
                  "Schemes to visualise",
                  bsicons::bs_icon("info-circle")
                ),
                "Defaults to peers of the selected scheme if 'Select all schemes?' is unchecked."
              ),
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            shiny::checkboxInput(
              inputId = "toggle_all_schemes",
              label = bslib::tooltip(
                trigger = list(
                  "Select all schemes?",
                  bsicons::bs_icon("info-circle")
                ),
                "Automatically select all schemes at once."
              ),
              value = TRUE
            )
          ),
          bslib::accordion_panel(
            id = "accordion_mitigators",
            title = "Select mitigators",
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
        id = "nav_panel_info",
        title = "Information",
        bslib::navset_card_underline(
          id = "navset",
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
        id = "nav_panel_pointrange",
        title = "Point-ranges",
        bslib::card(
          full_screen = TRUE,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              title = "Point-range settings",
              open = TRUE,
              shiny::checkboxInput(
                inputId = "toggle_horizon_pointrange",
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
                inputId = "toggle_mitigator_code_pointrange",
                label = bslib::tooltip(
                  trigger = list(
                    "Show mitigator code?",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Replaces the full mitigator name with the mitigator code."
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
                value = 2,
                min = 1,
                max = 5,
                step = 1
              ),
              shiny::bookmarkButton(
                label = "Bookmark ",
                icon = shiny::icon("bookmark", lib = "glyphicon"),
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            ),
            shiny::plotOutput("pointrange"),
          )
        )
      ),
      bslib::nav_panel(
        id = "nav_panel_heatmaps",
        title = "Heatmaps",
        bslib::card(
          full_screen = TRUE,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              title = "Heatmap settings",
              open = TRUE,
              shiny::selectInput(
                inputId = "heatmap_type",
                label = bslib::tooltip(
                  trigger = list(
                    "Value type",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Schemes' low or high 80% confidence internal selection in the NHP inputs app, or the range or midpoint of these."
                ),
                choices = c(
                  Binary = "value_binary",
                  Midpoint = "value_mid",
                  Range = "value_range",
                  Low = "value_lo",
                  High = "value_hi"
                ),
                selected = "value_mid",
                multiple = FALSE
              ),
              shiny::checkboxInput(
                inputId = "toggle_horizon_heatmap",
                label = bslib::tooltip(
                  trigger = list(
                    "Standardise by horizon length?",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Divides the scheme's chosen mitigator values by the number of years between the chosen start and final year."
                ),
                value = FALSE
              ),
              shiny::bookmarkButton(
                label = "Bookmark",
                icon = shiny::icon("bookmark", lib = "glyphicon"),
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            ),
            shiny::plotOutput("heatmap"),
          )
        )
      ),
      bslib::nav_panel(
        id = "nav_panel_data",
        title = "Data",
        bslib::navset_card_underline(
          full_screen = TRUE,
          bslib::nav_panel(
            "Raw data",
            DT::DTOutput("raw_data_dt")
          ),
          bslib::nav_panel(
            "Mitigator lookup",
            DT::DTOutput("mitigator_lookup_dt")
          ),
          bslib::nav_panel(
            "Scheme lookup",
            DT::DTOutput("scheme_lookup_dt")
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
