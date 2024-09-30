#' Pointrange plots
#'
#' Construct the point-range
#'
#' Options include:
#' 1. Standardised horizon
#'    Divides the mitigator by years from start to final year.
#'
#' 2. Mitigator code
#'    Replaces the mitigator name with the mitigator code - to reduce visual
#'    cutter.
#'
#' 3. Facet by scheme
#'    By default the mitigators are faceted with schemes on the y-axis. This
#'    option plots the mitigators on the y-axis instead and the plot is faceted
#'    by scheme.
#'
#' 4. Facet rows
#'    Allows the user to define how many rows to facet the plot over.
#'
#' @param dat_selected_pointrange Tibble of mitigator data as produced by `populate_table()` in `fct_tabulate.R`
#' @param input Reference to the Shiny input widget that triggered this chart
#'
#' @return ggplot2 object showing point-range view
#' @export
plot_pointrange <- function(dat_selected_pointrange, input) {

  ## logic ----
  # decide what to show for the mitigator
  if (input$toggle_mitigator_code_pointrange) {
    var_mitigator <- 'mitigator_code'
  } else {
    var_mitigator <- 'mitigator_name'
  }

  # decide what to plot on the y-axis and facets
  if (!input$toggle_invert_facets) {
    var_y_axis <- 'scheme_name'
    var_facet <- var_mitigator
  } else {
    var_y_axis <- var_mitigator
    var_facet <- 'scheme_name'
  }

  # convert to symbols - so can be used as variables in ggplot
  var_facet <- as.symbol(var_facet)
  var_y_axis <- as.symbol(var_y_axis)

  ## aesthetics ----
  # set aesthetics for all plot variants
  pointrange <- dat_selected_pointrange |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = value_mid,
        xmin = value_lo,
        xmax = value_hi,
        colour = point_colour
      )
    )

  # add calculated vars to the plot aesthetics
  pointrange <- pointrange +
    ggplot2::aes(y = {{var_y_axis}}) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars( {{var_facet}} ),
      labeller = ggplot2::label_wrap_gen(width = 20)
    )

  ## geoms ----
  # add nee as first geom (to put behind pointrange)
  if (input$toggle_nee_reference_range) {
    pointrange <- pointrange +
      ggplot2::geom_crossbar(
        ggplot2::aes(
          x = nee_mean,
          xmin = nee_p90,
          xmax = nee_p10
        ),
        fill = "lightgrey",
        colour = "grey85",
        alpha = 0.2,
        width = 0.4
      )
  }

  # add the point range
  pointrange <- pointrange +
    ggplot2::geom_pointrange()

  # set limits from 0 to 100% if not horizon standardised
  if (!input$toggle_horizon_pointrange) {
    pointrange <- pointrange + ggplot2::xlim(0, 1)
  }

  # formatting and labels
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

}


plot_heatmap <- function(dat_selected_heatmap, input) {

  heatmap <- dat_selected_heatmap |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = scheme_name,
        y = mitigator_code,
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

  if (input$heatmap_type != "value_binary") {
    heatmap <- heatmap +
      ggplot2::geom_text(
        ggplot2::aes(label = value),
        color = "white",
        size = 6
      )
  }

  if (input$heatmap_type == "value_binary") {
    heatmap <- heatmap + ggplot2::theme(legend.position = "none")
  }

  heatmap

}
