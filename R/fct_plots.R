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
#' 5. Show NEE reference range
#'    Allows the user to display the range of values from the National Elicitation
#'    Exercise (NEE) as context. Defaults to on.
#'
#' @param dat_selected_pointrange Tibble of mitigator data as produced by `populate_table()` in `fct_tabulate.R`
#' @param input Reference to the Shiny input widget that triggered this chart
#'
#' @return ggplot2 object showing point-range view facetted by scheme or mitigator
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

    # set char wrap to 100
    y_char_wrap <- 100

  } else {
    var_y_axis <- var_mitigator
    var_facet <- 'scheme_name'

    # where should character wrapping occur?
    y_max_char <- dat_selected_pointrange$mitigator_name |>
      as.character() |>
      nchar() |>
      max()

    y_char_wrap <- (y_max_char / 1.8) |>
      ceiling()
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
      labeller = ggplot2::label_wrap_gen(width = 20),
      ncol = input$facet_columns,
      scales = 'free_x' # add
    )


  ## geoms ----
  # add nee as first geom (to put behind pointrange)
  if (input$toggle_nee_reference_range & !input$toggle_horizon_pointrange) {
    pointrange <- pointrange +
      ggplot2::geom_crossbar(
        data = stats::na.omit(dat_selected_pointrange),
        ggplot2::aes(
          x = nee_mean,
          xmin = nee_p90,
          xmax = nee_p10
        ),
        #fill = "lightgrey",
        #colour = "grey85",
        alpha = 0.2,
        width = 0.4
      )
  }

  # add the point range
  pointrange <- pointrange +
    ggplot2::geom_pointrange()

  # set limits from 0 to 100% if not horizon standardised
  if (!input$toggle_horizon_pointrange) {
    pointrange <- pointrange +
      #ggplot2::xlim(0, 1)
      ggplot2::scale_x_continuous(
        labels = scales::label_percent(accuracy = 1),
        breaks = c(0.25, 0.5, 0.75),
        minor_breaks = c(0, 1),
        limits = c(0, 1)
      )
  } else {
    pointrange <- pointrange +
      ggplot2::scale_x_continuous(
        labels = scales::label_percent(accuracy = 1)
      )
  }

  # formatting and labels
  pointrange <- pointrange +
    ggplot2::scale_y_discrete(
      labels = \(.y) stringr::str_wrap(
        string = .y,
        width = y_char_wrap,
        whitespace_only = TRUE
      )
    ) +
    ggplot2::scale_color_manual(
      values = c("FALSE" = "black", "TRUE" = "red")
    ) +
    ggplot2::labs(x = "80% Prediction Interval") +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 12),
      legend.position = "none",
    )

  return(pointrange)
}


#' Heatmap plot
#'
#' Construct the heatmap showing values for mitigators when compared across
#' schemes.
#'
#' Options include:
#'
#' 1. Mitigator name
#'    Checkbox-controlled switch between mitigator code and mitigator name to
#'    help interpret the plots.
#'
#' 2. Value to plot
#'    Drop-down selection of binary (if a mitigator is used at all), value, low,
#'    high and range
#'
#' x-axis responsivity:
#' * <6 schemes selected - names are displayed vertically and wrapped at 20 char
#' * 6 <= x <12 schemes - names are displayed at 45 degrees with no wrap
#' * >= 12 schemes - names are rotated at 90 degrees
#'
#' y-axis:
#' * string-wrapped at around half the length of the longest mitigator name
#'
#' @param dat_selected_pointrange Tibble of mitigator data as produced by `populate_table()` in `fct_tabulate.R`
#' @param input Reference to the Shiny input widget that triggered this chart
#'
#' @return ggplot2 object showing heatmap
#' @export
plot_heatmap <- function(dat_selected_heatmap, input) {

  # gather information
  y_max_char = dat_selected_heatmap$mitigator_name |>
    as.character() |>
    nchar() |>
    max()

  y_char_wrap = (y_max_char / 1.8) |>
    ceiling()

  x_scheme_count <- dat_selected_heatmap$scheme_name |>
    unique() |>
    length()

  ## logic ----
  # decide whether to plot the mitigator code or name on the y-xais
  if (input$toggle_mitigator_name) {
    var_y_axis <- 'mitigator_name'
  } else {
    var_y_axis <- 'mitigator_code'
  }

  # convert to symbols - so can be used as variables in ggplot
  var_y_axis <- as.symbol(var_y_axis)

  # decide how scheme names should be plotted on the x-axis
  if (x_scheme_count < 6) {
    x_scheme_text <- ggplot2::element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5
    )
    x_scheme_wrap = 10
  } else if (x_scheme_count < 12) {
    x_scheme_text <- ggplot2::element_text(
      angle = 45,
      hjust = 0,
      vjust = -1
    )
    x_scheme_wrap = 100
  } else {
    x_scheme_text <- ggplot2::element_text(
      angle = 90,
      hjust = 0,
      vjust = -1
    )
    x_scheme_wrap = 100
  }

  # construct the heatmap plot
  heatmap <- dat_selected_heatmap |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = scheme_name,
        y = {{ var_y_axis }},
        fill = value
      )
    ) +
    ggplot2::geom_tile(colour = 'white') +
    ggplot2::scale_x_discrete(
      position = "top",
      labels = \(x) stringr::str_wrap(x, width = x_scheme_wrap),
      guide = ggplot2::guide_axis(angle = ggplot2::waiver())
    ) +
    ggplot2::scale_y_discrete(
      labels = \(.y) stringr::str_wrap(
        string = .y,
        width = y_char_wrap,
        whitespace_only = TRUE
      )
    ) +
    ggplot2::scale_fill_gradient(
      limits = c(0, 1),
      labels = scales::label_percent()
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = x_scheme_text,
      legend.title = ggplot2::element_text(
        size = 14,
        margin = ggplot2::margin(0, 1, 0, 0, 'cm')
      ),
      legend.position = 'bottom',
      legend.key.width = ggplot2::unit(3, 'cm')
    ) +
    ggplot2::labs(fill = '80% Prediction Interval')

  # handle non-binary plots
  if (input$heatmap_type != "value_binary") {
    heatmap <- heatmap +
      ggplot2::geom_text(
        ggplot2::aes(label = value |> scales::percent(accuracy = 1)),
        color = "white",
        size = 6
      )
  }

  # handle binary plots
  if (input$heatmap_type == "value_binary") {
    heatmap <- heatmap + ggplot2::theme(legend.position = "none")
  }

  heatmap

}


#' Mixture distribution / density plot
#'
#' Plot
#'
#' @param dat_selected_mixture_distributions Tibble - mixture distributions for all schemes for the selected mitigators - as produced by `dat_selected_mixture_distributions()` in  `app_server.R`
#' @param dat_filtered Tibble - of mitigator data as produced by `populate_table()` in `fct_tabulate.R`
#' @param dat_focal_scheme_code String - the `scheme_code` of the focal scheme
#' @param input Reference to the Shiny input widget that triggered this chart
#'
#' @return ggplot2 object showing the pointrange for the focal scheme in contrast with the mixture distributions for all schemes combined.
#' @export
plot_mixture_distributions <- function(
    dat_selected_mixture_distributions, # the pre-calculated mixture distributions for each mitigator (based on all schemes)
    dat_filtered, # the data filtered for scheme and mitigators
    dat_focal_scheme_code, # the focal scheme code
    input) {

  # logic ----
  # decide what to plot on the y-axis
  if (!input$toggle_mixture_distribution_ecdf) {
    var_y_axis <- 'pdf_value'

  } else {
    var_y_axis <- 'ecdf_value'

  }

  # convert to symbols - so can be used as variables in ggplot
  var_y_axis <- as.symbol(var_y_axis)

  # pre-processing ----
  # get reference lines from the mixture distributions
  ref_lines <- dat_selected_mixture_distributions |>
    dplyr::select(mitigator_name, mitigator_code, p10, p90, mu) |>
    dplyr::distinct()

  # get pointrange details
  focal_pointrange <- dat_filtered |>
    dplyr::filter(
      scheme_code %in% dat_focal_scheme_code,
      mitigator_name %in% ref_lines$mitigator_name
    ) |>
    # set the y-axis value to be half as high as the maximum pdf_value
    dplyr::left_join(
      y = dat_selected_mixture_distributions |>
        dplyr::summarise(
          y_mid_point = (max({{ var_y_axis }}, na.rm = TRUE) / 2),
          .by = mitigator_name
        ),
      by = 'mitigator_name'
    )

  # construct the plot ----
  plot <- dat_selected_mixture_distributions |>
    stats::na.omit() |>
    ggplot2::ggplot() +

    # plot mixture distribution - either PDF or ECDF
    ggplot2::geom_line(ggplot2::aes(x = q, y = {{ var_y_axis }}), colour = 'grey60') +
    ggplot2::geom_area(ggplot2::aes(x = q, y = {{ var_y_axis }}), alpha = 0.1) +

    # plot reference lines
    ggplot2::geom_rect(
      data = ref_lines,
      ggplot2::aes(xmin = p10, xmax = p90, ymin = 0, ymax = Inf),
      fill = 'turquoise', alpha = 0.08
    ) +
    ggplot2::geom_vline(
      data = ref_lines,
      ggplot2::aes(xintercept = mu),
      linetype = 'dashed'
    ) +
    ggplot2::geom_vline(
      data = ref_lines,
      ggplot2::aes(xintercept = p10),
      linetype = 'dotted'
    ) +
    ggplot2::geom_vline(
      data = ref_lines,
      ggplot2::aes(xintercept = p90),
      linetype = 'dotted'
    ) +

    # plot the focal scheme's range
    ggplot2::geom_pointrange(
      data = focal_pointrange,
      ggplot2::aes(
        y = y_mid_point,
        x = value_mid * 100,
        xmin = value_lo * 100,
        xmax = value_hi * 100
      ),
      colour = 'red', size = 1, linewidth = 1.5, alpha = 0.75
    ) +

    # produce the rest of the plot
    ggplot2::facet_wrap(
      facets = ggplot2::vars(mitigator_name),
      ncol = 1,
      scales = 'free'
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = '80% Prediction Interval'
    )

  ## geoms ----
  # add nee as first geom (to put behind pointrange)
  if (input$toggle_nee_reference_range_density) {
    plot <- plot +
      ggplot2::geom_crossbar(
        data = stats::na.omit(focal_pointrange),
        ggplot2::aes(
          y = y_mid_point,
          x = nee_mean * 100,
          xmin = nee_p90 * 100,
          xmax = nee_p10 * 100,
          width = y_mid_point / 5, # set width based on highest point on density
        ),
        #fill = "lightgrey",
        colour = "grey60",
        #width = 0.015,
        alpha = 0.6,
      )
  }

  return(plot)

}
