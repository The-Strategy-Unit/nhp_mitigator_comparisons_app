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
      facets = ggplot2::vars( forcats::fct_rev( {{var_facet}} )),
      labeller = ggplot2::label_wrap_gen(width = 20),
      ncol = input$facet_columns,
      scales = 'free_x' # add
    )


  ## geoms ----
  # add nee as first geom (to put behind pointrange)
  if (input$toggle_nee_reference_range) {
    pointrange <- pointrange +
      ggplot2::geom_crossbar(
        # limit to records with nee data
        data = dat_selected_pointrange |>
          dplyr::filter(!is.na(nee_mean) & !is.na(nee_p90) & !is.na(nee_p10)),
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

  # set limits from 0 to 100%
  pointrange <- pointrange +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = c(0.25, 0.5, 0.75),
      minor_breaks = c(0, 1),
      limits = c(0, 1)
    )

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
      #values = c("FALSE" = "black", "TRUE" = "red")
      values = c(
        'black' = 'black',
        'red' = 'red',
        'blue' = '#337ab7'
      )
    ) +
    ggplot2::labs(x = input$values_displayed) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 12),
      legend.position = "none",
    )

  return(pointrange)
}


#' Set breaks for a percent value
#'
#' An internal function to produce ggplot breaks on 0.01 (1%) intervals.
#' Based on code found here:
#' https://jhrcook.github.io/jhrcook-website/posts/2019-11-09_integer-values-ggplot-axis/
#'
#' @param n integer - the approximate number of breaks to set (defaults to 5)
#' @param ... other parameters, not yet defined
#'
#' @return ggplot2::breaks object
percent_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x * 100, n, ...)) / 100
    names(breaks) <- attr(breaks, 'labels')
    breaks
  }
  return(fxn)
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
    ggplot2::labs(fill = input$values_displayed)

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
#' Plot the mixutre distribution plots.
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
      x = input$values_displayed
    )

  ## geoms ----
  # add nee as first geom (to put behind pointrange)
  if (input$toggle_nee_reference_range_density) {

    # Note - suppressing a warning about width being an unknown aesthetic
    # this is tracked in issue #82
    suppressWarnings({
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
    })

  }

  return(plot)

}



#' Plot the baseline comparison
#'
#' Show a scatter plot with baseline rate on the x-axis and mitigator value on
#' the y-axis.
#'
#' @param dat Tibble - mitigator data as produced by `populate_table()` in `fct_tabulate.R`
#' @param rates_data Tibble - historical rates data for schemes / mitigators
#' @param mitigator_codes Character vector - the mitigator codes to visualise
#' @param focal_scheme_code Character vector - the scheme code for the focal scheme
#' @param rate_title Character - the label for the x-axis
#' @param value_title Character - the label for the y-axis, should reflect the user's preference in input$values_displayed
#' @param trendline Boolean (default = TRUE) show a trendline between mid-points
#' @param range Boolean (default = TRUE) show the 10% and 90% range as points connected by a line
#' @param scheme_label Boolean (default = TRUE) label the mid-points with the scheme code
#' @param quadrants Boolean (default = TRUE) show the lines splitting the data into quadrants
#' @param facet_columns Integer (default = 1) the number of columns to facet the baseline plot
#' @param facet_height_px Integer (default = 250) the pixel height of each facet in the plot
#'
#' @returns {plotly} plot
plot_baseline_comparison <- function(
    dat, rates_data, mitigator_codes, focal_scheme_code,
    rate_title = 'Baseline rate',
    value_title = 'Percent mitigated',
    trendline = TRUE,
    range = TRUE,
    scheme_label = TRUE,
    quadrants = TRUE,
    facet_columns = 1,
    facet_height_px = 250
) {

  # prepare the rates data
  rates_data <-
    rates_data |>
    dplyr::mutate(
      baseline_year = stringr::str_sub(
        string = fyear,
        start = 1L,
        end = 4L
      ) |> as.integer()
    ) |>
    dplyr::select(procode, strategy, baseline_year, rate_baseline = rate)

  # prepare the data for plotting
  plot_dat <-
    dat |>
    # limit to the selected mitigators
    dplyr::filter(
      mitigator_code %in% mitigator_codes
    ) |>
    # add in the baseline rate
    dplyr::left_join(
      y = rates_data,
      by = dplyr::join_by(
        scheme_code == procode,
        mitigator_variable == strategy,
        year_baseline == baseline_year
      )
    ) |>
    # avoid console errors by limiting to records with mitigator value AND rate values
    dplyr::filter(
      !is.na(value_mid),
      !is.na(rate_baseline)
    ) |>
    # work out quadrant
    dplyr::mutate(
      y_mid = mean(value_mid, na.rm = TRUE),
      x_mid = mean(rate_baseline, na.rm = TRUE),
      quad_baseline = ifelse(rate_baseline <= x_mid, 'Low baseline', 'High baseline'),
      quad_reduction = ifelse(value_mid <= y_mid, 'Low reduction', 'High reduction'),
      .by = mitigator_code
    ) |>
    # prepare for use
    dplyr::mutate(
      # highlight the focal scheme
      scheme_highlight = scheme_code == focal_scheme_code,

      # set the tooltip text
      tooltip_text = glue::glue(
        '<b>{scheme_name}</b> ({scheme_code})\n',
        '<i>low:</i> <b>{round(value_lo, 3)*100}</b>% ',
        '<i>mid:</i> <b>{round(value_mid, 3)*100}</b>% ',
        '<i>upp:</i> <b>{round(value_hi, 3)*100}</b>%\n',
        '<i>Baseline rate:</i> {round(rate_baseline, 2)} in year {year_baseline}\n',
        '{stringr::str_wrap(mitigator_activity_title, width = 50)}\n',
        '<i>Quadrant:</i> {quad_baseline} / {quad_reduction}'
      ) |>
        as.character()
    )

  plot_quadrants <-
    plot_dat |>
    # divide into quadrants
    dplyr::summarise(
      # where are the mid-values?
      y_mid = mean(value_mid, na.rm = TRUE),
      x_mid = mean(rate_baseline, na.rm = TRUE),
      # where are the bounds?
      y_min = min(value_mid, na.rm = TRUE) |>
        magrittr::multiply_by(10) |>
        floor() |>
        magrittr::divide_by(10),
      x_min = min(rate_baseline, na.rm = TRUE) |>
        magrittr::multiply_by(10) |>
        floor() |>
        magrittr::divide_by(10),
      y_max = max(value_mid, na.rm = TRUE) |>
        magrittr::multiply_by(10) |>
        ceiling() |>
        magrittr::divide_by(10),
      x_max = max(rate_baseline, na.rm = TRUE) |>
        magrittr::multiply_by(10) |>
        ceiling() |>
        magrittr::divide_by(10),
      .by = mitigator_name
    )

  # create the plot ---
  plot <-
    plot_dat |>
    ggplot2::ggplot(ggplot2::aes(x = rate_baseline, text = tooltip_text)) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(mitigator_name),
      scales = 'free_x',
      ncol = facet_columns
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      limits = c(0,1)
    )

  # add in quadrants?
  if (quadrants) {
    plot <-
      plot +
      ggplot2::geom_hline(
        data = plot_quadrants,
        ggplot2::aes(yintercept = y_mid),
        linetype = 'dotted',
        colour = 'grey80',
        alpha = 0.8
      ) +
      ggplot2::geom_vline(
        data = plot_quadrants,
        ggplot2::aes(xintercept = x_mid),
        linetype = 'dotted',
        colour = 'grey80',
        alpha = 0.8
      )
  }

  # add in a trendline?
  if (trendline) {
    plot <-
      plot +
      ggplot2::geom_smooth(
        ggplot2::aes(y = value_mid),
        formula = y ~ x,
        method = 'lm', se = FALSE, linetype = 'dotted', linewidth = 1
      )
  }

  # add in a range?
  if (range) {
    plot <-
      plot +
      ggplot2::geom_segment(
        ggplot2::aes(
          xend = rate_baseline, # keep same x-position
          y = value_lo,
          yend = value_hi,
          colour = scheme_highlight
        ),
        linewidth = 2, alpha = 0.1
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = value_lo, colour = scheme_highlight),
        alpha = 0.5
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = value_hi, colour = scheme_highlight),
        alpha = 0.5
      )
  }

  # add in scheme code label?
  if (scheme_label) {
    plot <-
      plot +
      ggplot2::geom_text(
        ggplot2::aes(y = value_mid, label = scheme_code),
        check_overlap = TRUE, nudge_y = 0.04
      )
  }

  # add in midpoints
  plot <-
    plot +
    ggplot2::geom_point(ggplot2::aes(y = value_mid, colour = scheme_highlight)) +
    ggplot2::scale_color_manual(
      values = c(
        'FALSE' = 'grey50',
        'TRUE' = 'red'
      )
    )

  # themes and decorations
  plot <-
    plot +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      # need to adjust the margin to avoid the axis labels being 'pushed out' of the image
      plot.margin = ggplot2::margin(t = 0, r = 0, b = 200, l = 15, unit = 'pt'),
      # panel
      panel.border = ggplot2::element_rect(colour = 'grey90', fill = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # legend
      legend.position = 'none'
    ) +
    ggplot2::labs(
      x = rate_title,
      y = value_title
    )

  # work out how tall to make this plot
  n_mitigators <-
    plot_dat |>
    dplyr::pull(mitigator_code) |>
    unique() |>
    length()

  # e.g. 200px for the padding and pro-rata height for each chart
  plot_height <-
    200 + (n_mitigators * facet_height_px)

  # convert to plotly and tweak settings
  plot <-
    plot |>
    plotly::ggplotly(tooltip = c('text'), height = plot_height) |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtons = list(list('toImage')),
      toImageButtonOptions = list(
        'format' = 'svg',
        'filename' = glue::glue(
          "nhp_baseline_comparison_", # name for this plot
          "{paste0(focal_scheme_code, collapse = '_')}_", # focal scheme code
          "{strftime(Sys.time(), '%Y%m%d_%H%M%S')}") # datetime
      )
    ) |>
    plotly::layout(
      font = list(family = 'Arial, Helvetica, Droid Sans, sans'),
      hoverlabel = list(font = list(size = 16))
    )

  return(plot)

}

#' Wrap a vector of strings to fit a specified width
#'
#' Takes a vector of strings and wraps them to fit within a specified pixel
#' width limit.
#'
#' @param strings Character vector - the input vector of strings to wrap
#' @param px_limit Integer (default 300) - the number of pixels to wrap `strings` to fit
#' @param font_family Character (default 'Arial, Helvetica, Droid Sans, sans') - the font family to use when estimating the width of strings
#' @param font_size Integer (default 14) - the font size to use when estimating the width of strings
#'
#' @returns Character vector
wrap_strings_to_fit_pixel_limit <- function(
    strings,
    px_limit = 300,
    font_family = 'Arial, Helvetica, Droid Sans, sans',
    font_size = 14
) {

  # add some margin to the px_limit
  px_limit <- px_limit * 0.8

  # take in the input vector and process it
  df <-
    strings |>
    tibble::as_tibble() |>
    dplyr::rename(input = value) |>
    dplyr::mutate(
      input_width_px = systemfonts::string_width(
        strings = input,
        family = font_family,
        size = font_size
      ),
      input_chars = nchar(input),
      input_char_px = input_width_px / nchar(input),
      chars_optimal = floor(px_limit / input_char_px),
    ) |>
    dplyr::rowwise() |> # to apply each chars_optimal to each input string
    dplyr::mutate(
      output = stringr::str_wrap(
        string = input,
        width = chars_optimal
      ),
      output_width_px = systemfonts::string_width(
        strings = stringr::str_split_i(
          string = output,
          pattern = '\n',
          i = 1
        ), # take the first line of the wrapped string
        family = font_family,
        size = font_size
      )
    ) |>
    dplyr::ungroup() # end the rowwise operation

  # return the output strings
  return(df$output)
}


#' Plot an individual trendline comparison plot
#'
#' Produces a single {plotly} object showing the trendline for the specified
#' scheme and mitigator.
#'
#' Note - this function is designed for use with `plot_facetted_trendlines()`
#' which coordinates the production of plots for multiple mitigators.
#' The use of the `subplot()` features of {plotly} mean that the overall plot
#' height is specified in each subplot's definition.
#'
#' @param plot_data Tibble - historical rate of activity for the mitigator, as produced within `plot_facetted_trendlines()` in `fct_plots.R`
#' @param dat_lu Tibble - lookup information for schemes and mitigators, as produced within `plot_facetted_trendlines()` in `fct_plots.R`
#' @param mitigator_codes Character vector - the `mitigator_code` to produce the plot for
#' @param focal_scheme_code Character vector - the focal `scheme_code` to produce the plot for
#' @param show_other_schemes Boolean (default = TRUE) plot time series for other schemes
#' @param show_horizon_timeline Boolean (default = TRUE) plot the predicted activity on the timeline at the horizon year
#' @param show_horizon_overlay Boolean (default = TRUE) plot the predicted activity as an overlay over the historical time series plots
#' @param show_prebaseline_average Boolean (default = TRUE) show the pre-baseline average (mean) with a range of two standard deviations above and below
#' @param facet_columns Integer (default = 1) the number of columns to facet the baseline plot
#' @param facet_height_px Integer (default = 400) the height of the plot
#' @param facet_count Integer (default = 1) the number of individual plots to be combined
#' @param x_axis_min Integer (default = 2010) the minimum year to show on the x-axis to help coordinate the x-axes for each plot
#' @param return_data Boolean (default = FALSE) TRUE = return a list object of tibbles used in the production of the plot - FOR TROUBLESHOOTING PURPOSES ONLY
#'
#' @returns {plotly} plot (or list of tibbles if return_data == TRUE)
plot_trendline_comparison <- function(
  # data objects
  plot_data, dat_lu, mitigator_codes, focal_scheme_code,

  # presentation objects
  show_other_schemes = TRUE,
  show_horizon_timeline = TRUE,
  show_horizon_overlay = TRUE,
  show_prebaseline_average = TRUE,
  facet_columns = 1,
  facet_height_px = 400,
  facet_count = 1,
  x_axis_min = 2010,
  return_data = FALSE
) {

  # wrap the y-axis labels to fit within facet_height_px
  plot_data <-
    plot_data |>
    dplyr::mutate(
      mitigator_activity_title = wrap_strings_to_fit_pixel_limit(
        strings = mitigator_activity_title,
        px_limit = facet_height_px
      )
    )

  ## horizon data ----
  # prepare the horizon forecast data for the focal scheme
  plot_data_horizon_focal <-
    plot_data |>
    # filter to baseline year for the focal scheme
    dplyr::filter(
      scheme_code %in% focal_scheme_code,
      year == year_baseline
    ) |>
    dplyr::mutate(
      # work out the forecasted horizon rate
      horizon_value_lo = rate * pi_value_lo,
      horizon_value_mid = rate * pi_value_mid,
      horizon_value_hi = rate * pi_value_hi,

      # how do the horizon values compare with average data (i.e. z-scores)
      zscore_value_lo = (horizon_value_lo - rate_mean) / rate_sd,
      zscore_value_mid = (horizon_value_mid - rate_mean) / rate_sd,
      zscore_value_hi = (horizon_value_hi - rate_mean) / rate_sd,

      # prepare the tooltip text
      tooltip_text = glue::glue(
        '<b>{scheme_name}</b> ({scheme_code})\n',
        '<i>Baseline year:</i> {year_baseline}\n',
        '<i>Horizon year:</i> {year_horizon}\n',
        '<i>Horizon rate:</i> {round(horizon_value_mid,2)} ',
        '({round(horizon_value_lo,2)} to {round(horizon_value_hi,2)})\n',
        '<i>Z-score:</i> {round(zscore_value_mid,2)} ',
        '({round(zscore_value_lo,2)} to {round(zscore_value_hi,2)})'
      )
    )

  # create the plot ----
  plot <-
    plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = rate, text = tooltip_text)) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(mitigator_name),
      scales = 'free',
      ncol = facet_columns
    )

  ## show pre-baseline average ----
  if (show_prebaseline_average) {

    # calculate data to plot
    df_horizon_overlay <-
      plot_data |>
      dplyr::filter(
        scheme_code %in% focal_scheme_code,
        year <= year_baseline
      ) |>
      dplyr::mutate(tooltip_text = '')

    # show average +/- 2 s.d.
    plot <-
      plot +
      # fill
      ggplot2::geom_ribbon(
        data = df_horizon_overlay,
        ggplot2::aes(
          x = year, y = rate_mean,
          ymin = rate_mean - (rate_sd * 2),
          ymax = rate_mean + (rate_sd * 2),
          group = 1
        ),
        alpha = 0.05
      ) +
      # mean
      ggplot2::geom_line(
        data = df_horizon_overlay,
        ggplot2::aes(x = year, y = rate_mean, group = 1),
        alpha = 0.5, linetype = 'dotted'
      ) +
      # mean + 2sd
      ggplot2::geom_line(
        data = df_horizon_overlay,
        ggplot2::aes(x = year, y = rate_mean + (rate_sd * 2), group = 1),
        alpha = 0.2, linetype = 'dotted'
      ) +
      # mean - 2sd
      ggplot2::geom_line(
        data = df_horizon_overlay,
        ggplot2::aes(x = year, y = rate_mean - (rate_sd * 2), group = 1),
        alpha = 0.2, linetype = 'dotted'
      )
  }

  ## plot all schemes? ----
  if (show_other_schemes) {

    # get the data
    df_other_schemes <-
      plot_data |> dplyr::filter(scheme_code != focal_scheme_code)

    # add to the plot only if there is data present
    if (nrow(df_other_schemes) > 0) {
      plot <-
        plot +
        ggplot2::geom_line(
          data = plot_data |> dplyr::filter(scheme_code != focal_scheme_code),
          ggplot2::aes(group = scheme_code),
          alpha = 0.2
        )
    }
  }

  ## horizon overlay? -----
  # show the horizon forecast overlaid on the series?
  if (show_horizon_overlay) {

    # # calculate data to plot
    df_horizon_overlay <- tibble::tibble(
      year_min = min(plot_data$year[plot_data$scheme_code %in% focal_scheme_code]),
      year_max = ifelse(
        test = show_horizon_timeline,
        yes = max(plot_data_horizon_focal$year_horizon),
        no = max(plot_data$year)
      ),
      year = dplyr::coalesce(year_min, 2010):dplyr::coalesce(year_max, 2041),
      ymin = plot_data_horizon_focal$horizon_value_lo,
      ymax = plot_data_horizon_focal$horizon_value_hi,
      rate = plot_data_horizon_focal$horizon_value_mid,
      tooltip_text = ''
    )

    # overlay the horizon forecast range on the plot
    plot <-
      plot +
      # add a filled area showing the horizon forecast
      ggplot2::geom_ribbon(
        data = df_horizon_overlay,
        ggplot2::aes(
          x = year,
          y = rate,
          ymin = ymin,
          ymax = ymax,
        ),
        fill = 'red', alpha = 0.1
      ) +
      ggplot2::geom_line(
        data = df_horizon_overlay,
        ggplot2::aes(x = year, y = ymin),
        colour = 'red', alpha = 0.15
      ) +
      ggplot2::geom_line(
        data = df_horizon_overlay,
        ggplot2::aes(x = year, y = ymax),
        colour = 'red', alpha = 0.15
      ) +
      ggplot2::geom_line(
        data = df_horizon_overlay,
        ggplot2::aes(x = year, y = rate),
        colour = 'red', alpha = 0.15
      )
  }

  ## horizon timeline? -----
  # add the horizon year as a point range at horizon year
  if (show_horizon_timeline) {
    plot <-
      plot +
      ggplot2::geom_pointrange(
        data = plot_data_horizon_focal,
        ggplot2::aes(
          x = year_horizon,
          y = horizon_value_mid,
          ymin = horizon_value_lo,
          ymax = horizon_value_hi
        ),
        colour = 'red'
      ) +
      ggplot2::geom_segment(
        data = plot_data_horizon_focal,
        ggplot2::aes(
          x = year, xend = year_horizon,
          y = rate, yend = horizon_value_lo
        ),
        alpha = 0.2, colour = 'red', linetype = 'dotted'
      ) +
      ggplot2::geom_segment(
        data = plot_data_horizon_focal,
        ggplot2::aes(
          x = year, xend = year_horizon,
          y = rate, yend = horizon_value_hi
        ),
        alpha = 0.2, colour = 'red', linetype = 'dotted'
      )
  }

  # add the focal scheme's historical activity
  plot <-
    plot +
    ggplot2::geom_line(
      data = plot_data |> dplyr::filter(scheme_code == focal_scheme_code),
      ggplot2::aes(group = scheme_code),
      colour = 'red', linewidth = 2
    )

  # scales
  plot <-
    plot +
    ggplot2::scale_x_continuous(
      limits = c(x_axis_min, NA),
      breaks = scales::pretty_breaks(),
      name = ''
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      name = '' # NB, plotly subplots remove axes titles, so setting blank here in ggplot to avoid overlapping titles
    )

  # themes and decorations
  plot <-
    plot +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      # panel
      panel.border = ggplot2::element_rect(colour = 'grey90', fill = NA),
      panel.grid.minor = ggplot2::element_blank(),
      # legend
      legend.position = 'none',
      # axes
      axis.title = ggplot2::element_blank()
  )

  # convert to plotly object and tweak settings
  plot <-
    plot |>
    plotly::ggplotly(tooltip = c('text')) |>
    plotly::layout(
      font = list(family = 'Arial, Helvetica, Droid Sans, sans'),
      hoverlabel = list(font = list(size = 16)),
      # label the y-axis
      yaxis = list(
        title = plot_data |>
          dplyr::slice_head(n = 1) |>
          dplyr::pull(mitigator_activity_title),
        titlefont = list(size = 14)
      )
    )

  # return plot or data
  if (return_data) {
    return(
      list(
        dat_lu = dat_lu,
        plot_data = plot_data,
        plot_data_horizon = plot_data_horizon_focal,
        df_horizon_overlay = df_horizon_overlay
      )
    )
  } else {
    return(plot)
  }
}

#' Plot faceted trendlines
#'
#' Co-ordinates the production of a a {plotly} object containing multiple
#' time series plots, one for each mitigator in `mitigator_codes`.
#'
#' @param dat Tibble - mitigator data as produced by `populate_table()` in `fct_tabulate.R`
#' @param rates_data Tibble - historical rates data for schemes / mitigators
#' @param mitigator_codes Character vector - the mitigator codes to visualise
#' @param focal_scheme_code Character vector - the scheme code for the focal scheme
#' @param scheme_codes Character vector - a list of codes for other schemes to visualise
#' @param show_other_schemes Boolean (default = TRUE) plot time series for other schemes
#' @param show_horizon_timeline Boolean (default = TRUE) plot the predicted activity on the timeline at the horizon year
#' @param show_horizon_overlay Boolean (default = TRUE) plot the predicted activity as an overlay over the historical time series plots
#' @param show_prebaseline_average Boolean (default = TRUE) show the pre-baseline average (mean) with a range of two standard deviations above and below
#' @param facet_height_px Integer (default = 400) the height of the plot
#' @param return_data Boolean (default = FALSE) TRUE = return a list object of tibbles used in the production of the plot - FOR TROUBLESHOOTING PURPOSES ONLY
#'
#' @returns {plotly} plot combining individual trendline plots for each mitigator
plot_faceted_trendlines <- function(
  # data objects
  dat, rates_data, mitigator_codes, focal_scheme_code, scheme_codes,

  # presentation objects
  show_other_schemes = TRUE,
  show_horizon_timeline = TRUE,
  show_horizon_overlay = TRUE,
  show_prebaseline_average = TRUE,
  facet_height_px = 400,
  return_data = FALSE
) {

  ## data wrangling ----

  # get some lookup information from dat
  dat_lu <-
    dat |>
    dplyr::filter(
      !is.na(scheme_code),
      mitigator_code %in% mitigator_codes,
      scheme_code %in% c(focal_scheme_code, scheme_codes)
    ) |>
    dplyr::select(
      # keys
      mitigator_variable, scheme_code,

      # lookup data
      scheme_name,
      mitigator_code, mitigator_name, mitigator_activity_title,
      year_baseline, year_horizon,
      pi_value_lo, pi_value_mid, pi_value_hi
    ) |>
    dplyr::distinct(.keep_all = TRUE)

  # prepare the rates data
  plot_data <-
    dat_lu |>
    # add the rates data to the lookup information
    # NB, inner join to ensure both rates and horizon values are present
    dplyr::inner_join(
      y = rates_data,
      by = dplyr::join_by(
        scheme_code == procode,
        mitigator_variable == strategy
      )
    ) |>
    # limit to mitigators where the focal scheme has data
    dplyr::filter(
      mitigator_code %in% mitigator_code[scheme_code == focal_scheme_code]
    ) |>
    # format the rates data ready for use
    dplyr::mutate(
      year = stringr::str_sub(
        string = fyear,
        start = 1L,
        end = 4L
      ) |> as.integer()
    ) |>
    # work out some useful metrics
    dplyr::mutate(
      rate_mean = mean(rate[year <= year_baseline], na.rm = T),
      rate_sd = sd(rate[year <= year_baseline], na.rm = T),
      .by = c(scheme_code, mitigator_code)
    ) |>
    dplyr::mutate(
      # prepare the tooltip information
      tooltip_text = glue::glue(
        '<b>{scheme_name}</b> ({scheme_code})\n',
        '<i>Year:</i> {year}\n',
        '<i>Rate:</i> {round(rate, 2)}',
        '{ifelse(year == year_baseline, "\nBaseline year", "")}'
      )
    )

  # reset the list of mitigators to where the focal scheme has data
  mitigator_codes_new <-
    plot_data |>
    dplyr::pull(mitigator_code) |>
    unique() |>
    sort()

  ## plotting -----
  # iterate over the mitigators and plot the result
  plots <-
    purrr::map(
      .x = mitigator_codes_new,
      .f = \(.x) plot_trendline_comparison(
        plot_data = plot_data |> dplyr::filter(mitigator_code == .x),
        dat_lu = dat_lu |> dplyr::filter(mitigator_code == .x),
        mitigator_codes = .x,
        focal_scheme_code = focal_scheme_code,

        show_other_schemes = show_other_schemes,
        show_horizon_timeline = show_horizon_timeline,
        show_horizon_overlay = show_horizon_overlay,
        show_prebaseline_average = show_prebaseline_average,
        facet_columns = 1,
        facet_height_px = facet_height_px,
        facet_count = length(mitigator_codes_new),
        x_axis_min = min(plot_data$year, na.rm = TRUE),
        return_data = FALSE
      )
    )

  # display a message if no plots can be produced - better than an error
  # message on the console
  shiny::validate(
    shiny::need(
      expr = length(plots) > 0,
      message = glue::glue(
        "Unable to plot because the focal scheme has no:\n",
        "• historical data for the selected mitigators, or\n",
        "• horizon estimates for the selected mitigators."
      )
    )
  )

  # combine the plots
  plot <-
    plotly::subplot(
      plots,
      nrows = length(plots),
      # add spacing between plots to allow for facet titles
      # needs scaling in proportion to number of facets
      margin = c(
        0, 0,
        0.1 * (1 / length(mitigator_codes_new)),
        0.1 * (1 / length(mitigator_codes_new))
      ), # l, r, t, b
      shareY = FALSE, titleY = TRUE,
      shareX = FALSE, titleX = TRUE
    ) |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtons = list(list('toImage')),
      toImageButtonOptions = list(
        'format' = 'svg',
        'filename' = glue::glue(
          "nhp_trendline_comparison_", # name for this plot
          "{paste0(focal_scheme_code, collapse = '_')}_", # focal scheme code
          "{strftime(Sys.time(), '%Y%m%d_%H%M%S')}") # datetime
      )
    ) |>
    plotly::layout(
      title = list(
        # name the focal scheme so available when exported as image
        text = plot_data |>
          dplyr::filter(scheme_code == focal_scheme_code) |>
          dplyr::slice_head(n = 1) |>
          dplyr::mutate(focal_scheme_name = glue::glue('{scheme_name} [{scheme_code}]')) |>
          dplyr::pull(focal_scheme_name),
        x = 0.5,
        font = list(
          family = 'Arial, Helvetica, Droid Sans, sans',
          size = 10,
          color = 'red'
        )
      )
    )

  # set the overall height of the plot
  # NB, suppressing warnings because of the following message:
  # `Warning: Specifying width/height in layout() is now deprecated.
  # Please specify in ggplotly() or plot_ly()`
  suppressWarnings({
    plot <-
      plot |>
      plotly::layout(
        height = facet_height_px * length(plots)
      )
  })


  # return the plot
  if (return_data == FALSE) {
    return(plot)
  } else {
    return(
      list(
        dat = dat,
        rates_data = rates_data,
        mitigator_codes = mitigator_codes,
        focal_scheme_code = focal_scheme_code,
        scheme_codes = scheme_codes,
        dat_lu = dat_lu,
        plot_data = plot_data,
        plots = plots
      )
    )
  }

}



