

#' Prepare heatmap data
#'
#' Prepares the data ready for use in the heatmap plots.
#'
#' @param dat Tibble of mitigator data as produced by `populate_table()` in `fct_tabulate.R`
#' @param mitigator_codes Character vector of mitigator codes to display in the heatmap
#' @param scheme_codes Character vector of scheme codes to display in the heatmap
#' @param heatmap_type String describing the type of heatmap to produce - as defined by `input$heatmap_type``
#' @param standardise_heatmap Boolean (default = FALSE) should the values be standardised per year?
#' @param scheme_order String describing the display order for schemes
#' @param mitigator_order String description of the display order for mitigators
#' @param values_displayed String description of the values displayed - as defined by `input$values_displayed`
#'
#' @returns Tibble of data ready for use with heatmap plots
prepare_heatmap_dat <- function(
    dat,
    mitigator_codes,
    scheme_codes,
    focal_scheme_code,
    heatmap_type = "value_binary",
    standardise_heatmap = FALSE,
    scheme_order,
    mitigator_order,
    values_displayed
) {
  ## wrangle data ----
  # limit to schemes and mitigators of choice
  dat <-
    dat |>
    dplyr::filter(
      mitigator_code %in% mitigator_codes,
      scheme_code %in% scheme_codes
    )

  # should values be standardised?
  if (standardise_heatmap) {
    dat <-
      dat |>
      dplyr::mutate(
        dplyr::across(
          c(value_lo, value_hi, value_mid),
          \(.x) .x / year_range
        )
      )
  }

  # calculate the heatmap values
  dat <-
    dat |>
    dplyr::mutate(
      # produce a binary value to show if a value has been set
      value_binary = !is.na(value_lo),
      # round the values to avoid precision clutter
      dplyr::across(
        c(value_lo, value_hi, value_mid, value_range),
        \(.x) janitor::round_half_up(.x, 3)
      ),
      # capture values as text (for tooltip)
      value_description = glue::glue(
        "{scales::percent(value_mid, accuracy = 0.1)} ",
        "({scales::percent(value_lo, accuracy = 0.1)} to ",
        "{scales::percent(value_hi, accuracy = 0.1)})"
      ),
      # flag the focal scheme
      focal_scheme_text = dplyr::if_else(
        condition = scheme_code %in% focal_scheme_code,
        true = "Focal scheme",  false = ""
      )#,
      # scheme_name = dplyr::if_else(
      #   condition = scheme_code %in% focal_scheme_code,
      #   true = glue::glue("⫷ {scheme_name} ⫸"),
      #   false = scheme_name
      # )
    ) |>
    # put each value type on its own row
    tidyr::pivot_longer(
      c(value_lo, value_hi, value_mid, value_range, value_binary),
      names_to = "value_type",
      values_to = "value"
    ) |>
    # limit to specified heatmap type
    dplyr::filter(
      value_type == heatmap_type
    ) |>
    dplyr::mutate(
      # standardise values across mitigators (for heatmap colouring across rows)
      value_scaled = value |>
        scales::rescale() |>
        janitor::round_half_up(digits = 3),

      # find min/max/average for each mitigator
      value_min_mit = value |> min(na.rm = TRUE),
      value_max_mit = value |> max(na.rm = TRUE),
      value_mean_mit = value |> mean(na.rm = TRUE),

      # rank schemes within each mitigator
      value_rank = dplyr::min_rank(x = dplyr::desc(value)),
      value_rank_total = length(value),

      .by = mitigator_code
    ) |>
    dplyr::mutate(
      value_min_sch = value |> min(na.rm = TRUE),
      value_max_sch = value |> max(na.rm = TRUE),
      value_mean_sch = value |> mean(na.rm = TRUE),

      .by = scheme_code
    ) |>
    # create the tooltip text
    dplyr::mutate(
      tooltip_text = glue::glue(
        "<b>{scheme_name}</b> [{scheme_code}]",
        " {focal_scheme_text}\n",
        "{mitigator_name} [{mitigator_code}]\n",
        "<b>{scales::percent(value, accuracy = 0.1)}</b> {values_displayed}\n",
        "<i>Scheme rank:</i> {value_rank} of {value_rank_total}\n",
        "<i>Mitigation:</i> {value_description}\n",
        "Mitigator ",
        "<i>min:</i> {scales::percent(value_min_mit, accuracy = 0.1)}, ",
        "<i>max:</i> {scales::percent(value_max_mit, accuracy = 0.1)}, ",
        "<i>mean:</i> {scales::percent(value_mean_mit, accuracy = 0.1)}\n",
        "Scheme ",
        "<i>min:</i> {scales::percent(value_min_sch, accuracy = 0.1)}, ",
        "<i>max:</i> {scales::percent(value_max_sch, accuracy = 0.1)}, ",
        "<i>mean:</i> {scales::percent(value_mean_sch, accuracy = 0.1)}"
      )
    )

  ### order schemes ----
  # order schemes according to user preference
  base::switch(
    EXPR = scheme_order,

    "scheme_name_asc" = {
      dat <-
        dat |>
        dplyr::mutate(
          scheme_code = forcats::fct(
            x = scheme_code,
            levels = dat |>
              dplyr::select(scheme_code, scheme_name) |>
              dplyr::distinct(scheme_code, .keep_all = TRUE) |>
              dplyr::arrange(scheme_name) |>
              dplyr::pull(scheme_code)
          )
        )
    },

    "scheme_name_desc" = {
      dat <-
        dat |>
        dplyr::mutate(
          scheme_code = forcats::fct(
            x = scheme_code,
            levels = dat |>
              dplyr::select(scheme_code, scheme_name) |>
              dplyr::distinct(scheme_code, .keep_all = TRUE) |>
              dplyr::arrange(desc(scheme_name)) |>
              dplyr::pull(scheme_code)
          )
        )
    },

    "scheme_mitigator_count_asc" = {
      dat <-
        dat |>
        dplyr::mutate(
          scheme_code = forcats::fct(
            x = scheme_code,
            levels = dat |>
              dplyr::select(scheme_code, mitigator_code) |>
              dplyr::filter(!is.na(mitigator_code)) |>
              dplyr::summarise(
                mitigator_count = dplyr::n_distinct(mitigator_code, na.rm = TRUE),
                .by = scheme_code
              ) |>
              dplyr::arrange(mitigator_count, scheme_code) |>
              dplyr::pull(scheme_code)
          )
        )
    },

    "scheme_mitigator_count_desc" = {
      dat <-
        dat |>
        dplyr::mutate(
          scheme_code = forcats::fct(
            x = scheme_code,
            levels = dat |>
              dplyr::select(scheme_code, mitigator_code) |>
              dplyr::filter(!is.na(mitigator_code)) |>
              dplyr::summarise(
                mitigator_count = dplyr::n_distinct(mitigator_code, na.rm = TRUE),
                .by = scheme_code
              ) |>
              dplyr::arrange(desc(mitigator_count), scheme_code) |>
              dplyr::pull(scheme_code)
          )
        )
    },

    "scheme_average_asc" = {
      dat <-
        dat |>
        dplyr::mutate(
          scheme_code = forcats::fct(
            x = scheme_code,
            levels = dat |>
              dplyr::select(scheme_code, value) |>
              dplyr::summarise(
                average = mean(value, na.rm = TRUE),
                .by = scheme_code
              ) |>
              dplyr::arrange(average, scheme_code) |>
              dplyr::pull(scheme_code)
          )
        )
    },

    "scheme_average_desc" = {
      dat <-
        dat |>
        dplyr::mutate(
          scheme_code = forcats::fct(
            x = scheme_code,
            levels = dat |>
              dplyr::select(scheme_code, value) |>
              dplyr::summarise(
                average = mean(value, na.rm = TRUE),
                .by = scheme_code
              ) |>
              dplyr::arrange(desc(average), scheme_code) |>
              dplyr::pull(scheme_code)
          )
        )
    },
  )

  ### order mitigators ----
  # order mitigators according to user preference
  base::switch(
    EXPR = mitigator_order,

    "mitigator_name_asc" = {
      dat <-
        dat |>
        dplyr::mutate(
          mitigator_code = forcats::fct(
            x = mitigator_code,
            levels = dat |>
              dplyr::select(mitigator_code, mitigator_name) |>
              dplyr::distinct(mitigator_code, .keep_all = TRUE) |>
              dplyr::arrange(mitigator_name) |>
              dplyr::pull(mitigator_code)
          )
        )
    },

    "mitigator_name_desc" = {
      dat <-
        dat |>
        dplyr::mutate(
          mitigator_code = forcats::fct(
            x = mitigator_code,
            levels = dat |>
              dplyr::select(mitigator_code, mitigator_name) |>
              dplyr::distinct(mitigator_code, .keep_all = TRUE) |>
              dplyr::arrange(desc(mitigator_name)) |>
              dplyr::pull(mitigator_code)
          )
        )
    },

    "mitigator_scheme_count_asc" = {
      dat <-
        dat |>
        dplyr::mutate(
          mitigator_code = forcats::fct(
            x = mitigator_code,
            levels = dat |>
              dplyr::select(mitigator_code, scheme_code) |>
              dplyr::filter(!is.na(scheme_code)) |>
              dplyr::summarise(
                scheme_count = dplyr::n_distinct(scheme_code, na.rm = TRUE),
                .by = mitigator_code
              ) |>
              dplyr::arrange(scheme_count, mitigator_code) |>
              dplyr::pull(mitigator_code)
          )
        )
    },

    "mitigator_scheme_count_desc" = {
      dat <-
        dat |>
        dplyr::mutate(
          mitigator_code = forcats::fct(
            x = mitigator_code,
            levels = dat |>
              dplyr::select(mitigator_code, scheme_code) |>
              dplyr::filter(!is.na(scheme_code)) |>
              dplyr::summarise(
                scheme_count = dplyr::n_distinct(scheme_code, na.rm = TRUE),
                .by = mitigator_code
              ) |>
              dplyr::arrange(desc(scheme_count), mitigator_code) |>
              dplyr::pull(mitigator_code)
          )
        )
    },

    "mitigator_average_asc" = {
      dat <-
        dat |>
        dplyr::mutate(
          mitigator_code = forcats::fct(
            x = mitigator_code,
            levels = dat |>
              dplyr::select(mitigator_code, value) |>
              dplyr::summarise(
                average = mean(value, na.rm = TRUE),
                .by = mitigator_code
              ) |>
              dplyr::arrange(average, mitigator_code) |>
              dplyr::pull(mitigator_code)
          )
        )
    },

    "mitigator_average_desc" = {
      dat <-
        dat |>
        dplyr::mutate(
          mitigator_code = forcats::fct(
            x = mitigator_code,
            levels = dat |>
              dplyr::select(mitigator_code, value) |>
              dplyr::summarise(
                average = mean(value, na.rm = TRUE),
                .by = mitigator_code
              ) |>
              dplyr::arrange(desc(average), mitigator_code) |>
              dplyr::pull(mitigator_code)
          )
        )
    }
  )

  # order scheme and mitigator names to match codes
  dat <-
    dat |>
    dplyr::mutate(
      # need to reverse ordering for y-axis to ensure correct display in ggplot2
      mitigator_code = mitigator_code |>
        forcats::fct_rev(),

      # order scheme name to match scheme code
      scheme_name = scheme_name |>
        forcats::fct() |>
        stats::reorder(as.numeric(scheme_code)),
      # order mitigator name to match mitigator code
      mitigator_name = mitigator_name |>
        forcats::fct() |>
        stats::reorder(as.numeric(mitigator_code)),
    )

  # return the result
  return(dat)
}

#' Plot heatmap
#'
#' Produces a {plotly} version of the heatmap showing mitigators on the y-axis
#' and schemes across the x-axis with the selected mitigator value (mid, low,
#' high, binary) controlling the fill colour.
#'
#' @param dat Tibble of mitigator data as produced by `populate_table()` in `fct_tabulate.R`
#' @param toggle_mitigator_name Boolean - value from `input$toggle_mitigator_name` - used to decide what to display on y-axis
#' @param toggle_scale_fill_by_mitigator Boolean - value from `input$toggle_heatmap_scale_fill_by_mitigator` - used to control whether the colour is scaled across the whole heatmap (FALSE) or applied across each mitigator (TRUE)
#' @param values_displayed Character - value from `input$values_displayed` - describing if values represent either percent of activity mitigated or the 80% prediction interval - used to label the scales
#' @param heatmap_type Character - value from `input$heatmap_type` - used to control whether a solid colour is applied (binary) or colour is dependent on the value
#' @param colour_binary Character - value from `input$heatmap_binary_colour` - hex colour string to use in the binary plots
#' @param colour_value_low Character - value from `input$heatmap_value_colour_low` - hex colour string to use in the gradient fill for low values
#' @param colour_value_high Character - value from `input$heatmap_value_colour_high` - hex colour string to use in the gradient fill for high values
#' @param name Integer - value from `ra$heatmap_min_height` - the number of pixels to set as the height of the plot
#'
#' @returns {plotly} object showing heatmap
plot_heatmap <- function(
  # data
  dat,
  focal_scheme_code,

  # options
  toggle_mitigator_name,
  toggle_scale_fill_by_mitigator,
  values_displayed,
  heatmap_type,

  # formatting
  colour_binary,
  colour_value_low,
  colour_value_high,
  plot_height,
  font_family = 'Arial, Helvetica, Droid Sans, sans'
) {

  # preparation ----
  # gather information
  y_max_char = dat$mitigator_name |>
    as.character() |>
    nchar() |>
    max()

  y_char_wrap = (y_max_char / 1.8) |>
    ceiling()

  x_scheme_count <- dat$scheme_name |>
    unique() |>
    length()

  # logic ----

  ## y-axis ----
  # decide whether to plot the mitigator code or name on the y-xais
  ifelse(
    test = toggle_mitigator_name,
    yes = var_y_axis <- 'mitigator_name',
    no = var_y_axis <- 'mitigator_code'
  )

  ## fill ----
  # decide whether fill colour applies across mitigators or across whole plot
  ifelse(
    test = toggle_scale_fill_by_mitigator,
    yes = var_fill <- 'value_scaled', # mitigator-wise fill
    no = var_fill <- 'value' # plot-wise fill
  )

  # convert to symbols - so can be used as variables in ggplot
  var_y_axis <- as.symbol(var_y_axis)
  var_fill <- as.symbol(var_fill)

  ## x-axis ----
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

  # plotting ----
  heatmap <-
    dat |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = scheme_name,
        y = {{ var_y_axis }},
        fill = {{ var_fill }},
        text = tooltip_text
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
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = x_scheme_text,
    ) +
    ggplot2::labs(fill = values_displayed)

  # handle non-binary plots
  if (heatmap_type != "value_binary") {
    heatmap <-
      heatmap +
      ggplot2::geom_text(
        ggplot2::aes(label = value |> scales::percent(accuracy = 1)),
        color = "white",
        size = 4
      ) +
      ggplot2::scale_fill_gradient(
        low = colour_value_low,
        high = colour_value_high,
        labels = scales::label_percent()
      )
  }

  # handle binary plots
  if (heatmap_type == "value_binary") {
    heatmap <-
      heatmap +
      ggplot2::theme(legend.position = "none") +
      ggplot2::aes(fill = as.character(value)) +
      ggplot2::scale_fill_manual(values = c('1' = colour_binary))
  }

  # final ggplot formatting
  heatmap <-
    heatmap +
    ggplot2::theme(
      legend.position = 'none' # remove the legend
    )

  # convert to plotly
  heatmap <-
    heatmap |>
    plotly::ggplotly(tooltip = c('text'), height = plot_height) |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtons = list(list('toImage')),
      toImageButtonOptions = list(
        'format' = 'svg',
        'filename' = glue::glue(
          "nhp_heatmap_", # name for this plot
          "{paste0(focal_scheme_code, collapse = '_')}_", # focal scheme code
          "{strftime(Sys.time(), '%Y%m%d_%H%M%S')}") # datetime
      )
    ) |>
    plotly::layout(
      font = list(family = font_family),
      xaxis = list(
        side = 'top',
        tickfont = list(
          family = font_family,
          size = 15
        )
      ),
      yaxis = list(
        tickfont = list(
          family = font_family,
          size = 14
        )
      ),
      hoverlabel = list(
        font = list(
          family = font_family,
          size = 14
        )
      ),
      # ensure the heatmap extends down and across, but leave room for titles
      margin = list(t = 100, b = 0, l = 100, r = 0)
    )

  return(heatmap)

}
