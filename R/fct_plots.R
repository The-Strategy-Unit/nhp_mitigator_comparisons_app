plot_pointrange <- function(dat_selected_pointrange, input) {

  pointrange <- dat_selected_pointrange |>
    dplyr::mutate(
      point_colour = dplyr::if_else(
        scheme_code == input$focus_scheme,
        TRUE,
        FALSE
      )
    ) |>
    ggplot2::ggplot()

  if (!input$toggle_invert_facets) {
    pointrange <- pointrange +
      ggplot2::geom_pointrange(
        ggplot2::aes(
          x = mid,
          y = scheme_name,
          xmin = lo,
          xmax = hi,
          colour = point_colour
        )
      ) +
      ggplot2::facet_wrap(~mitigator, nrow = input$facet_rows)
  }

  if (input$toggle_invert_facets) {
    pointrange <- pointrange +
      ggplot2::geom_pointrange(
        ggplot2::aes(
          x = mid,
          y = mitigator,
          xmin = lo,
          xmax = hi,
          colour = point_colour
        )
      ) +
      ggplot2::facet_wrap(~scheme_code, nrow = input$facet_rows)
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

}


plot_heatmap <- function(dat_selected_heatmap, input) {

  heatmap <- dat_selected_heatmap |>
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

}
