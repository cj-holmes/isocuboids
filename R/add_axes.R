#' Add axes to plot
#'
#' Intended for internal use only
#' @param a1 projection angle a1
#' @param a2 projection angle a2
#' @param x_dim Length of x dimension (image width)
#' @param z_dim Length of z dimension (image height)
#' @param y_dim Length of y dimension (image height out of plane)
#' @param x_axis_col x axis colour
#' @param y_axis_col y axis colour
#' @param z_axis_col z axis colour
#' @param data dataframe created during processing of image
add_axes <- function(a1, a2, x_dim, z_dim, y_dim,
                     x_axis_col, z_axis_col, y_axis_col, data){

  axes <-
    dplyr::tribble(
      ~face,         ~ux,   ~uy,   ~uz,
      "right_start", 0,     0,     0,
      "right_end",   x_dim, 0,     0,
      "left_start",  0,     0,     0,
      "left_end",    0,     0,     z_dim,
      "top_start",   0,     0,     0,
      "top_end",     0,     y_dim, 0) |>
    dplyr::mutate(
      project(x=ux, y=uy, z=uz, va=a1, ha=a2) |>
        dplyr::as_tibble(.name_repair = ~c("px", "py", "pz")))

  list(
    ggplot2::annotate("segment",
                      x = axes$px[1], xend=axes$px[2],
                      y = axes$py[1], yend=axes$py[2],
                      col = x_axis_col,
                      size = 1,
                      arrow = ggplot2::arrow(length = ggplot2::unit(ifelse(a1 == 90 & a2 == 0, 0, 0.15),
                                                                    "inches"))),

    ggplot2::annotate("text",
                      x = axes$px[2],
                      y = axes$py[2],
                      label = "x",
                      col = x_axis_col,
                      hjust = 0
                      ),

    ggplot2::annotate("segment",
                      x = axes$px[3], xend=axes$px[4],
                      y = axes$py[3], yend=axes$py[4],
                      col = z_axis_col,
                      size = 1,
                      arrow = ggplot2::arrow(length = ggplot2::unit(ifelse(a1 == 0 & a2 == 0, 0, 0.15),
                                                                    "inches"))),

    ggplot2::annotate("text",
                      x = axes$px[4],
                      y = axes$py[4],
                      label = "z",
                      col = z_axis_col,
                      hjust = 1),

    ggplot2::annotate("segment",
                      x = axes$px[5], xend=axes$px[6],
                      y = axes$py[5], yend=axes$py[6],
                      col = y_axis_col,
                      size = 1,
                      arrow = ggplot2::arrow(length = ggplot2::unit(ifelse(a2 == 90 | max(data$y) == 0, 0, 0.15),
                                                                    "inches"))),
    ggplot2::annotate("text",
                      x = axes$px[6],
                      y = axes$py[6],
                      label = "y",
                      col = y_axis_col,
                      vjust = 0,
                      hjust = 1)

  )
}
