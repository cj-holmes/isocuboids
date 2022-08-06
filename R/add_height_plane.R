#' Add height plane to plot
#'
#' Intended for internal use only
#'
#' @param a1 Angle of rotation around the vertical axis in degrees
#' @param a2 Angle of rotation around the horizontal axis in degrees
#' @param x_dim Length of x dimension (image width)
#' @param z_dim Length of z dimension (image height)
#' @param y_dim Maximal height of cuboids
#' @param col colour for border of height plane
#' @param lty linetype for border of height plane
#' @param fill fill colour for height plane
#' @param alpha transparency of height plane
add_height_plane <- function(a1, a2, x_dim, z_dim, y_dim, col, lty, fill, alpha){

  height_plane <-
    dplyr::tribble(
      ~ux,   ~uy,   ~uz,
      0,     y_dim, 0,
      x_dim, y_dim, 0,
      x_dim, y_dim, z_dim,
      0,     y_dim, z_dim) |>
    dplyr::mutate(
      project(x=ux, y=uy, z=uz, va=a1, ha=a2) |>
        dplyr::as_tibble(.name_repair = ~c("px", "py", "pz")))

    ggplot2::annotate("polygon",
                      x = height_plane$px,
                      y = height_plane$py,
                      col = col,
                      fill = fill,
                      lty = lty,
                      alpha = alpha,
                      size = 1)
}
