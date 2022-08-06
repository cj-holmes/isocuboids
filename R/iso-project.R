#' Compute projected coordinates
#'
#' Intended for internal use only
#' https://en.wikipedia.org/wiki/Isometric_projection
#'
#' @param x x coordinate
#' @param y y coordinate
#' @param z z coordinate
#' @param va angle of rotation around the vertical axis (degrees)
#' @param ha angle of rotation arounf the horizontal axis (degrees)
project <- function(x, y, z, va, ha){

    # Convert rotation angles to radians
    var <- va*(pi/180)
    har <- ha*(pi/180)

    # Construct rotation matrices
    m1a <- matrix(c(1, 0, 0, 0, cos(har), sin(har), 0, -sin(har), cos(har)),
                  byrow = TRUE,
                  ncol = 3)

    m1b <- matrix(c(cos(var), 0, -sin(var), 0, 1, 0, sin(var), 0, cos(var)),
                  byrow = TRUE,
                  ncol = 3)

    m2 <- matrix(c(1,0,0,0,1,0,0,0,0), byrow = TRUE, nrow=3, ncol=3)

    m3 <- m2 %*% (m1a %*% m1b)

    # Matrix multiplication
    # m3 %*% matrix(c(x, y, z), nrow = 3, byrow = T)

    # Slightly faster?
    crossprod(x = matrix(c(x, y, z), nrow = 3, byrow = T),
              y = t(m3))
}
