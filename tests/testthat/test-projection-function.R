library(isocuboids)
library(tibble)
library(ggplot2)
library(testthat)

# Project a horizontal line parallel to x by default isometric angles
p <-
    isocuboids:::project(
        x=0:1,
        z=c(0,0),
        y=c(0,0),
        va=45,
        ha=asin(tan(pi/6))*(180/pi))

test_that("Isometric projection angles rotate a horizontal line to be 30 degrees from the x axis", {
  expect_equal(
      object = atan(p[2,2]/p[2,1])*(180/pi),
      expected = 30)
})




# Long winded way of testing that all three axes are at 120 degrees -------

# Set angles to isometric default
a1 <- 45
a2 <- asin(tan(pi/6))*(180/pi)

# Create unprojected and projected vectors
# v1 = vertical
v1_u <- tibble(x = c(0,0), y=c(0,-1), z= c(0,0))
v1_p <- isocuboids:::project(v1_u$x, v1_u$y, v1_u$z, va = a1, ha = a2)

# v2 = to the left
v2_u <- tibble(x = c(0,0), y=c(0,0), z= c(0,1))
v2_p <- isocuboids:::project(v2_u$x, v2_u$y, v2_u$z, va = a1, ha = a2)

# v3 = to the right
v3_u <- tibble(x = c(0,1), y=c(0,0), z=c(0,0))
v3_p <- isocuboids:::project(v3_u$x, v3_u$y, v3_u$z, va = a1, ha = a2)

# Viualise the three vectors
ggplot()+
    annotate("segment", x=v1_p[1,1], xend=v1_p[2,1], y=v1_p[1,2], yend=v1_p[2,2], arrow=arrow(), col = "red")+
    annotate("segment", x=v2_p[1,1], xend=v2_p[2,1], y=v2_p[1,2], yend=v2_p[2,2], arrow=arrow(), col = "blue")+
    annotate("segment", x=v3_p[1,1], xend=v3_p[2,1], y=v3_p[1,2], yend=v3_p[2,2], arrow=arrow(), col = "orange")+
    coord_equal()

# Extract x and y components
v1x <- v1_p[2,1]
v2x <- v2_p[2,1]
v3x <- v3_p[2,1]

v1y <- v1_p[2,2]
v2y <- v2_p[2,2]
v3y <- v3_p[2,2]

# Compute magnitudes of each vector
mv1 <- sqrt(v1x^2 + v1y^2)
mv2 <- sqrt(v2x^2 + v2y^2)
mv3 <- sqrt(v3x^2 + v3y^2)

# Use dot product and magnitudes to compute angle and convert to degrees
v1v2 <- acos(((v1x*v2x) + (v1y*v2y)) / (mv1*mv2))*(180/pi)
v2v3 <- acos(((v2x*v3x) + (v2y*v3y)) / (mv2*mv3))*(180/pi)
v3v1 <- acos(((v3x*v1x) + (v3y*v1y)) / (mv3*mv1))*(180/pi)

test_that("3 component angles are all 120 degrees to eachother for isometric projection angles", {
    expect_equal(
        object = c(v1v2, v2v3, v3v1),
        expected = c(120, 120, 120))
})
