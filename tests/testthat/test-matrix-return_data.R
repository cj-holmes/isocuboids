library(isocuboids)
library(testthat)

df <- cuboid_matrix(matrix(seq(1, 10, l=100), nrow = 10), return_data = TRUE)

test_that("Matrix height is mapped correctly", {
  expect_equal(max(df$y), 10)
})

test_that("Matrix x dimensions are correct", {
  expect_equal(unique(df$x), 9:0)
})

test_that("Matrix z dimensions are correct", {
  expect_equal(unique(df$z), 9:0)
})


df2 <- cuboid_matrix(matrix(seq(1, 50, l=100), nrow = 20), return_data = TRUE)

test_that("Matrix height is mapped correctly", {
  expect_equal(max(df2$y), 50)
})

test_that("Matrix x dimensions are correct", {
  expect_equal(unique(df2$x), 4:0)
})

test_that("Matrix z dimensions are correct", {
  expect_equal(unique(df2$z), 19:0)
})



# Test colours map correctly ----------------------------------------------
cols <- hcl.colors(30, "plasma")
df3 <- cuboid_matrix(volcano, cuboid_fill = cols, return_data = TRUE)

test_that("Highest point of plot is coloured with last colour", {
  expect_equal(unique(df3$col_adjusted[df3$y == max(df3$y) & df3$face == "top"]), rev(cols)[1])
})

test_that("Lowest point of plot is coloured with first colour", {
  expect_equal(unique(df3$col_adjusted[df3$y == min(df3$y) & df3$face == "top"]), cols[1])
})



# Check function stops correctly ------------------------------------------
test_that(
    desc = "Function errors if passed a vector",
    code =
        {expect_error(
            object = cuboid_matrix(mat = 1:10),
            regexp = "mat must be a matrix")})

test_that(
    desc = "Function errors if passed a dataframe",
    code =
        {expect_error(
            object = cuboid_matrix(mat = data.frame(x=1, y=2)),
            regexp = "mat must be a matrix")})

test_that(
    desc = "Function errors if passed an array",
    code =
        {expect_error(
            object = cuboid_matrix(mat = array(1:10)),
            regexp = "mat must be a matrix")})

test_that(
    desc = "Function errors if negative values in matrix",
    code =
        {expect_error(
            object = cuboid_matrix(mat = matrix(c(1,2,3,-4))),
            regexp = "Negative values not allowed in matrix")})

test_that(
    desc = "Function errors if NA values in matrix",
    code =
        {expect_error(
            object = cuboid_matrix(mat = matrix(c(1,2,3,NA))),
            regexp = "Missing values not allowed in matrix")})

test_that(
    desc = "Function errors if angles are out of range",
    code =
        {expect_error(
            object = cuboid_matrix(mat = matrix(c(1,2,3)), a1 = -1, a2 = 45),
            regexp = "Both angles a1 and a2 must be between 0 and 90")})

test_that(
    desc = "Function errors if angles are out of range",
    code =
        {expect_error(
            object = cuboid_matrix(mat = matrix(c(1,2,3)), a1 = 10, a2 = 93),
            regexp = "Both angles a1 and a2 must be between 0 and 90")})

test_that(
    desc = "Function errors if angles are out of range",
    code =
        {expect_error(
            object = cuboid_matrix(mat = matrix(c(1,2,3)), a1 = -10, a2 = 93),
            regexp = "Both angles a1 and a2 must be between 0 and 90")})

test_that(
    desc = "Function errors if shading values are out of range",
    code =
        {expect_error(
            object = cuboid_matrix(mat = matrix(c(1,2,3)), shading = c(-1, 0, 10)),
            regexp = "All shading values must be between 0 and 1")})

test_that(
    desc = "Function errors if shading values are out of range",
    code =
        {expect_error(
            object = cuboid_matrix(mat = matrix(c(1,2,3)), shading = c(0, 0, 1.1)),
            regexp = "All shading values must be between 0 and 1")})

test_that(
    desc = "Function errors if shading values are out of range",
    code =
        {expect_error(
            object = cuboid_matrix(mat = matrix(c(1,2,3)), shading = c(-1, -10, 2)),
            regexp = "All shading values must be between 0 and 1")})
