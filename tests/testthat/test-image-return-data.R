library(isocuboids)
library(testthat)

i <- magick::image_read('https://www.r-project.org/logo/Rlogo.png')

df <- cuboid_image(i, res = 60, crop_square = TRUE, height_scale = c(0, 60), return_data = TRUE)

test_that("Image height is mapped correctly", {
  expect_equal(max(df$y), 60)
})

test_that("Image has been cropped square", {
  expect_true(all(c(range(df$x) == c(0, 59),
                    range(df$z) == c(0, 59))))
})

df <- cuboid_image(i, res = 60, crop_square = FALSE, height_scale = c(0, 60), return_data = TRUE)

test_that("Image has not been cropped square", {
  expect_true(all(c(range(df$x) == c(0, 59),
                    range(df$z) == c(0, 45))))
})


# Make a small 20x30 test image (for speed)
i <- magick::image_read(matrix(rep("black", 20*30), nrow = 30))
df <- cuboid_image(i, res=NULL, return_data = TRUE)

test_that("Image has not been cropped or resized from original dimensions", {
  expect_true(all(c(range(df$x) == c(0, 19),
                    range(df$z) == c(0, 29))))
})




# Test function errors correctly -------------------------------------------
test_that(
    desc = "Function errors if shading values are out of range",
    code =
        {expect_error(
            object = cuboid_image(img = i, shading = c(0, 0, 1.1)),
            regexp = "All shading values must be between 0 and 1")})

test_that(
    desc = "Function errors if shading values are out of range",
    code =
        {expect_error(
            object = cuboid_image(img = i, shading = c(-20, NA, 1.1)),
            regexp = "All shading values must be between 0 and 1")})

test_that(
    desc = "Function errors if angles are out of range",
    code =
        {expect_error(
            object = cuboid_image(img = i, a1 = -10, a2 = 93),
            regexp = "Both angles a1 and a2 must be between 0 and 90")})


test_that(
    desc = "Function errors if height_scale has negative values",
    code =
        {expect_error(
            object = cuboid_image(img = i, height_scale = c(10, -2)),
            regexp = "Both height scale values must be greater than or equal to 0")})

test_that(
    desc = "Function errors if height_scale has negative values",
    code =
        {expect_error(
            object = cuboid_image(img = i, height_scale = c(-1, -2)),
            regexp = "Both height scale values must be greater than or equal to 0")})
