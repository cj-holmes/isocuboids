library(tidyverse)
library(isocuboids)

# Create hex logo
set.seed(12)
cuboid_matrix(matrix(runif(64, 7.8, 8), nrow = 8),
              show_axes = F,
              cuboid_col = 1,
              cuboid_fill = hcl.colors(100, "zissou"))+
  theme_void()
ggsave('data-raw/hex-logo.pdf', width=10, height=10)
ggsave('data-raw/hex-logo.png', width=10, height=10, dpi = 300)



# Penrose steps (approx)
# Trial and error to get the stairs to line up!
d <- seq(3, by=0.214, l=14)

m <-
  matrix(
    c(
      rev(d[1:6]),
      d[7], 0, 0, 0, 0, 0,
      d[8], 0, 0, 0, 0, 0,
      d[9], 0, 0, 0, 0, 0,
      d[10], 0, d[14], 0, 0, 0,
      d[11:13], 0, 0, 0
    ),
    ncol = 6,
    byrow = T)

cuboid_matrix(m, cuboid_col = 1, show_axes = F, return_data = T) |>
  filter(!(x > 2 & z < 5)) |>
  ggplot()+
  geom_polygon(aes(px, py, group = plot_group, fill=face), col = 1)+
  coord_equal()+
  theme_void()
