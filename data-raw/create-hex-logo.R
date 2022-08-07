library(tidyverse)
library(isocuboids)

# Create hex logo
set.seed(12)
cuboid_matrix(matrix(runif(64, 7.8, 8), nrow = 8),
              show_axes = F,
              cuboid_col = 1,
              cuboid_fill = hcl.colors(100, "zissou"))+
  theme_void()
ggsave('man/hex-logo.pdf', width=10, height=10)
ggsave('man/hex-logo.png', width=10, height=10, dpi = 300)
