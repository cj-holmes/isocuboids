library(tidyverse)

unit_cube_df <-
  tribble(
    ~face,   ~ux, ~uy, ~uz,
    "right", 0,   0,   0,
    "right", 0,   1,   0,
    "right", 1,   1,   0,
    "right", 1,   0,   0,
    "left",  0,   0,   0,
    "left",  0,   0,   1,
    "left",  0,   1,   1,
    "left",  0,   1,   0,
    "top",   0,   1,   0,
    "top",   1,   1,   0,
    "top",   1,   1,   1,
    "top",   0,   1,   1)

usethis::use_data(unit_cube_df, overwrite = TRUE)
