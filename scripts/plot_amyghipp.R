### This script plots a 3D rendering of the amygdala and hippocampus (ideally
### with the center of the regions, TBD)
###
### Ellyn Butler
### May 4, 2023

library(ggseg)
library(ggseg3d)
library(dplyr)
library(tidyr)

#aseg_3d$ggseg_3d[[1]]$region

ah_aseg = aseg_3d %>%
  unnest(cols = ggseg_3d) %>%
  select(label) %>%
  filter(grepl("Right-Hippocampus|Right-Amygdala", label)) %>%
  mutate(p = seq(1, nrow(.)))

ggseg3d(.data = ah_aseg, atlas = aseg_3d,
        colour = "p", text = "p",
        na.alpha= .5) %>%
  add_glassbrain(opacity = .1) %>%
  remove_axes()
