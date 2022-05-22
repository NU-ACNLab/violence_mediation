### This script generates a figure of the brain where regions that mediate
### the association between violence and internalizing symptoms are highlighted
###
### Ellyn Butler
### December 14, 2021 - May 20, 2022

from nilearn import plotting

med_coords = [(-56.16, -44.76, -24.23),
              (10.09, -17.10, 74.14),
              (36.04, -9.44, 13.95),
              (51.52, -32.52, 7.55),
              (-33.00, -51.00, -50.00)]

view = plotting.view_markers(
    med_coords, ['red', 'red', 'red', 'red', 'red'], marker_size=10)

view.open_in_browser()
