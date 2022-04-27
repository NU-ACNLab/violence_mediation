### This script generates a figure of the brain where regions that mediate
### the association between violence and internalizing symptoms are highlighted
###
### Ellyn Butler
### December 14, 2021

from nilearn import plotting

med_coords = [(-42, -57, 41)]

view = plotting.view_markers(
    med_coords, ['red'], marker_size=10)

view.open_in_browser()
