### This script generates a figure of the brain where regions that mediate
### the association between violence and internalizing symptoms are highlighted
###
### Ellyn Butler
### December 14, 2021

from nilearn import plotting

dmn_coords = [(0, -52, 18), (-46, -68, 32), (46, -68, 32), (1, 50, -5)]

view = plotting.view_markers(
    dmn_coords, ['red', 'cyan', 'magenta', 'orange'], marker_size=10)

view.open_in_browser() 
