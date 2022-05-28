### This script generates a figure of the brain where regions that mediate
### the association between violence and internalizing symptoms are highlighted
###
### Ellyn Butler
### December 14, 2021 - May 27, 2022

from nilearn import plotting

# Amygdalae (244, 245):
#19.51_-1.85_-23.11
#-20.3 -2.27 -22.21

###### IE.M2

# 14: 0.05_-14.53_46.74
# 29: 3.45_-17.44_58.45
# 32: 10.09_-17.10_74.14
# 214: 51.52_-32.52_7.55
# 296: 7.50_-72.00_-39.00

med_coords = [(19.51, -1.85, -23.11),
              (-20.3, -2.27, -22.21),
              (0.05, -14.53, 46.74),
              (3.45, -17.44, 58.45),
              (10.09, -17.10, 74.14),
              (51.52, -32.52, 7.55),
              (7.50, -72.00, -39.00)]

view = plotting.view_markers(
    med_coords, ['red', 'red', 'black', 'black', 'black', 'black', 'black'], marker_size=12)

view.open_in_browser()


###### IE.M1M2

# 14: 0.05_-14.53_46.74
# 63: 42.05_-0.39_47.10
# 188: 31.83_14.37_55.98

med_coords = [(19.51, -1.85, -23.11),
              (-20.3, -2.27, -22.21),
              (0.05, -14.53, 46.74),
              (42.05, -0.39, 47.10),
              (31.83, 14.37, 55.98)]

view = plotting.view_markers(
    med_coords, ['red', 'red', 'black', 'black', 'black'], marker_size=12)

view.open_in_browser()
