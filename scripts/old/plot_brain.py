### This script generates a figure of the brain where regions that mediate
### the association between violence and internalizing symptoms are highlighted
###
### Ellyn Butler
### August 22, 2022 - August 23, 2022

from nilearn import plotting

# Amygdalae (244, 245):
#19.51_-1.85_-23.11
#-20.3 -2.27 -22.21

###### IE.M2

#M2.1, M2.32, M2.35, M2.294

# 1: -56.16_-44.76_-24.23
# 32: 10.09_-17.10_74.14
# 35: 22.45_-42.29_68.99
# 294: -33.00_-51.00_-50.00

med_coords = [(19.51, -1.85, -23.11),
              (-20.3, -2.27, -22.21),
              (-56.16, -44.76, -24.23),
              (10.09, -17.10, 74.14),
              (22.45, -42.29, 68.99),
              (-33.00, -51.00, -50.00)]

view = plotting.view_markers(
    med_coords, ['red', 'red', 'black', 'black', 'black', 'black'], marker_size=12)

view.open_in_browser()


###### IE.M2 Regress

#M2.32, M2.35, M2.143, M2.164

# 32: 10.09_-17.10_74.14
# 35: 22.45_-42.29_68.99
# 143: 17.27_-91.09_-13.64
# 164: 19.81_-65.56_1.72

reg_coords = [(19.51, -1.85, -23.11),
              (-20.3, -2.27, -22.21),
              (10.09, -17.10, 74.14),
              (22.45, -42.29, 68.99),
              (17.27, -91.09, -13.64),
              (19.81, -65.56, 1.72)]

view = plotting.view_markers(
    reg_coords, ['red', 'red', 'black', 'black', 'black', 'black'], marker_size=12)

view.open_in_browser()
