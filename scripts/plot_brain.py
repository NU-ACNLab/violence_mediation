### This script generates a figure of the brain where regions that mediate
### the association between violence and internalizing symptoms are highlighted
###
### Ellyn Butler
### August 22, 2022 - November 18, 2022

from nilearn import plotting

# Amygdalae (244, 245):
#19.51_-1.85_-23.11
#-20.3 -2.27 -22.21

###### IE.M2

#    matnames  regnames
#2       M2.2   region2 -24.66_-97.84_-12.33    17Networks_LH_VisCent_ExStr_2 (primary visual cortex)
#12     M2.12  region14 -46.68_-50.91_-20.91    17Networks_LH_DorsAttnA_SPL_1 (dorsal attention network - inferior temporal gyrus)
#234   M2.234 region237 24.73_-11.25_-22.68     hippocampus
#256   M2.256 region261 -18.84_-5.08_-3.06      pallidum
#271   M2.271 region281 -5.72_-50.80_-40.84     medial inferior cerebellum

med_coords = [(19.51, -1.85, -23.11),
              (-20.3, -2.27, -22.21),
              (-24.66, -97.84, -12.33),
              (-46.68, -50.91, -20.91),
              (24.73, -11.25, -22.68),
              (-18.84, -5.08, -3.06),
              (-5.72, -50.80, -40.84)]

view = plotting.view_markers(
    med_coords, ['red', 'red', 'black', 'black', 'black', 'black', 'black',
        'black', 'black', 'black', 'black'], marker_size=12)

view.open_in_browser()

# Just hippocampus
med_coords = [(19.51, -1.85, -23.11),
              (-20.3, -2.27, -22.21),
              (24.73, -11.25, -22.68)]

view = plotting.view_markers(
    med_coords, ['red', 'red', 'black'], marker_size=12)

view.open_in_browser()

# Just pallidum
med_coords = [(19.51, -1.85, -23.11),
              (-20.3, -2.27, -22.21),
              (-18.84, -5.08, -3.06)]

view = plotting.view_markers(
    med_coords, ['red', 'red', 'black'], marker_size=12)

view.open_in_browser()

###### IE.M2 Full match

#    matnames  regnames
#2       M2.2   region2 -24.66_-97.84_-12.33    17Networks_LH_VisCent_ExStr_2 (primary visual cortex)
#12     M2.12  region14 -46.68_-50.91_-20.91    17Networks_LH_DorsAttnA_SPL_1 (dorsal attention network - inferior temporal gyrus)
#116   M2.116 region118 8.80_54.23_3.45         medial prefrontal cortex
#130   M2.130 region132 51.90_6.81_-29.61       anterior inferior temporal gyrus
#188   M2.188 region190 33.60_54.22_-12.95      ventrolateral prefrontal cortex
#235   M2.235 region237 24.73_-11.25_-22.68     hippocampus
#248   M2.248 region252 28.56_-7.53_7.66        putamen
#257   M2.257 region261 -18.84_-5.08_-3.06      pallidum

full_coords = [(19.51, -1.85, -23.11),
              (-20.3, -2.27, -22.21),
              (-24.66, -97.84, -12.33),
              (-46.68, -50.91, -20.91),
              (8.80, 54.23, 3.45),
              (51.90, 6.81, -29.61),
              (33.60, 54.22, -12.95),
              (24.73, -11.25, -22.68),
              (28.56, -7.53, 7.66),
              (-18.84, -5.08, -3.06)]

view = plotting.view_markers(
    full_coords, ['red', 'red', 'black', 'black', 'slateblue', 'slateblue',
    'slateblue', 'slateblue', 'slateblue', 'slateblue'], marker_size=12)

view.open_in_browser()



# Old
#44     M2.44  region46 -49.47_-11.06_34.95
#67     M2.67  region69 -52.92_-21.83_22.97
#126   M2.126 region129 49.26_35.47_-12.20
#136   M2.136 region139 1.75_-24.25_30.36
#139   M2.139 region142 7.98_-91.08_-7.10
#171   M2.171 region174 42.52_-78.17_-11.78
