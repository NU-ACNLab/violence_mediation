### This script generates a figure of the brain where regions that mediate
### the association between violence and internalizing symptoms are highlighted
###
### Ellyn Butler
### August 22, 2022 - November 9, 2022

from nilearn import plotting

# Amygdalae (244, 245):
#19.51_-1.85_-23.11
#-20.3 -2.27 -22.21

###### IE.M2

#    matnames  regnames
#2       M2.2   region2 -24.66_-97.84_-12.33    17Networks_LH_VisCent_ExStr_2 (primary visual cortex)
#12     M2.12  region14 -46.68_-50.91_-20.91    17Networks_LH_DorsAttnA_SPL_1 (dorsal attention network - temporal lobe)
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



# Old
#44     M2.44  region46 -49.47_-11.06_34.95
#67     M2.67  region69 -52.92_-21.83_22.97
#126   M2.126 region129 49.26_35.47_-12.20
#136   M2.136 region139 1.75_-24.25_30.36
#139   M2.139 region142 7.98_-91.08_-7.10
#171   M2.171 region174 42.52_-78.17_-11.78
